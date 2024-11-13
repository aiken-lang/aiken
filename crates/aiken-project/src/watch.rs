use crate::{telemetry::EventTarget, Project};
use miette::{Diagnostic, IntoDiagnostic};
use notify::{Event, RecursiveMode, Watcher};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{
    collections::VecDeque,
    env,
    ffi::OsStr,
    fmt::{self, Display},
    path::Path,
    sync::{Arc, Mutex},
};

#[derive(Debug, Diagnostic, thiserror::Error)]
enum ExitFailure {
    #[error("")]
    ExitFailure,
}

impl ExitFailure {
    fn into_report() -> miette::Report {
        ExitFailure::ExitFailure.into()
    }
}

struct Summary {
    check_count: Option<usize>,
    warning_count: usize,
    error_count: usize,
}

impl Display for Summary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!(
            "      {} {}{}{} {}, {} {}",
            "Summary"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
            if let Some(c) = self.check_count {
                format!("{} ", c)
            } else {
                "".to_string()
            },
            match self.check_count {
                Some(1) => "check, ",
                Some(_) => "checks, ",
                None => "",
            }
            .if_supports_color(Stderr, |s| s.green())
            .if_supports_color(Stderr, |s| s.bold()),
            self.error_count,
            if self.error_count == 1 {
                "error"
            } else {
                "errors"
            }
            .if_supports_color(Stderr, |s| s.red())
            .if_supports_color(Stderr, |s| s.bold()),
            self.warning_count,
            if self.warning_count == 1 {
                "warning"
            } else {
                "warnings"
            }
            .if_supports_color(Stderr, |s| s.yellow())
            .if_supports_color(Stderr, |s| s.bold()),
        ))
    }
}

/// A default filter for file events that catches the most relevant "source" changes
pub fn default_filter(evt: &Event) -> bool {
    // Only watch for changes to .ak and aiken.toml files, and ignore the build directory
    let source_file = evt
        .paths
        .iter()
        .any(|p| p.extension() == Some(OsStr::new("ak")) || p.ends_with("aiken.toml"));
    let build_dir = evt
        .paths
        .iter()
        .all(|p| p.ancestors().any(|a| a.ends_with("build")));
    match evt.kind {
        notify::EventKind::Any => true,
        notify::EventKind::Create(_)
        | notify::EventKind::Modify(_)
        | notify::EventKind::Remove(_) => source_file && !build_dir,
        _ => false,
    }
}

pub fn with_project<A>(
    directory: Option<&Path>,
    deny: bool,
    json: bool,
    mut action: A,
) -> miette::Result<()>
where
    A: FnMut(&mut Project<EventTarget>) -> Result<(), Vec<crate::error::Error>>,
{
    let project_path = if let Some(d) = directory {
        d.to_path_buf()
    } else if std::env::consts::OS == "windows" {
        env::current_dir().into_diagnostic()?
    } else {
        let mut current_dir = std::path::PathBuf::new();
        current_dir.push(".");
        current_dir
    };

    let mut project = match Project::new(project_path, EventTarget::default()) {
        Ok(p) => Ok(p),
        Err(e) => {
            e.report();
            Err(ExitFailure::into_report())
        }
    }?;

    let build_result = action(&mut project);

    let warnings = project.warnings();

    let warning_count = warnings.len();

    if !json {
        for warning in &warnings {
            warning.report()
        }

        if let Err(errs) = build_result {
            for err in &errs {
                err.report()
            }

            eprintln!(
                "{}",
                Summary {
                    check_count: project.checks_count,
                    warning_count,
                    error_count: errs.len(),
                }
            );

            return Err(ExitFailure::into_report());
        }

        if project.checks_count.unwrap_or_default() + warning_count > 0 {
            eprintln!(
                "{}",
                Summary {
                    check_count: project.checks_count,
                    error_count: 0,
                    warning_count
                }
            );
        }
    }

    if warning_count > 0 && deny {
        Err(ExitFailure::into_report())
    } else {
        Ok(())
    }
}

/// Run a function each time a file in the project changes
///
/// ```text
/// // Note: doctest disabled, because aiken_project doesn't have an implementation of EventListener I can use
/// use aiken_project::watch::{watch_project, default_filter};
/// use aiken_project::{Project};
/// watch_project(None, default_filter, 500, |project| {
///   println!("Project changed!");
///   Ok(())
/// });
/// ```
pub fn watch_project<F, A>(
    directory: Option<&Path>,
    filter: F,
    debounce: u32,
    mut action: A,
) -> miette::Result<()>
where
    F: Fn(&Event) -> bool,
    A: FnMut(&mut Project<EventTarget>) -> Result<(), Vec<crate::error::Error>>,
{
    let project_path = directory
        .map(|p| p.to_path_buf())
        .unwrap_or(env::current_dir().into_diagnostic()?);

    // Set up a queue for events, primarily so we can debounce on related events
    let queue = Arc::new(Mutex::new(VecDeque::new()));

    // Run the action once, to start
    queue
        .lock()
        .expect("lock queue")
        .push_back(Event::default());

    // Spawn a file-watcher that will put each change event on the queue
    let queue_write = queue.clone();
    let mut watcher = notify::recommended_watcher(move |res: notify::Result<Event>| {
        match res {
            Ok(event) => queue_write
                .lock()
                .expect("lock queue")
                .push_back(event.clone()),
            Err(e) => {
                // TODO: miette diagnostic?
                println!(
                    "Encountered an error while monitoring for file changes: {:?}",
                    e
                )
            }
        };
    })
    .into_diagnostic()?;

    // Start watching for any changes in the project directory
    let _ = watcher.watch(project_path.as_path(), RecursiveMode::Recursive);

    // And then start reading from the queue
    let queue_read = queue.clone();
    loop {
        // We sleep for the debounce interval, because notify will dump 12 related events into the queue all at once
        std::thread::sleep(std::time::Duration::from_millis(debounce.into()));

        // Grab the lock, and pop all events except the last one off the queue
        let mut queue = queue_read.lock().expect("lock queue");
        let mut latest = None;
        // debounce the events, and ignore build/lock changes, because they come in in large batches
        while let Some(evt) = queue.pop_back() {
            // check if this event is meaningful to the caller
            if !filter(&evt) {
                continue;
            }
            latest = Some(evt);
        }
        // release the lock here, in case other events come in
        drop(queue);

        // If we have an event that survived the filter, then we can construct the project and invoke the action
        if latest.is_some() {
            print!("{esc}c", esc = 27 as char);
            eprint!("{esc}c", esc = 27 as char);
            eprintln!(
                "{} ...",
                "     Watching"
                    .if_supports_color(Stderr, |s| s.bold())
                    .if_supports_color(Stderr, |s| s.purple()),
            );
            with_project(directory, false, false, &mut action).unwrap_or(())
        }
    }
}
