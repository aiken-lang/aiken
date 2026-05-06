use crate::{Project, config::WorkspaceConfig, telemetry::EventTarget};
use miette::{Diagnostic, IntoDiagnostic};
use notify::{Event, RecursiveMode, Watcher};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{
    collections::VecDeque,
    env,
    ffi::OsStr,
    fmt::{self, Display},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum ExitFailure {
    #[error("")]
    ExitFailure,
    #[error("{0}")]
    Message(String),
}

impl ExitFailure {
    pub fn into_report() -> miette::Report {
        ExitFailure::ExitFailure.into()
    }

    pub fn with_message(message: impl Into<String>) -> miette::Report {
        ExitFailure::Message(message.into()).into()
    }
}

fn render_diagnostic_message<E>(error: &E) -> String
where
    E: Diagnostic + ToString + ?Sized,
{
    let mut rendered = error.to_string();

    if let Some(help) = Diagnostic::help(error) {
        let help = help.to_string();
        if !help.trim().is_empty() {
            rendered.push_str("\n\n");
            rendered.push_str(&help);
        }
    }

    rendered
}

fn render_error(error: &crate::error::Error) -> String {
    match error {
        crate::error::Error::Parse { error, .. } => render_diagnostic_message(error.as_ref()),
        crate::error::Error::Type { error, .. } => render_diagnostic_message(error.as_ref()),
        _ => render_diagnostic_message(error),
    }
}

fn render_errors(errors: &[crate::error::Error]) -> String {
    errors
        .iter()
        .map(render_error)
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn render_warnings(warnings: &[crate::error::Warning]) -> String {
    warnings
        .iter()
        .map(crate::error::Warning::render)
        .collect::<Vec<_>>()
        .join("\n\n")
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
                format!("{c} ")
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

/// Mimic the way `with_project` wraps the `action`, except there is no project to load.
pub fn without_project<A>(mut action: A) -> miette::Result<()>
where
    A: FnMut() -> Result<(), Vec<crate::error::Error>>,
{
    let summary = |err_count| Summary {
        check_count: None,
        warning_count: 0,
        error_count: err_count,
    };
    match action() {
        Ok(_) => {
            eprintln!("{}", summary(0));
            Ok(())
        }
        Err(errs) => {
            for err in &errs {
                err.report();
            }
            eprintln!("{}", summary(errs.len()));
            Err(ExitFailure::into_report())
        }
    }
}

pub fn workspace_root(directory: Option<&Path>) -> miette::Result<PathBuf> {
    if let Some(d) = directory {
        Ok(d.to_path_buf())
    } else if std::env::consts::OS == "windows" {
        env::current_dir().into_diagnostic()
    } else {
        let mut current_dir = std::path::PathBuf::new();
        current_dir.push(".");
        Ok(current_dir)
    }
}

pub fn with_project<A>(
    directory: Option<&Path>,
    deny: bool,
    suppress_warnings: bool,
    show_summary: bool,
    action: A,
) -> miette::Result<()>
where
    A: FnMut(&mut Project<EventTarget>) -> Result<(), Vec<crate::error::Error>>,
{
    with_project_event_target(
        directory,
        deny,
        suppress_warnings,
        show_summary,
        EventTarget::default(),
        true,
        action,
    )
}

pub fn with_project_event_target<A>(
    directory: Option<&Path>,
    deny: bool,
    suppress_warnings: bool,
    show_summary: bool,
    event_target: EventTarget,
    report_diagnostics: bool,
    mut action: A,
) -> miette::Result<()>
where
    A: FnMut(&mut Project<EventTarget>) -> Result<(), Vec<crate::error::Error>>,
{
    let workspace_root = workspace_root(directory)?;

    let mut warnings = Vec::new();
    let mut errs: Vec<crate::error::Error> = Vec::new();
    let mut check_count = None;

    let mut is_terminal = true;

    if let Ok(workspace) = WorkspaceConfig::load(&workspace_root) {
        let res_projects = workspace
            .members
            .into_iter()
            .map(|member| {
                let event_target = event_target.clone();
                is_terminal = matches!(event_target, EventTarget::Terminal(_));
                Project::new(member, event_target)
            })
            .collect::<Result<Vec<Project<_>>, crate::error::Error>>();

        let projects = match res_projects {
            Ok(p) => Ok(p),
            Err(e) => {
                if report_diagnostics {
                    e.report();
                    Err(ExitFailure::into_report())
                } else {
                    Err(ExitFailure::with_message(render_error(&e)))
                }
            }
        }?;

        for mut project in projects {
            let build_result = action(&mut project);

            warnings.extend(project.warnings());

            let sum = check_count.unwrap_or(0) + project.checks_count.unwrap_or(0);
            check_count = if sum > 0 { Some(sum) } else { None };

            if let Err(e) = build_result {
                errs.extend(e);
            }
        }
    } else {
        let event_target = event_target.clone();
        is_terminal = matches!(event_target, EventTarget::Terminal(_));
        let mut project = match Project::new(workspace_root, event_target) {
            Ok(p) => Ok(p),
            Err(e) => {
                if report_diagnostics {
                    e.report();
                    Err(ExitFailure::into_report())
                } else {
                    Err(ExitFailure::with_message(render_error(&e)))
                }
            }
        }?;

        let build_result = action(&mut project);

        warnings.extend(project.warnings());

        let sum = check_count.unwrap_or(0) + project.checks_count.unwrap_or(0);
        check_count = if sum > 0 { Some(sum) } else { None };

        if let Err(e) = build_result {
            errs.extend(e);
        }
    }

    let warning_count = warnings.len();

    if report_diagnostics && is_terminal && !suppress_warnings {
        for warning in &warnings {
            eprintln!();
            warning.report()
        }
    }

    if !errs.is_empty() {
        if report_diagnostics {
            for err in &errs {
                err.report()
            }
        }

        if report_diagnostics && is_terminal {
            eprintln!(
                "{}",
                Summary {
                    check_count,
                    warning_count,
                    error_count: errs.len(),
                }
            );
        }

        return Err(if report_diagnostics {
            ExitFailure::into_report()
        } else {
            ExitFailure::with_message(render_errors(&errs))
        });
    }

    if report_diagnostics && is_terminal && show_summary {
        eprintln!(
            "{}",
            Summary {
                check_count,
                error_count: 0,
                warning_count
            }
        );
    }

    if warning_count > 0 && deny {
        Err(if report_diagnostics {
            ExitFailure::into_report()
        } else {
            ExitFailure::with_message(format!(
                "Warnings were denied by --deny.\n\n{}",
                render_warnings(&warnings)
            ))
        })
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
///
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
                println!("Encountered an error while monitoring for file changes: {e:?}")
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
            with_project(directory, false, false, false, &mut action).unwrap_or(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::with_project_event_target;
    use crate::telemetry::EventTarget;
    use std::fs;

    #[test]
    fn render_warnings_reuses_the_warning_renderer() {
        let warning = crate::error::Warning::NoValidators;
        assert_eq!(
            super::render_warnings(std::slice::from_ref(&warning)),
            warning.render()
        );
    }

    #[test]
    fn suppressed_project_load_errors_preserve_the_root_cause() {
        let tmp = tempfile::tempdir().expect("tempdir");
        fs::create_dir_all(tmp.path().join("lib")).expect("lib dir");
        fs::write(
            tmp.path().join("aiken.toml"),
            "name = \"aiken-lang/watch-error\"\nversion = [\n",
        )
        .expect("manifest");
        fs::write(
            tmp.path().join("lib/tests.ak"),
            "use missing/module\n\ntest broken() { True }\n",
        )
        .expect("source");

        let err = with_project_event_target(
            Some(tmp.path()),
            false,
            true,
            false,
            EventTarget::default(),
            false,
            |_project| Ok(()),
        )
        .expect_err("broken project should fail to load");

        assert!(
            err.to_string().contains("unclosed array"),
            "suppressed diagnostics must still preserve the underlying loader/compiler error: {err}"
        );
    }

    #[test]
    fn suppressed_parse_errors_preserve_the_parser_failure() {
        let tmp = tempfile::tempdir().expect("tempdir");
        fs::create_dir_all(tmp.path().join("lib")).expect("lib dir");
        fs::write(
            tmp.path().join("aiken.toml"),
            "name = \"aiken-lang/watch-parse\"\nversion = \"0.0.0\"\nplutusVersion = \"v3\"\n",
        )
        .expect("manifest");
        fs::write(tmp.path().join("lib/tests.ak"), "validator foo {\n").expect("source");

        let err = with_project_event_target(
            Some(tmp.path()),
            false,
            true,
            false,
            EventTarget::default(),
            false,
            |project| {
                project.build(
                    false,
                    aiken_lang::ast::Tracing::silent(),
                    project.blueprint_path(None),
                    crate::options::BlueprintExport::from(false),
                    None,
                )
            },
        )
        .expect_err("broken source should fail to load or build");

        let rendered = err.to_string().to_lowercase();
        assert!(
            rendered.contains("unexpected"),
            "suppressed parse diagnostics must preserve the parser failure: {err}",
        );
    }

    #[test]
    fn suppressed_type_errors_preserve_the_type_failure() {
        let tmp = tempfile::tempdir().expect("tempdir");
        fs::create_dir_all(tmp.path().join("lib")).expect("lib dir");
        fs::write(
            tmp.path().join("aiken.toml"),
            "name = \"aiken-lang/watch-type\"\nversion = \"0.0.0\"\nplutusVersion = \"v3\"\n",
        )
        .expect("manifest");
        fs::write(
            tmp.path().join("lib/tests.ak"),
            "test broken() {\n  missing\n}\n",
        )
        .expect("source");

        let err = with_project_event_target(
            Some(tmp.path()),
            false,
            true,
            false,
            EventTarget::default(),
            false,
            |project| {
                project.build(
                    false,
                    aiken_lang::ast::Tracing::silent(),
                    project.blueprint_path(None),
                    crate::options::BlueprintExport::from(false),
                    None,
                )
            },
        )
        .expect_err("type-invalid source should fail to build");

        let rendered = err.to_string().to_lowercase();
        assert!(
            rendered.contains("unknown variable"),
            "suppressed type diagnostics must preserve the type failure: {err}",
        );
    }

    #[test]
    fn suppressed_denied_warnings_preserve_the_warning_cause() {
        let tmp = tempfile::tempdir().expect("tempdir");
        fs::create_dir_all(tmp.path().join("lib")).expect("lib dir");
        fs::write(
            tmp.path().join("aiken.toml"),
            "name = \"aiken-lang/watch-warning\"\nversion = \"0.0.0\"\nplutusVersion = \"v3\"\n",
        )
        .expect("manifest");
        fs::write(
            tmp.path().join("lib/tests.ak"),
            "validator foo {\n  spend(c) {\n    True\n  }\n}\n",
        )
        .expect("source");

        let err = with_project_event_target(
            Some(tmp.path()),
            true,
            true,
            false,
            EventTarget::default(),
            false,
            |project| {
                project.build(
                    false,
                    aiken_lang::ast::Tracing::silent(),
                    project.blueprint_path(None),
                    crate::options::BlueprintExport::from(false),
                    None,
                )
            },
        )
        .expect_err("warning-only project should fail under --deny");

        let rendered = err.to_string().to_lowercase();
        assert!(
            rendered.contains("warnings were denied by --deny")
                && rendered.contains("validator")
                && rendered.contains("ignore"),
            "suppressed denied warnings must preserve the underlying warning cause: {err}",
        );
    }
}
