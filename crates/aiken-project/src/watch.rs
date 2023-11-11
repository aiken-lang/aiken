use miette::IntoDiagnostic;
use notify::{Event, RecursiveMode, Watcher};
use std::{
    collections::VecDeque,
    env,
    ffi::OsStr,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use crate::{telemetry, Project};

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

/// Run a function each time a file in the project changes
///
/// ```text
/// // Note: doctest disabled, because aiken_project doesn't have an implementation of EventListener I can use
/// use aiken_project::watch::{watch_project, default_filter};
/// use aiken_project::{Project};
/// watch_project(None, Terminal, default_filter, 500, |project| {
///   println!("Project changed!");
///   Ok(())
/// });
/// ```
pub fn watch_project<T, F, A>(
    directory: Option<PathBuf>,
    events: T,
    filter: F,
    debounce: u32,
    mut action: A,
) -> miette::Result<()>
where
    T: Copy + telemetry::EventListener,
    F: Fn(&Event) -> bool,
    A: FnMut(&mut Project<T>) -> Result<(), Vec<crate::error::Error>>,
{
    let project_path = directory.unwrap_or(env::current_dir().into_diagnostic()?);

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
            let mut project = match Project::new(project_path.clone(), events) {
                Ok(p) => p,
                Err(e) => {
                    // TODO: what should we actually do here?
                    e.report();
                    return Err(miette::Report::msg("??"));
                }
            };

            // Invoke the action, and abort on an error
            // TODO: what should we actually do with the error here?
            action(&mut project).or(Err(miette::Report::msg("??")))?;
        }
    }
}
