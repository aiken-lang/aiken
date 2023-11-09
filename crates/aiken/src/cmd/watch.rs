use notify::{RecursiveMode, Watcher, Event, event::EventAttributes};
use std::{path::Path, error::Error, time::SystemTime, collections::VecDeque, sync::{Mutex, Arc}};
#[derive(clap::Args)]
/// Type-check an Aiken project
pub struct Args {
    /// Clear the screen between each run
    #[clap(long)]
    clear: bool,
}

pub fn exec(Args { clear }: Args) -> miette::Result<()> {
    let project = Path::new("../sundae-contracts/aiken").to_path_buf().canonicalize().expect("");
    let build = project.join("build").canonicalize().expect("");
    let lock = project.join("aiken.lock");
    let queue = Arc::new(Mutex::new(VecDeque::new()));
    queue.lock().unwrap().push_back(Event { kind: notify::EventKind::Any, paths: vec![], attrs: EventAttributes::new() });

    let queue_write = queue.clone();
    let mut watcher = notify::recommended_watcher(move |res: notify::Result<Event>| {
        match res {
            Ok(event) => {
                match event.kind {
                    notify::EventKind::Create(_) |
                    notify::EventKind::Modify(_) |
                    notify::EventKind::Remove(_) => {
                        let mut queue = queue_write.lock().expect("lock queue");
                        queue.push_back(event.clone());
                        drop(queue);
                    }
                    _ => {}
                }
            },
            Err(e) => {
                println!("watch error: {:?}", e)
            },
        };
    }).expect("watcher");
    let _ = watcher.watch(Path::new("../sundae-contracts/aiken"), RecursiveMode::Recursive);
    let queue_read = queue.clone();
    let mut last_evt = SystemTime::UNIX_EPOCH;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(300));

        let mut queue = queue_read.lock().expect("lock queue");
        let mut latest = None;
        while let Some(evt) = queue.pop_back() {
            if evt.paths.iter().any(|p| {
                let p = p.canonicalize().expect("");
                p.starts_with(&build) || p.starts_with(&lock)
             }) {
                continue;
            }
            latest = Some(evt);
        }
        drop(queue);
        if let Some(evt) = latest {   
            if clear {
                println!("{esc}c", esc = 27 as char);
            }
            let _ = crate::with_project_ok(Some(project.clone()), false, |p| {
                p.check(
                    false,
                    None,
                    true,
                    false,
                    false.into(),
                )
            });
        }
    }
}
