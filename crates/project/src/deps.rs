use std::path::Path;

use crate::{
    error::Error,
    paths,
    telemetry::{Event, EventListener},
};

use self::manifest::Manifest;

pub mod manifest;

pub enum UseManifest {
    Yes,
    No,
}

pub fn download<T>(
    event_listener: &T,
    new_package: Option<(Vec<String>, bool)>,
    use_manifest: UseManifest,
    root_path: &Path,
) -> Result<Manifest, Error>
where
    T: EventListener,
{
    let build_path = root_path.join(paths::build());

    let mut build_lock = fslock::LockFile::open(&build_path).expect("Build Lock Creation");

    if !build_lock
        .try_lock_with_pid()
        .expect("Trying build locking")
    {
        event_listener.handle_event(Event::WaitingForBuildDirLock);

        build_lock.lock_with_pid().expect("Build locking")
    }

    todo!()
}
