use std::env;

use aiken_project::{config::Config, paths};
use lsp_server::Connection;

mod cast;
pub mod error;
mod line_numbers;
pub mod server;
mod utils;

use error::Error;

use crate::server::Server;

pub fn start() -> Result<(), Error> {
    tracing::info!("Aiken language server starting");

    let root = env::current_dir()?;

    let config = if paths::project_config().exists() {
        tracing::info!("Aiken project detected");

        Some(Config::load(&root).expect("failed to load aiken.toml"))
    } else {
        tracing::info!("Aiken project config not found");

        None
    };

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&capabilities())?;

    let initialization_params = connection.initialize(server_capabilities)?;
    let initialize_params = serde_json::from_value(initialization_params)?;

    let mut server = Server::new(initialize_params, config, root);

    server.listen(connection)?;

    io_threads.join()?;

    tracing::info!("Aiken language server shutting down");

    Ok(())
}

fn capabilities() -> lsp_types::ServerCapabilities {
    lsp_types::ServerCapabilities {
        completion_provider: Some(lsp_types::CompletionOptions {
            resolve_provider: None,
            trigger_characters: Some(vec![".".into(), " ".into()]),
            all_commit_characters: None,
            work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }),
        document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: None,
                change: Some(lsp_types::TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
                    lsp_types::SaveOptions {
                        include_text: Some(false),
                    },
                )),
            },
        )),
        ..Default::default()
    }
}
