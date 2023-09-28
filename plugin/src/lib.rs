use extism_pdk::*;
use serde::Serialize;

pub mod blueprint;

use blueprint::Blueprint;

#[derive(Serialize)]
struct Output {
    pub code: String,
}

#[plugin_fn]
pub fn count_vowels(Json(blueprint): Json<Blueprint>) -> FnResult<Json<Output>> {
    let output = Output {
        code: blueprint.preamble.title,
    };

    Ok(Json(output))
}
