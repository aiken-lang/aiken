# Contributing

## What & How Can You Contribute?

1. **Feedback**

   Contributions in the form of feedback and issues are very much welcome. Whether it may be a suggestion, a bug report, or maybe some questions that you have. It helps in improving Aiken in over time and these are the best kind of contributions to start with.

   Do not hesitate to add _thumbs up :+1:_ on open issues you support to show your interest.

2. **Documentation**

   Any updates, typo fixes, or expansion of the documention is more than welcome. At the moment,
   we are using [mdBook](https://github.com/rust-lang/mdBook) to run https://txpipe.github.io/aiken.
   That can be considered user level documentation but that is not strict. There is also doc comments in the Rust code, which can be considered strictly developer level documentation.

3. **Code**

   **Getting started**

   - `git clone git@github.com:txpipe/aiken.git`
   - `cd aiken`
   - `cargo test`
   - `cargo run -- help`

   ***

   If everything runs fine without any errors you're good to go. If you do run into any errors please come chat with us on [Discord (#aiken)](https://discord.gg/Vc3x8N9nz2)

   Coding standards are enforced using CI. Make sure to configure your editor to pick them up. As an example, if you're using VSCode, please use the [rust-analyzer](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer) extension. Then turn on format on save and map `check` to `clippy` in the VSCode settings. Any clippy or formatting errors will cause CI to fail.

   Pull requests are welcome, but we do recommend you open an issue to bring any idea to discussion first! Especially if the pull request will end up very large, any significant changes should be discussed up front with the maintainers. This avoids awkward situations where someone puts in a bunch of work to ultimately have the pull request closed due to a potential variety of unforeseen reasons.

   CI runs the following commands. To save time, it's good to run these on your local machine before pushing to origin.

   ```
   cargo build --workspace
   cargo test --workspace
   cargo fmt --all -- --check
   cargo clippy --all-targets --all-features -- -D warnings
   ```

   **Changelog**

   Please add an entry into [CHANGLOG.md](./CHANGELOG.md) when submitting changes. New entries should go into the `[next] YYYY-MM-DD` section. This let's us keep track of unreleased changes
   for use in release notes.

   Once a release is ready `[next] YYYY-MM-DD` gets replaced with a verion number and a the release date `[0.0.0] 2009-01-03`. Usually the maintainers will handle the section renaming along with creating a new _empty_ `[next] YYYY-MM-DD` section at the top of the changelog.

   Each release section will be further broken down into three sections named `Added`, `Changed`, and `Removed`. Please put the associated crate's name in **bold** followed by a `:` as a prefix to the new entry.

   ```md
   ## [next] - YYYY-MM-DD

   ### Added

   - **crate**: some new thing

   ### Changed

   - **crate**: fixed or updated something

   ### Removed

   - **crate**: something is gone now
   ```

4. **Donation**

   Want to give some financial support? Have a look at the ways to sponsor below for more details.

   - [rvcas](https://github.com/sponsors/rvcas/)
   - [microproofs](https://github.com/sponsors/microproofs/)

   Want to support with crypto?

   - Our Ada address is `addr1q83nlzwu4zjeu927m8t24xa68upgmwgt5w29ww5ka695hc5rez2r4q7gcvj7z0ma6d88w3j220szsqk05sn43ghcsn4szvuklq`
   - Our Ada handle is `$aiken_lang`

## About Issues

### :bug: How To Report A Bug

Open a [Bug Issue](https://github.com/txpipe/aiken/issues/new?template=bug.md).

### :bulb: How To Propose An Idea

Feel free to bring any idea as a [discussion [category: idea]](https://github.com/txpipe/aiken/discussions/new?category=ideas). Make sure to highlight your use case so we can understand the design space and agree on a solution.

> **Note** Ideally, follow this simple template:
>
> - What is your idea? Describe it in simple words. Provide a use case.
> - Why is it a good idea?
> - What is the current alternative and why is it not good enough?

### :question: How To Ask a Question

Open a [Q&A Discussion](https://github.com/txpipe/aiken/discussions/new?category=q-a).

> Make sure to mark your question as _Answered_ once resolved!

## Need Help Getting Started?

Should you be unsure about where to start, feel free to come and chat on [Discord (#aiken)](https://discord.gg/Vc3x8N9nz2).
