# Hello, World!

An example of an Hello, World! contract using Aiken and [Lucid](https://github.com/spacebudz/lucid).

See the [full tutorial on aiken-lang.org](https://aiken-lang.org/getting-started/hello-world).

## Building

```
aiken build
```

## Generating Credentials

```
deno run --allow-net --allow-write generate-credentials.ts
```

## Locking Funds

> **Warning** Require `BLOCKFROST_API_KEY` environment variable to be set.

```
deno run --allow-net --allow-read --allow-env hello_world-lock.ts
```

## Unlocking Funds

> **Warning** Require `BLOCKFROST_API_KEY` environment variable to be set.

```
deno run --allow-net --allow-read --allow-env hello_world-unlock.ts TRANSACTION_ID_FROM_LOCK
```
