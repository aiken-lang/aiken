# Hello, World!

An example of an Hello, World! contract using Aiken and [Mesh](https://meshjs.dev/).

See the [full tutorial on aiken-lang.org](https://aiken-lang.org/example--hello-world/end-to-end/mesh).

## Setup

### Building

```
aiken build
```

### Initializing workspace

```
npm init -y
npm install @meshsdk/core tsx
```

### Setup environment variables

```
export BLOCKFROST_PROJECT_ID=preprod...
```

## Usage

### Generating Credentials

```
npx tsx generate-credentials.ts
```

### Locking Funds

> **Warning** Require `BLOCKFROST_API_KEY` environment variable to be set.

```
npx tsx lock.ts
```

Successful transaction hash: `bfa4818940831dff961a2f097e1aef9bf626de744fd96abfd2be7d6b61afb270` (preprod)

### Unlocking Funds

> **Warning** Require `BLOCKFROST_API_KEY` environment variable to be set.

```
npx tsx unlock.ts TRANSACTION_ID_FROM_LOCK
```

Successful transaction hash: `1f8f3abac70c3a71c6aa943b4b9a6ac002e63a69225eb59305c3cd663cda3dd7` (preprod)
