# CSMT

[![CI](https://github.com/paolino/csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/csmt/actions/workflows/CI.yaml)

This package provides:
- A Haskell library implementing a Compact Sparse Merkle Tree (CSMT) data structure with support for persistent storage backends. It offers efficient insertion, deletion, and proof generation functionalities, making it suitable for applications requiring verifiable data structures.
- A CLI tool for interacting with the CSMT, allowing users to perform operations such as adding and removing elements, generating proofs, and verifying membership within the tree.
- An HTTP service that exposes the CSMT functionalities via a RESTful API, enabling remote interaction with the tree for various applications.

This package does not provide a storage for the preimage of the hashes; it is designed to be storage-agnostic, allowing users to integrate their preferred persistent storage solutions. It will compute the hashes of key and values and store those in the tree, but it is up to the user to manage the actual key-value pairs externally.

## Status

- Library
  - [x] Insertion
  - [x] Deletion
  - [x] Proof generation
  - [x] Proof verification
  - [x] Persistent storage backend support
  - [x] Comprehensive tests
  - [x] Insertion benchmarks
  - [ ] Deletion benchmarks
  - [ ] Proof generation benchmarks
  - [ ] Proof verification benchmarks
  - [ ] Production grade tests
- CLI tool
  - [x] Add elements
  - [x] Remove elements
  - [x] Generate proofs
  - [x] Verify membership
- HTTP service
  - [ ] RESTful API for CSMT operations
  - [ ] Documentation of API endpoints

## Installation

Currently there is no packaging available. You can clone the repository and build the project using [Cabal](https://www.haskell.org/cabal/):

```bash
git clone https://github.com/paolino/csmt.git
cd csmt
cabal install
```
This will install the library, CLI tool.

Nix support is also available. You can build the project using Nix:

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:paolino/csmt --refresh
```

will drop you in a shell where csmt is built and available.
