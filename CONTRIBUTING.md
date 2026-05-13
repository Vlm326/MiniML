# Contributing to MiniML

First off, thanks for taking the time to contribute!

## How Can I Contribute?

### Reporting Bugs

- Open an [issue](https://github.com/Vlm326/MiniML/issues) with a clear description.
- Include steps to reproduce, expected behavior, and actual behavior.
- Attach example MiniML code that triggers the bug.

### Suggesting Features

- Open an [issue](https://github.com/Vlm326/MiniML/issues) describing the feature.
- Explain why the feature would be useful and how it might work.

### Pull Requests

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature/my-feature`.
3. Make your changes.
4. Ensure code is formatted: `dune fmt`.
5. Run tests: `dune runtest`.
6. Run coverage: `dune runtest --instrument-with bisect_ppx --force`.
7. Commit with a descriptive message following [Conventional Commits](https://www.conventionalcommits.org/).
8. Push and open a Pull Request.

## Code Style

- The project uses `ocamlformat` with the `conventional` profile (see `.ocamlformat`).
- All `.ml` and `.mli` files must include the Apache 2.0 license header.
- Keep changes focused — one pull request per feature or bugfix.

## Development Setup

```bash
# Install dependencies
opam install . --deps-only --with-test -y

# Build
dune build

# Run tests
dune runtest

# Format code
dune fmt
```

## License

By contributing, you agree that your contributions will be licensed under the Apache License 2.0.
