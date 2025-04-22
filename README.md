# Aegis: A language that enforces information flow with types

Aegis is a programming language, with a compiler implemented in OCaml, that enforces fine‑grained information flow security. Aegis integrates a security‑augmented type system, based on the high‑watermark policy and novel operational semantics, to guarantee that both explicit and implicit data flows follow your chosen confidentiality and integrity constraints. Whether you’re building cryptographic protocols, secure services, or any application where preventing data leakage is critical, Aegis provides compile‑time guarantees that sensitive information will never “flow down” to less‑secure contexts.

## Features

- **Support for basic data types**: Aegis supports basic data types, integers and booleans, as well as regular operations (including binary and unary operations) on these types.
- **Object-oriented features**: Aegis supports basic object‑oriented features, including classes and objects, but currently does not support inheritance or overloading.
- **Security-augmented type system**: Aegis uses a security‑augmented type system to enforce high‑watermark policies, ensuring that sensitive data is never leaked to less secure contexts.
- **Exception handling**: Aegis includes try catch finally constructs for exception handling, allowing you to manage errors and exceptional events in a controlled manner.
- **Resumable exceptions**: Aegis introduces advanced error handling through resumable exceptions, using algebraic effects and continuations. This allows programs to recover from and resume after any exceptions have been raised without breaking security guarantees.

## Syntax

Aegis follows a syntax that is a fusion of OCaml and Java. Each variable is annotated with its type and security level, as shown in example below:

```aegis
let x:(int, Low) = 5 in {
    let y:(int, Low) = 10 in {
        x + y
    }
}
```

## Getting Started

To get started with Aegis, you will need to have the following dependencies installed:

- OCaml 5.3.0 or later
- Dune 3.18.0 or later
- OPAM 2.3.0 or later

## Installation Steps

### 1. Clone the Repository

Clone the Aegis repository from GitHub:

```bash
https://github.com/komalrathi/aegis
cd aegis
```

### 2. Install Dependencies

Install the required dependencies using OPAM. You can create a new OPAM switch for Aegis to avoid conflicts with other OCaml projects.
Aegis requires OCaml 5.3.0 or later.

```bash
opam switch create <switch-name> <ocaml-version>
eval $(opam env)
opam install . --deps-only
```

### 3. Build Aegis

Build the Aegis compiler using Dune:

```bash
dune build
```

### 4. Run Aegis

You can run the Aegis compiler using the following command:

```bash
dune exec src/main.exe [path to your Aegis file]
```

### 5. Explore Examples

Explore the `examples/` directory for sample programs and benchmarks comparing Aegis’s security enforcement overhead against OCaml.

### 6. Contributing

We welcome contributions, issues, and discussion on the GitHub issue tracker as we continue to evolve Aegis’s performance, expressiveness, and proof‑carrying security guarantees.

### 7. License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Running Tests and Benchmarks

To run the tests and benchmarks, you can use the following command:

```bash
dune runtest
```

This will execute all the tests and benchmarks defined in the `test/` directory.
You can also run specific tests or benchmarks by specifying the test file:

```bash
dune runtest test/[path to your test file]
```

This will execute only the specified test file.

To benchmark Aegis programs against OCaml, run the benchmark suite as follows:

```bash
dune exec benchmarks/benchmark_runner.exe
```

This will execute the benchmark suite and display the results.

## Troubleshooting

If you encounter any issues during installation or setup, please consult the Issues page on GitHub or open a new issue if your problem has not been addressed.
