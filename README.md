# OCaml Expression Parser

## Project Overview

This project implements a parser for a simple expression language using OCaml. It demonstrates how to build a recursive descent parser for a minimal functional language, showcasing various aspects of language processing and compiler design.

## Key Features

- Parsing of common language constructs:
  - `let` and `let rec` bindings
  - Conditional expressions (`if ... then ... else`)
  - Function definitions and applications
  - Binary and logical operations
  - String concatenation
  - Record expressions and field selection

- Handling of top-level directives (mutop)
- Token definition and management
- Abstract Syntax Tree (AST) construction
- Basic environment management for variable lookup and updates

## Technical Highlights

- **Recursive Descent Parsing**: Implements a top-down parsing approach, with each grammar rule represented by a corresponding function.
- **Token Management**: Includes functions for peeking at and consuming tokens, ensuring proper syntax adherence.
- **AST Generation**: Builds an Abstract Syntax Tree representation of the parsed expressions, facilitating further processing or evaluation.
- **Extensible Design**: The parser's modular structure allows for easy addition of new language features or modifications to existing ones.

## Project Structure

The project is organized into several key OCaml files:

- `types.ml`: Defines token and AST node types
- `utils.ml`: Contains utility functions for token handling and string conversions
- `parser.ml`: Implements the core parsing logic
- `lexer.ml` (optional): For custom lexical analysis, if implemented
