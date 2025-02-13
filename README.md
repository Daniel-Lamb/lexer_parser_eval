OCaml Expression Parser

This repository contains an OCaml-based parser for a simple expression language, featuring constructs such as:

let and let rec

if ... then ... else

Functions (fun x -> ...)

Binary operations (e.g., +, -, *, /, =, <>, <, > etc.)

Logical operations (and, or, not)

String concatenation

Record expressions (like {field = expression; ...})

Field selection expressions (expr.field)

It also includes code for handling top-level directives (mutop) such as definitions (def x = expr ;;) or evaluating expressions followed by ;;.

Table of Contents

Project Overview

Code Structure

Getting Started

Prerequisites

Build Instructions

Running the Parser

How the Parser Works

Extending the Parser

Contributing

License

Project Overview

The goal of this project is to demonstrate how to parse and partially evaluate a minimal functional language in OCaml. It showcases:

Token definitions (e.g., Tok_Int, Tok_Bool, Tok_String, Tok_ID, etc.)

Grammar rules for expressions (Expr) and top-level directives (Mutop).

Parsing logic that translates a list of tokens into an Abstract Syntax Tree (AST).

Environment management (extending, looking up, updating variables) when evaluating these expressions.

Code Structure

Below is a simplified overview of the files and directories in this repository:

.
├── README.md              # This file
├── types.ml               # Defines token types, AST node types, etc.
├── utils.ml               # Utility functions for printing or string conversions
├── parser.ml              # Main parser code (the core of this project)
├── lexer.ml (optional)    # An optional file if you use a custom lexer
├── dune                   # Dune configuration (if you use Dune as a build system)
└── ... (any additional files)

types.ml: Contains type definitions for tokens (token), AST nodes (e.g., Expr, Binop), and potential exceptions or labels.

utils.ml: Houses helper functions such as string_of_token to convert tokens to strings or other generic utility functions.

parser.ml: The core parser implementation. It includes:

match_token, match_many, lookahead functions to consume or peek tokens.

Recursive descent parsing functions (parse_Expr, parse_IfExpr, etc.) to handle different language constructs.

parse_mutop and related logic for handling top-level directives.

Getting Started

Prerequisites

OCaml (version 4.14 or later recommended).

Dune (optional but recommended for building; you can also use ocamlc or ocamlopt directly).

Make sure you have installed OCaml and Dune on your system. For example, on many Linux distributions:

sudo apt-get install ocaml
sudo apt-get install dune

Or on macOS with Homebrew:

brew install ocaml
brew install dune

Build Instructions

With Dune (Recommended)If you have a dune file at the root with appropriate configuration:



dune build


   This will compile your project, placing the resulting executables (if any) in the `_build` directory.

2. **With `ocamlc` (Manually)**  
   You can compile the files in sequence (assuming all `.ml` files are in the current directory):

   ```bash
ocamlc -c types.ml
ocamlc -c utils.ml
ocamlc -c parser.ml
ocamlc -o parser types.cmo utils.cmo parser.cmo

This produces an executable named parser. Adjust the sequence if your file names differ.

Running the Parser

If you have an executable built (for instance named parser):

./parser

How you actually provide input tokens may vary depending on your implementation. For example, you might:

Integrate a lexer that reads code from a file or standard input, tokenizes it, and passes it to the parser.

Or have a REPL-style loop where you type expressions and the parser evaluates them.

How the Parser Works

This code implements a recursive descent parser that:

Token PeekingIt peeks (lookahead) at the next token to decide which production rule to apply.

Token Matchingmatch_token and match_many functions ensure the next token(s) are what the rule expects, or raise an exception if they aren’t.

Grammar RulesEach function (e.g., parse_IfExpr, parse_LetExpr) corresponds to a grammar production. The parser calls these functions in a top-down manner to consume tokens and build an AST.

ResultOnce tokens are fully consumed for a construct, the parser returns a tuple of (remaining_tokens, ast_node) to the caller.

Extending the Parser

To add more features:

Update the Token TypeAdd any new tokens you need in types.ml.

Extend GrammarImplement or modify functions in parser.ml. For example, if you add new statements or operators, create new functions or enhance existing ones (like parse_NewFeatureExpr).

Adjust AST DefinitionsIf the new feature changes the structure of expressions or statements, add or modify constructors in the AST definitions (e.g., a new Expr variant).

TestingProvide test expressions in a suite or a simple script that feeds inputs into the parser. Verify the resulting AST or final evaluation matches your expectations.

Contributing

Contributions are welcome! Feel free to:

File an Issue if you find a bug or want to request a feature.

Open a Pull Request to fix errors, improve documentation, or add functionality.

When contributing, please ensure:

Your code passes all existing tests.

You include new tests for any new behavior.

Your code follows a similar style as the existing code for consistency.

License

This project is released under the MIT License, making it open for personal or commercial use, modification, and distribution. Feel free to adapt this parser to your own needs.

