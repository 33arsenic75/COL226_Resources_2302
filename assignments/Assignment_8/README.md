# ProCaml: Prolog Interpreter in OCaml

ProCaml is a Prolog interpreter implemented in OCaml. It allows you to define Prolog-like programs and query them to find solutions.

## Features

- Supports defining clauses, heads, bodies, atoms, symbols, terms, variables, substitutions, goals, and more.
- Implements unification, backtracking, and goal-solving algorithms.
- Handles arithmetic operations within terms.
- Provides a simple command-line interface for running queries.

## Usage

To load a prolog file by the name "file.pl" run the following command

```bash
make
./main file.pl
```

If the program is syntactically correct, it will show the following screen :-

![welcomescreen](welcomescreen.jpg)

After this you can enter your queries. To exit enter "exit" or "quit".
