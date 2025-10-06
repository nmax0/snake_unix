Snake (OCaml, Unix)

This project builds a single executable from `snake.ml` and `deque.ml` using Dune.

Project layout
- `dune-project`: declares Dune language and project name
- `dune`: defines the `snake` executable, modules `snake` and `deque`, and links the `unix` library
- `snake.ml`: main program
- `deque.ml`: helper module

Prerequisites
- OCaml (>= 4.14 or 5.x recommended)
- Dune (>= 3.8)

Build
```bash
dune build
```

Run
```bash
# Run the built executable via dune
dune exec ./snake.exe
```

Clean
```bash
dune clean
```

Notes
- The `unix` library is already declared in `dune`; no extra setup needed if OCaml’s Unix is available in your compiler toolchain.
- If you see terminal/TTY related errors, ensure you’re running in a Unix-like terminal (Linux/macOS) and that your locale is set properly.

## Gameplay

When you run the game, a snake (initial length 3) is displayed in a 25×25 grid. Your goal is to maneuver the snake to eat the apple (`@`) without hitting the walls or yourself. Each time the snake eats an apple, it grows by one segment and a new apple spawns at a random empty location.

## Controls

- Arrow keys: move Up, Down, Left, Right
- Z / Q / S / D: (alternative controls) Z=Up, S=Down, Q=Left, D=Right
- L key (uppercase or lowercase): quit the game

## Terminal Requirements

- Ensure your terminal window is at least 25 columns by 25 rows.
- If the display is garbled, try resetting your terminal or increasing font size.


