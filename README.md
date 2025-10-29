# OCaml Platformer Game

Project developed as part of the *Functional Programming in OCaml* course at Université Paris-Est Créteil.

---

## Description

This project implements a small **ASCII-based platformer game** built with a custom physics engine written in OCaml.
The goal is to simulate a player moving through multiple maps with gravity, collisions, enemies, and power-ups.

The game logic (player movement, enemies, projectiles, and transitions between maps) is defined in `project.ml`.
The core engine (`engine.ml` and `engine.mli`) handles the physics, rendering, and update loop.

The project is primarily intended to be **compiled and run online** via the official platform:

**[https://prog-reseau-m1.lacl.fr/projetfun2023/](https://prog-reseau-m1.lacl.fr/projetfun2023/)**

---

## Project Structure

```
OcamlPlatformer/
 ├── src/
 │    ├── engine.ml
 │    ├── engine.mli
 │    ├── project.ml
 │    └── Makefile
 ├── README.md
 └── .gitignore

```

— `engine.ml` / `engine.mli`: provided engine files 
— `project.ml`: player and game logic implementation
— `Makefile`: optional local compilation and cleanup commands

---

## Compilation and Execution

### Recommended: Online Platform

The game should be compiled and executed on the official platform:
**[https://prog-reseau-m1.lacl.fr/projetfun2023/](https://prog-reseau-m1.lacl.fr/projetfun2023/)**

Steps:

1. Open the website above.
2. Upload `project.ml`.
3. Click **Submit**.
4. The game will start in the integrated ASCII console with full keyboard input support.

This platform ensures a correct display and proper keyboard event handling, which may not work reliably in local terminals.

---

### Optional: Local Compilation

You can compile the game locally using the provided Makefile, though the display and controls may not render correctly on macOS or some terminals.

From the project root:
```bash
make -C src
./project
```

> **Note:** The Makefile is included for convenience and testing,
> but the **official website is the correct environment** for full functionality.

---

## Controls

| Key                     | Action                                         |
| ----------------------- | ---------------------------------------------- |
| **z**                   | Jump (or ascend with jetpack if active)        |
| **q**                   | Move left                                      |
| **d**                   | Move right                                     |
| **s**                   | Move down / drop                               |
| **p**                   | Shoot a projectile                             |
| **b**                   | Toggle "ball" form                             |
| **c**                   | Toggle "cannon" form (unlocked with power-up)  |
| **j**                   | Toggle "jetpack" form (unlocked with power-up) |
| **Shift** / **Control** | Boost movement or shot speed                   |
| **+ / -**               | Enter or exit a level                          |

Power-ups can be collected to unlock new forms and abilities:

* `p` symbol — health refill
* `c` symbol — cannon form
* `j` symbol — jetpack form

---

## Game Progression

* Multiple maps (`map0` to `map6`) with increasing difficulty
* Player can lose health when touching enemies
* Progress is made by reaching the exit gate (`-`)
* Some maps serve as end screens (`WINNER`, `GAME OVER`)

---

## Author

**Salah Zili**
Université Paris-Est Créteil
