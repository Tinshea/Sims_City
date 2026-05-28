# Sims City

> Academic project developed as part of my PAF (Programmation Avancée en style Fonctionnel) at STL Master

A city-builder simulation game inspired by SimCity, implemented entirely in Haskell with a graphical interface powered by SDL2. The project explores advanced functional programming concepts — pure functions, immutable state management, and type-driven design — applied to an interactive, real-time game loop.

## 🛠️ Tech Stack

- **Core Technologies**: Haskell, SDL2 (graphics & rendering), SDL2-TTF (font rendering), Linear (vector math)
- **Tools & Environment**: Stack (build tool & dependency manager), HSpec (unit testing), QuickCheck (property-based testing), PQueue (priority queue), Cabal

## 📦 Installation & Setup

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Stack](https://docs.haskellstack.org/en/stable/) — Haskell build tool
- `libsdl2-dev` — SDL2 system library (v2.0+)
- `libsdl2-ttf-dev` — SDL2 TTF extension

> **Linux / macOS**: Install SDL2 via your package manager.
> ```bash
> # Debian / Ubuntu
> sudo apt install libsdl2-dev libsdl2-ttf-dev
>
> # macOS (Homebrew)
> brew install sdl2 sdl2_ttf
> ```
> **Windows**: Install [msys2](https://www.msys2.org/) (automatically set up by Stack), then run:
> ```bash
> stack exec -- pacman -S mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf
> ```
> ⚠️ SDL2 is currently not compatible with Apple M1 chips.

### Instructions

```bash
# 1. Clone the repository
git clone https://github.com/Tinshea/Sims_City.git
cd Sims_City

# 2. Build the project
stack build

# 3. Run the game
stack run

# 4. (Optional) Run the test suite
stack test
```

## 🗂️ Project Structure

```
Sims_City/
├── app/          # Application entry point (Main.hs)
├── src/          # Core game logic (pure Haskell modules)
├── test/         # HSpec & QuickCheck test suites
├── assets/       # Game assets (sprites, fonts, etc.)
├── package.yaml  # Stack package manifest
└── stack.yaml    # Stack resolver & dependency configuration
```

## ⚙️ Key Dependencies

| Package      | Role                                      |
|--------------|-------------------------------------------|
| `sdl2`       | Window management, rendering, input       |
| `sdl2-ttf`   | TrueType font rendering                   |
| `linear`     | 2D/3D vector & matrix math                |
| `containers` | Maps, sets, and other data structures     |
| `random`     | Random generation for city events         |
| `pqueue`     | Priority queue for game scheduling        |
| `hspec`      | Unit testing framework                    |
| `QuickCheck` | Property-based testing                    |

## 📄 License

Distributed under the BSD3 License. See [`LICENSE`](./LICENSE) for details.
