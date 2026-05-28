# Sims City

> Academic project developed as part of my PAF (Programmation Avancée en style Fonctionnel) at STL Master — Sorbonne Université

A SimCity-inspired city-builder game built entirely in Haskell, where the player constructs and manages a living city on a top-down grid. The simulation runs in real time, managing individual citizen lifecycles — including immigration, work, rest, hunger, and emigration — while the player balances economy, pollution, safety, and infrastructure.

## 🛠️ Tech Stack

- **Core Technologies**: Haskell, SDL2 (rendering & input), SDL2-TTF (font rendering), Linear (vector math)
- **Tools & Environment**: Stack (build & dependency management), HSpec (unit testing), QuickCheck (property-based testing), PQueue (A\* pathfinding), Cabal

## 📦 Installation & Setup

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) — Glasgow Haskell Compiler
- [Stack](https://docs.haskellstack.org/en/stable/) — Haskell build tool
- `libsdl2-dev` — SDL2 system library (v2.0+)
- `libsdl2-ttf-dev` — SDL2 TTF extension

> **Linux / Ubuntu:**
> ```bash
> sudo apt install libsdl2-dev libsdl2-ttf-dev
> ```
> **macOS (Homebrew):**
> ```bash
> brew install sdl2 sdl2_ttf
> ```
> **Windows:** Install [msys2](https://www.msys2.org/) (automatically configured by Stack), then:
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

## 🎮 Gameplay

The game runs on a 1920×1080 top-down grid. Using the **Tools panel** (top-right), the player places infrastructure by spending money, and monitors key city metrics in real time.

### 🏗️ Buildable Infrastructures

| Infrastructure     | Size  | Cost  | Description                                                                 |
|--------------------|-------|-------|-----------------------------------------------------------------------------|
| Residential Zone   | 70×70 | $100  | Houses citizens. Contains up to 4 buildings (shacks or houses). Collects rent. |
| Commercial Zone    | 70×70 | $100  | Contains 4 grocery stores. Citizens shop here to restore hunger.           |
| Industrial Zone    | 70×70 | $100  | Contains 4 workshops. Citizens earn a salary of $200 per work cycle.       |
| Road               | 30×30 | $10   | Required adjacency for all zones. New roads must connect to existing ones. |
| Energy Plant       | 70×70 | $500  | Powers adjacent zones. Increases pollution by +50 units.                   |
| Wire               | —     | $5    | Conducts electricity from energy plants to distant zones.                  |
| Police Station     | 70×70 | $100  | Increases city safety by +1 per station within 500px of residential/commercial zones. |

### 📊 City Metrics

| Metric     | Description                                                                 |
|------------|-----------------------------------------------------------------------------|
| Money      | Earned via taxes and rent. Spent on infrastructure.                        |
| Population | Total number of citizens currently in the city.                            |
| Pollution  | Increases near industrial zones (+10) and energy plants (+50).             |
| Safety     | Improves with police stations near residential/commercial zones.           |

## 👥 Citizen Lifecycle

Citizens go through three distinct states:

```
Immigrant ──(zone available)──► Citizen ──(broke / starving / exhausted)──► Emigrant ──► expelled
```

- **Immigrants** spawn at a rate of 1% per tick (1–20 at a time) when pollution is low, safety is sufficient, and population is under 100.
- **Citizens** are each assigned one residential, one industrial, and one commercial building. Their state (money, hunger, fatigue) updates every 100 ticks.
- **Emigrants** are expelled from the city every tick. Citizens become emigrants when money = 0, hunger = 0, or fatigue = 0.

### Citizen Behavior Loop

| Occupation  | Trigger                  | Effect                                      |
|-------------|--------------------------|---------------------------------------------|
| `Travailler`| Default                  | +$200, −5 hunger, −10 fatigue               |
| `Shopping`  | Hunger < 20              | Moves to grocery store, +20 hunger/tick     |
| `Dormir`    | Fatigue < 20             | Moves to residence, +20 fatigue/tick        |
| `Move`      | Changing destination     | Travels toward target zone                  |

## 🗂️ Project Structure

```
Sims_City/
├── app/              # Entry point (Main.hs)
├── src/              # Core game logic
│   ├── Ville.hs      # City model, citizen lifecycle, A* pathfinding, economy
│   └── Shapes.hs     # Geometric types (Coord, Forme, Zone, Batiment)
├── test/             # HSpec unit tests & QuickCheck properties
│   ├── VilleSpec.hs  # Tests for city operations
│   └── ShapesSpec.hs # Tests for geometric primitives
├── assets/           # Game sprites and fonts (Minecraft-inspired)
├── package.yaml      # Stack package manifest
└── stack.yaml        # Stack resolver & dependency config
```

## ✅ Testing

The project uses two complementary testing strategies:

- **HSpec unit tests** — cover core operations: route connectivity, zone collision, power supply (`isPowered`), citizen updates, A\* pathfinding, building placement, pollution calculation, and all geometric predicates.
- **QuickCheck property tests** — define invariants and pre/post-conditions for all key types (`Ville`, `Citoyen`, `Zone`, `Batiment`, `Forme`, `Coord`) and operations.

```bash
stack test
```

> ⚠️ Some QuickCheck properties (`prop_isPowered`, `prop_updateCitizens`, `prop_invariant_creer_imigrant`) are defined but currently failing and require further adjustment.

## ⚙️ Key Dependencies

| Package      | Role                                         |
|--------------|----------------------------------------------|
| `sdl2`       | Window management, rendering, event handling |
| `sdl2-ttf`   | TrueType font rendering                      |
| `linear`     | 2D vector & coordinate math                  |
| `containers` | Maps and sets for city state                 |
| `random`     | Stochastic immigrant generation              |
| `pqueue`     | Priority queue for A\* pathfinding           |
| `hspec`      | Unit testing framework                       |
| `QuickCheck` | Property-based testing                       |

## 👤 Authors

- **Malek Bouzarkouna** — 28706508
- **Paul Mekhail** — 28708078

Sorbonne Université — Master STL, 2024

## 📄 License

Distributed under the BSD3 License. See [`LICENSE`](./LICENSE) for details.
```
