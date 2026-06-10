# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

All development is done from within R (or via `devtools`):

```r
devtools::load_all()       # Load package for interactive development
devtools::document()       # Regenerate roxygen2 docs (NAMESPACE + man/)
devtools::test()           # Run full test suite
devtools::check()          # Full R CMD check
```

Run a single test file:
```r
testthat::test_file("tests/testthat/test_ENMTools.R")
```

From the shell, build and check:
```bash
R CMD build .
R CMD check ENMTools_*.tar.gz
```

## Linting

Do not enforce line length limits or snake_case naming — existing code uses longer lines and mixed naming conventions, and consistency with the existing style takes precedence.

## Architecture

ENMTools is an R package for ecological niche modeling (ENM) and comparative niche analysis. It wraps multiple modeling backends behind a unified S3 interface, adds hypothesis-testing infrastructure, and provides visualization utilities.

### Core S3 Classes

**`enmtools.species`** — the fundamental unit. Holds:
- `presence.points`: `SpatVector` of occurrence records
- `background.points`: `SpatVector` of pseudo-absence/background points
- `range`: `SpatRaster` of species range
- `models`: named list for storing fitted model objects

**`enmtools.clade`** — a monophyletic group for comparative analyses:
- `species`: named list of `enmtools.species` objects
- `tree`: `phylo` object (ape package)

**`enmtools.model`** — returned by all model-fitting functions. Slots include:
- `model`: the underlying fitted model object (e.g., a `glm`, `gam`, `randomForest`)
- `suitability`: `SpatRaster` of predicted suitability
- `training.evaluation`, `test.evaluation`: `ModelEvaluation` objects (dismo)
- `env.training.evaluation`, `env.test.evaluation`: metrics in environment space
- `response.plots`: list of ggplot2 response curves
- `clamping.strength`: SpatRaster showing where clamping occurred

### Spatial Data

The package migrated from `raster` to `terra` at v1.1.0. All spatial objects are now `SpatRaster` / `SpatVector` (terra). The `raster` package remains a dependency for `dismo` compatibility, but new code should use terra throughout.

### Model Builders

Each lives in `R/enmtools.<type>.R` and returns an `enmtools.model`:
- `enmtools.glm` / `enmtools.gam` — GLM and GAM via `stats`
- `enmtools.bc` / `enmtools.dm` — Bioclim and Domain via `dismo`
- `enmtools.maxent` — MaxEnt via `dismo`/`ENMeval`
- `enmtools.rf` / `enmtools.rf.ranger` — Random Forest via `randomForest` / `ranger`

### Hypothesis Testing

Permutation-based tests in `R/<test>.R`:
- **Niche identity/equivalency**: `identity.test` — shuffles occurrence points between two species
- **Background similarity**: `background.test` — samples background from one or both species ranges
- **Rangebreak tests**: `rangebreak.linear`, `rangebreak.blob`, `rangebreak.ribbon` — test whether niche differentiation is driven by geography
- **Ecospat wrappers**: `enmtools.ecospat.id`, `enmtools.ecospat.bg`
- **Age-Overlap Correlation**: `enmtools.aoc` — phylogenetic test correlating clade age with pairwise niche overlap

All tests refit models many times (Monte Carlo replicates) so they are slow; the `test.type` argument controls whether resampling is done in geographic or environment space.

### Overlap & Breadth Metrics

- `raster.overlap` — computes Schoener's D, I (Hellinger), and rank correlation between two suitability rasters
- `env.overlap` — same metrics in PCA-reduced environment space
- `raster.breadth` / `calc.B1` / `calc.B2` — Levins' niche breadth measures

### Validation & Utilities

- `check.species`, `check.clade`, `check.env` — validate objects before modeling; called at the top of most functions
- `background.buffer`, `background.points.buffer`, `background.raster.buffer`, `background.shape.buffer` — generate background points with various spatial constraints
- `add.env` — extract environmental values at presence/background points and attach to an `enmtools.species`

### Test Data

Built-in (loaded with the package): `iberolacerta.clade` (7 Iberolacerta lizard species with phylogeny) and `euro.worldclim` (European Worldclim rasters). Test helpers are in `tests/testthat/helper_make_species.R`.
