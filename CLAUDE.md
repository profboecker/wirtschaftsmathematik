# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Course materials for *Wirtschaftsmathematik* (Business Mathematics) taught by Prof. Dr. Stefan Böcker at FH Südwestfalen. All slide decks are R Markdown (`.Rmd`) files compiled to PDF via the custom `fhswf` Beamer theme.

## Building slides (PDF)

From the `Wirtschaftsmathematik/` directory:

```bash
make          # build all PDFs from *gesamt.Rmd sources
make pdf      # same
make clean    # remove generated PDFs and build artefacts
make clean-cache   # remove knitr cache/temp files only
```

To render a single `.Rmd` from R:

```r
rmarkdown::render("path/to/file.Rmd")
```

The `fhswf` presentation package must be installed from GitHub before first render:

```r
remotes::install_github("profboecker/fhswf")
```

The RStudio project (`wirtschaftsmathematik.Rproj`) is configured for **LuaLaTeX** and **knitr**; use these when rendering manually.

## Repository layout

| Path | Contents |
|---|---|
| `Wirtschaftsmathematik/` | Main lecture slides (`Folien.Rmd`) and image assets |
| `LineareOptimierung/` | Slides + R script for the Simplex / LP topic |
| `Wechsel_FPO/` | Info slides about the new *Fachprüfungsordnung* (curriculum regulations) |
| `Beispiele/` | Stand-alone R examples: LGS (linear systems), Stromdaten (energy data) |
| `Snippets/` | Reusable `.Rmd` slide fragments (cash-flow diagrams, squared/lined paper macros, etc.) |

## Rmd front-matter conventions

Every slide deck uses this YAML header pattern:

```yaml
output: fhswf::presentation
knit: fhswf::render_presentation
lang: de-de
german: true
```

The `knit:` field means clicking "Knit" in RStudio calls `fhswf::render_presentation` instead of the default knit function.

## Common R packages

`tidyverse`, `knitr`, `rmarkdown`, `lpSolve`, `gridExtra`, `remotes` — all available on CRAN except `fhswf` (GitHub: `profboecker/fhswf`).

## Editor settings

2-space indentation, UTF-8 encoding (set in `wirtschaftsmathematik.Rproj`).
