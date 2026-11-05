# AGENTS.md

German-language course materials for *Wirtschaftsmathematik* (FH Südwestfalen). Slide decks are R Markdown (`.Rmd`) rendered to Beamer PDFs with the custom `fhswf` theme; PDFs are committed alongside sources.

## Setup

- R with `rmarkdown`, `knitr`, `tidyverse`; `fhswf` is GitHub-only:
  ```bash
  Rscript -e 'remotes::install_github("profboecker/fhswf")'
  ```
- `pdflatex` must be on `PATH`. `fhswf::presentation` defaults to **pdflatex**, even though the `.Rproj` is set to LuaLaTeX. Its Fira-font warning about XeLaTeX/LuaLaTeX is expected.
- There is no `opencode.json`. `CLAUDE.md` also exists but predates recent changes: it is wrong about `make` building PDFs and about `Wechsel_FPO/` (deleted). Trust this file.

## Rendering slides

Run from the deck's own directory, because assets are referenced relatively (`./bilder/...`):

```bash
cd Wirtschaftsmathematik
Rscript -e 'fhswf::render_presentation("Folien.Rmd")'
```

- This mirrors RStudio's Knit button and cleans up generated theme files, logs, caches, and `titlepage.pdf`.
- Plain `rmarkdown::render("deck.Rmd")` also works but leaves the generated `.sty` files, `titlepage.pdf`, and `.log` next to the deck. Do not commit them. Exception: `Beispiele/LGS_Beispiel/` still tracks legacy copies, so avoid blanket `rm *.sty` there.
- `Wirtschaftsmathematik/Makefile` build targets are a **no-op**: `SOURCES` is overwritten with `*gesamt.Rmd`, and no such files exist. Its `clean`/`clean-cache` targets do still delete artifacts.
- No test suite. Verification means the deck renders without errors. `test.R` is a scratch regression-plot script, not a test.

## Layout and conventions

- `Wirtschaftsmathematik/Folien.Rmd` is the main lecture deck (large); `Wirtschaftsmathematik/bilder/` holds its images.
- `LineareOptimierung/` and `Beispiele/*/` are standalone topic decks; `Snippets/` contains reusable fragments. `Snippets/Snippets.Rmd` has no YAML header and cannot be rendered alone.
- `Übungen/` contains rendered exercise PDFs only; there are no exercise sources in this repo.
- Deck front matter: `output: fhswf::presentation`, `knit: fhswf::render_presentation`, `lang: de-de`, `german: true`. `german: true` is an `fhswf` dummy-text flag, not a LaTeX language setting.
- Use German number formatting (decimal comma), UTF-8, and the existing 2-space indentation.
- Avoid raw Unicode characters in plot labels/`cat()` output; they have caused LaTeX render failures. Prefer LaTeX math or ASCII labels.
- `Beispiele/Stromdaten/stromdaten.R` reads its CSV through the repo-root-relative path `Beispiele/Stromdaten/daten_10240.csv`; run it from the repo root.

## Git notes

- Commit regenerated PDFs with their `.Rmd` changes; logs and caches are ignored.
- Remote `all` has two push URLs, mirroring GitHub (`profboecker`) and Codeberg (`stefanboecker`).
- `Wirtschaftsmathematik/.claude/worktrees/thirsty-buck` is a stray empty gitlink with no `.gitmodules`. Leave it in place; deleting it creates a spurious deletion.
