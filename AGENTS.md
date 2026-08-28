# Repository Guidelines

## Project Structure & Module Organization

`ggalttext` is an R package that generates concise alternative text from `ggplot2` objects. Source code lives in `R/`: `parse.R` contains the exported `generate_alt_text()` API, `describe_helpers.R` builds description fragments, and `language.R` defines translations and language helpers. Tests are under `tests/testthat/`, with the package test entry point in `tests/testthat.R`. Generated help pages belong in `man/`; edit the roxygen comments in `R/` instead of editing `.Rd` files directly

## Build, Test, and Development Commands

- `Rscript -e 'devtools::test()'` runs the testthat suite during development.
- `Rscript -e 'devtools::check()'` performs the full package build and R CMD check used by CI.
- `Rscript -e 'devtools::document()'` regenerates `man/` and `NAMESPACE` after changing roxygen documentation or exports.
- `air format .` formats R files according to `air.toml`.
- `jarl check .` runs the same R linter configured in GitHub Actions.

## Coding Style & Naming Conventions

- Use four-space indentation, an 80-character target line width, and base R's native pipe (`|>`).
- Follow existing snake_case names such as `describe_panel_layout_sentence()`. Keep exported interfaces documented with roxygen2; mark private helpers with `@keywords internal`.
- Prefer explicit namespace qualification for external calls, for example `ggplot2::ggplot_build()`.
- Run Air (`air format .`) and Jarl (`jarl check .`) before accepting changes. They should both be clean.
- When making changes, make sure it works across all available languages
- Never specify default value of arguments of internal functions

## Example with poor alternative text

If you receive an example of a chart that leads to a poor alt text, do the following:

- figure out what leads to an unexpected poor text: is it a geom that is too niche? the code used to make the graph is particularly weird? etc
- define what would be a great alt text for this specific chart that would match behavior for existing examples in the codebase and tests
- figure out what needs to be updated to make this (and similar) chart having a better alt text when passed into ggaltext, without adding breaking changes
- if expected behavior is unclear, ask user directly what to do, with 3 different options
- implement the required changes and add regression test for this new code. Do not necessary reuse the exact same code, but something similar with mock data, to make the test suite fast enough and avoid having to handle real data
