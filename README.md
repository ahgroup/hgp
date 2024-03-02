
<!-- README.md is generated from README.qmd. Please edit that file -->

<!-- ASCII ART BANNER TITLE -->
```
  _    _                 _      _  _____                       _____           _                       ___               __  
 | |  | |               | |    | |/ ____|                     |  __ \         | |                     / / |              \ \ 
 | |__| | __ _ _ __   __| | ___| | |  __ _ __ ___  _   _ _ __ | |__) |_ _  ___| | ____ _  __ _  ___  | || |__   __ _ _ __ | |
 |  __  |/ _` | '_ \ / _` |/ _ \ | | |_ | '__/ _ \| | | | '_ \|  ___/ _` |/ __| |/ / _` |/ _` |/ _ \ | || '_ \ / _` | '_ \| |
 | |  | | (_| | | | | (_| |  __/ | |__| | | | (_) | |_| | |_) | |  | (_| | (__|   < (_| | (_| |  __/ | || | | | (_| | |_) | |
 |_|  |_|\__,_|_| |_|\__,_|\___|_|\_____|_|  \___/ \__,_| .__/|_|   \__,_|\___|_|\_\__,_|\__, |\___| | ||_| |_|\__, | .__/| |
                                                        | |                               __/ |       \_\       __/ | |  /_/ 
                                                        |_|                              |___/                 |___/|_|   
```
<!-- END OF TITLE -->

# `hgp: HandelGroupPackage`

<!-- badges: start -->
<!-- badges: end -->

`hgp` is a package for storing utility functions for use across
handelgroup. Right now, `hgp` only contains a `ggplot2` theme. But we
will update this description as we add more stuff.

**Because this repo is public, absolutely NO sensitive or confidential
information should be stored here. This repo is strictly for shared
utilities.**

## Installation

You can install the development version of hgp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahgroup/hgp")
```

Or if you are in a repository with `renv` enabled (recommended):

``` r
renv::install("ahgroup/hgp")
```

## Using the `ggplot2` theme

If you have a `ggplot2` plot already created, you can add an `hgp` theme
to it in the same way as any other theme

``` r
library(ggplot2)
## basic example code
ggplot(mtcars) +
    aes(x = wt, y = mpg) +
    geom_point() +
    hgp::theme_ms()
```

You can also implement an `hgp` theme at the global (script) level.
After executing this code, all `ggplot` objects you produce afterwards
will use this theme.

``` r
ggplot2::theme_set(hgp::theme_ms())
```

## Contributions

- If you contribute to `hgp`, please follow the following steps.
- We recommend referring to the appropriate section(s) in [R Packages,
  2e](https://r-pkgs.org/) for any confusing steps.
- Ensure that you run `usethis::git_vaccinate()` on every local
  environment (typically new machine) that you intend to edit this
  package on. This helps to prevent leakage of PII.
- All functions should be documented using standard `roxygen2` syntax,
  and you should use `devtools::document()` to generate the
  documentation files.
- Please run `devtools:check()` before merging any new functionality to
  the main branch and fix any resulting messages. It is unnecessary to
  `build` the package, we just need to ensure that any checks pass.
  - Note that as of `2023-11-03`, we have not decided on a package
    license, so you will get one warning in the check results for that.
- `renv` is initialized for this repository using the EXPLICIT snapshot
  mode.
  - If you need to import a new package, you MUST update the `NAMESPACE`
    file appropriately before invoking `renv::snapshot()`.
  - Remember to keep the `NAMESPACE` and lockfile up to date as you add
    new functionality. You can update the `NAMESPACE` via
    `usethis::use_package()`.
  - Developer-only dependencies like `devtools` and `usethis` will thus
    not appear in the lockfile, which is the currently recommended best
    practice.
- If you are a new author, you should add your information to the
  `DESCRIPTION` file. The easiest way is to use `usethis::use_author()`.
  See again the R packages book for information on the different fields
  here.
- If you update the `README`, you should only edit `README.qmd`. Make
  sure to render the file manually after updating.
- If you intend to add new functionality, you should either fork the
  repo or make a new branch. This way, other users can continue to
  install the working version of the package while you develop new
  features. Remember to commit your work regularly.
