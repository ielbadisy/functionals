
# funr: Functional mapping with parallelism and progress bars

## Overview

`funr` is a lightweight toolkit for functional programming in R with
built-in support for parallelism and progress bars. It extends base R’s
functional tools with a consistent, minimal API for mapping, walking,
reducing, cross-validating, and repeating computations across lists,
data frames, and grouped data.

## Function Reference Table

| Function | Main arguments | Output type | Description |
|----|----|----|----|
| `fmap()` | `.x`, `.f`, `ncores`, `pb` | list | Map `.f` over elements of `.x` |
| `fmapn()` | `.l`, `.f`, `ncores`, `pb` | list | Map `.f` over multiple aligned lists |
| `fmapr()` | `.df`, `.f`, `ncores`, `pb` | list | Map `.f` over each row of a data frame (as named list) |
| `fmapc()` | `.df`, `.f`, `ncores`, `pb` | list | Map `.f(column, name)` over each column |
| `fmapg()` | `.df`, `.f`, `by`, `ncores`, `pb` | list | Map `.f(group_df)` over groups defined by a column |
| `floop()` | `.x`, `.f`, `...`, `ncores`, `pb` | list | General-purpose functional loop with side-effects |
| `fwalk()` | `.x`, `.f`, `ncores`, `pb` | NULL | Map `.f` over `.x` for side-effects only (invisible return) |
| `frepeat()` | `times`, `expr`, `.x`, `ncores`, `pb` | list/vector | Repeat a call/expression multiple times |
| `fcv()` | `.splits`, `.f`, `ncores`, `pb` | list | Map `.f` over resampling splits from `rsample::vfold_cv()` |
| `freduce()` | `.x`, `.f`, `...` | scalar/list | Reduce `.x` using a binary function `.f` |
| `fcompose()` | any number of functions `f1, f2, ...` | function | Compose multiple functions: `f1(f2(...(x)))` |
| `fapply()` | `.x`, `.f`, `ncores`, `pb`, `...` | list | Core internal utility for applying a function over `.x` |

## Syntax Equivalence

| Task | `funr` Example | `purrr` Example | Base R |
|----|----|----|----|
| Map square | `fmap(1:5, function(x) x^2)` | `map(1:5, function(x) x^2)` | `lapply(1:5, function(x) x^2)` |
| Map over N arguments | `fmapn(list(1:3, 4:6, 7:9), function(x, y, z) x + y + z)` | `pmap(list(1:3, 4:6, 7:9), function(x, y, z) ...)` | `Map(function(x, y, z) ..., 1:3, 4:6, 7:9)` |
| Map over data frame rows | `fmapr(df, function(row) row$a + row$b)` | `pmap(df[c("a", "b")], function(x, y) x + y)` | `apply(df, 1, function(row) ...)` |
| Map over data frame cols | `fmapc(df, function(x, name) mean(x))` | `imap(df, function(x, name) mean(x))` | `lapply(df, mean)` |
| Grouped map | `fmapg(df, f, by = "group")` | `map(split(df, df$group), f)` | `lapply(split(df, df$group), f)` |
| General-purpose loop | `floop(1:3, function(x) cat(x))` | *(manual recursion)* | `for (x in 1:3) cat(x)` |
| Parallel + progress | `fmap(x, f, ncores = 4, pb = TRUE)` | *(future_map(x, f))* with `progressr` | `parLapply(cl, x, f)` or `mclapply()` |
| Repeat simulation | `frepeat(100, function() rnorm(1))` | *(manual loop)* | `replicate(100, rnorm(1))` |
| Walk with side effects | `fwalk(letters, function(x) cat(x))` | `walk(letters, function(x) cat(x))` | `lapply(letters, cat)` |
| Reduce | `` freduce(1:5, `+`) `` | `` reduce(1:5, `+`) `` | `` Reduce(`+`, 1:5) `` |
| Compose functions | `fcompose(sqrt, abs)(-4)` | `compose(sqrt, abs)(-4)` | `(function(x) sqrt(abs(x)))(-4)` |

## Why no formula interface like `~ .x + .y`?

While `funr` draws inspiration from `purrr`, it intentionally avoids
supporting the formula-based anonymous function syntax (e.g.,
`~ .x + 1`) for now.

This decision is based on:

- Keeping dependencies minimal (no reliance on `rlang`)
- Avoiding non-standard evaluation that can confuse new users
- Encouraging explicit, readable code using `function(x) { ... }` style

We may consider adding tidy evaluation support (e.g., with quosures or
`rlang::as_function`) in a future release. However, the current
philosophy favors clarity and simplicity.

## Installation

``` r
# install.packages("funr") # when available
#remotes::install_github("ielabdisy/funr")
```

## Examples

``` r
library(funr)
library(purrr)
library(furrr)
#> Loading required package: future
library(pbapply)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(rsample)

plan(multisession)

# utility to compare results
compare_outputs <- function(label, x, y) {
  cat("\n", label, "->", if (identical(x, y)) "dentical\n" else if (isTRUE(all.equal(x, y))) "nearly equal\n" else "different\n")
}

# strip names and convert to plain numeric vector
as_vec <- function(x) as.numeric(unlist(x, use.names = FALSE))
```

### Element-wise map

``` r
x1 <- fmap(1:5, function(x) x^2)
x2 <- lapply(1:5, function(x) x^2)
x3 <- map(1:5, ~ .x^2)
x4 <- future_map(1:5, ~ .x^2)
x5 <- pblapply(1:5, function(x) x^2)
compare_outputs("Element-wise: base", x1, x2)
#> 
#>  Element-wise: base -> dentical
compare_outputs("Element-wise: purrr", x1, x3)
#> 
#>  Element-wise: purrr -> dentical
compare_outputs("Element-wise: furrr", x1, x4)
#> 
#>  Element-wise: furrr -> dentical
compare_outputs("Element-wise: pbapply", x1, x5)
#> 
#>  Element-wise: pbapply -> dentical
```

## Multi-input map

``` r
x1 <- fmapn(list(1:3, 4:6), function(x, y) x + y)
x2 <- Map(`+`, 1:3, 4:6)
x3 <- pmap(list(1:3, 4:6), ~ ..1 + ..2)
x4 <- future_pmap(list(1:3, 4:6), ~ ..1 + ..2)
compare_outputs("Multi-input: base", x1, x2)
#> 
#>  Multi-input: base -> dentical
compare_outputs("Multi-input: purrr", x1, x3)
#> 
#>  Multi-input: purrr -> dentical
compare_outputs("Multi-input: furrr", x1, x4)
#> 
#>  Multi-input: furrr -> dentical
```

### Row-wise map

``` r
x1 <- fmapr(mtcars, function(row) row$mpg + row$cyl)
rowlist <- lapply(seq_len(nrow(mtcars)), function(i) as.list(mtcars[i, ]))
x2 <- lapply(rowlist, function(row) row$mpg + row$cyl)
x3 <- map(rowlist, function(row) row$mpg + row$cyl)
compare_outputs("Row-wise: base", as_vec(x1), as_vec(x2))
#> 
#>  Row-wise: base -> dentical
compare_outputs("Row-wise: purrr", as_vec(x1), as_vec(x3))
#> 
#>  Row-wise: purrr -> dentical
```

### Column-wise map

``` r
x1 <- fmapc(mtcars, function(col, name) mean(col))
x2 <- sapply(mtcars, mean)
x3 <- imap(mtcars, ~ mean(.x))
x4 <- future_imap(mtcars, ~ mean(.x))
compare_outputs("Column-wise: base", x1, as.list(x2))
#> 
#>  Column-wise: base -> dentical
compare_outputs("Column-wise: purrr", x1, x3)
#> 
#>  Column-wise: purrr -> dentical
compare_outputs("Column-wise: furrr", x1, x4)
#> 
#>  Column-wise: furrr -> dentical
```

### Group-wise map

``` r
x1 <- fmapg(iris, function(df) colMeans(df[1:4]), by = "Species")
x2 <- lapply(split(iris, iris$Species), function(df) colMeans(df[1:4]))
x3 <- map(split(iris, iris$Species), ~ colMeans(.x[1:4]))
x4 <- future_map(split(iris, iris$Species), ~ colMeans(.x[1:4]))
compare_outputs("Group-wise: base", x1, x2)
#> 
#>  Group-wise: base -> dentical
compare_outputs("Group-wise: purrr", x1, x3)
#> 
#>  Group-wise: purrr -> dentical
compare_outputs("Group-wise: furrr", x1, x4)
#> 
#>  Group-wise: furrr -> dentical
```

### Side-effect map

``` r
cat("\nSide-effects:\n")
#> 
#> Side-effects:
fwalk(1:3, print)
#> [1] 1
#> [1] 2
#> [1] 3
```

### General-purpose loop with return values

``` r
x1 <- floop(1:5, function(x) x^2, .capture = TRUE)
x2 <- lapply(1:5, function(x) x^2)
x3 <- {
  out <- list()
  for (i in 1:5) out[[i]] <- i^2
  out
}
compare_outputs("floop() vs lapply()", x1, x2)
#> 
#>  floop() vs lapply() -> dentical
compare_outputs("floop() vs for()", x1, x3)
#> 
#>  floop() vs for() -> dentical
```

### General-purpose loop (side-effect only)

``` r
cat("\nGeneral-purpose loop (side-effects):\n")
#> 
#> General-purpose loop (side-effects):
floop(1:3, function(x) cat("floop says:", x, "\n"), pb = TRUE, .capture = FALSE)
#>  |                                                  |   0% elapsed=00h 00m 00s, remaining~...floop says: 1 
#>  |================                                  |  33% elapsed=00h 00m 00s, remaining~00h 00m 00sfloop says: 2 
#>  |=================================                 |  67% elapsed=00h 00m 00s, remaining~00h 00m 00sfloop says: 3 
#>  |==================================================| 100% elapsed=00h 00m 00s, remaining~00h 00m 00s
cat("for-loop equivalent:\n")
#> for-loop equivalent:
for (x in 1:3) cat("for says:", x, "\n")
#> for says: 1 
#> for says: 2 
#> for says: 3
```

### Cross-validation

``` r
splits <- vfold_cv(iris, v = 3)$splits
fit_model <- function(split) mean(analysis(split)$Sepal.Length)
x1 <- fcv(splits, fit_model)
x2 <- lapply(splits, fit_model)
compare_outputs("CV map: base", x1, x2)
#> 
#>  CV map: base -> dentical
```

### Repeat simulation

``` r
x1 <- frepeat(times = 10, expr = rnorm(1))
x2 <- as.list(replicate(10, rnorm(1)))
x3 <- as.list(pbreplicate(10, rnorm(1)))
cat("\nRepeat: Results not comparable (randomized output)\n")
#> 
#> Repeat: Results not comparable (randomized output)
```

### Reduce

``` r
x1 <- freduce(1:5, `+`)
x2 <- Reduce(`+`, 1:5)
x3 <- reduce(1:5, `+`)
compare_outputs("Reduce: base", x1, x2)
#> 
#>  Reduce: base -> dentical
compare_outputs("Reduce: purrr", x1, x3)
#> 
#>  Reduce: purrr -> dentical
```

### Compose

``` r
x1 <- fcompose(sqrt, abs)(-4)
x2 <- (function(x) sqrt(abs(x)))(-4)
x3 <- compose(sqrt, abs)(-4)
compare_outputs("Compose: base", x1, x2)
#> 
#>  Compose: base -> dentical
compare_outputs("Compose: purrr", x1, x3)
#> 
#>  Compose: purrr -> dentical
```
