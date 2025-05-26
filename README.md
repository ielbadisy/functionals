
# funr

## Installation

You can install the development version of funr from [GitHub](https://github.com/) with:

```r
# install.packages("pak")
pak::pak("ielbadisy/funr")
```

## Example

ADD A BASIC EXAMPLE HERE

### Core functions

| Function        | Description                             | Base Equivalent                  | purrr Equivalent |
| --------------- | --------------------------------------- | -------------------------------- | ---------------- |
| `fmap()`        | Map a function over elements            | `lapply()`                       | `map()`          |
| `fold()`        | Fold/reduce (left) a list               | `Reduce()`                       | `reduce()`       |
| `faccumulate()` | Accumulate intermediate results of fold | `Reduce(..., accumulate = TRUE)` | `accumulate()`   |
| `fmapn()`       | Map over multiple vectors/lists (n-ary) | `mapply()` / `Map()`             | `pmap()`         |
| `frepn()` *      | Repeat an expression multiple times     | `replicate()`                    | (no exact match) |
| `safe()`        | Wrap a function with error handling     | `tryCatch()`                     | `safely()`       |
| `pipe()`  *      | Pipe a value through transformations    | manual nesting or `%>%`          | `%>%`            |
| `fkeep()`       | Keep elements where predicate is `TRUE` | `Filter()`                       | `keep()`         |
| `fdrop()`       | Drop elements where predicate is `TRUE` | `Filter(Negate(f))`              | `discard()`      |
| `fchain()` *     | Compose multiple functions              | `Reduce()` over list of funcs    | `compose()`      |
| `fpluck()` *      | Extract nested element by name/index    | `[[`                             | `pluck()`        |
