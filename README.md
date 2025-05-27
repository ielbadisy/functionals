
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

#### `fmap()`: Basic Map (Vector/List)

* **Input**: A single vector or list
* **Behavior**: Applies `.f(x)` to each element
* **Analogy**: `map()` in purrr, `lapply()` in base R

```r
fmap(1:3, sqrt)
# → sqrt(1), sqrt(2), sqrt(3)
```

---

#### `fmapn()`: N-ary Map (Zipped Multiple Lists)

* **Input**: A list of *n* equal-length vectors (like zipped columns)
* **Behavior**: Applies `.f(a1, a2, ..., an)` to each row of zipped inputs
* **Analogy**: `pmap()` in purrr, `Map()` in base R

```r
fmapn(list(a = 1:3, b = 4:6), function(x, y) x + y)
# → (1+4), (2+5), (3+6)
```

---

#### `fmapr()`: Row-wise Map (Data Frame Rows)

* **Input**: A `data.frame`
* **Behavior**: Applies `.f(row)` to each row, where `row` is a named list
* **Analogy**: `purrr::pmap(df, .f)` or `apply(df, 1, ...)`, but cleaner and safer

```r
df <- data.frame(a = 1:3, b = 4:6)
fmapr(df, function(row) row$a + row$b)
# → (1+4), (2+5), (3+6)
```

---

#### `fmapc()`: Column-wise Map

* **Input**: A `data.frame`
* **Behavior**: Applies `.f(column, name)` to each column
* **Analogy**: `sapply(df, f)` but with named and parallelized columns

```r
fmapc(iris, function(col, name) if (is.numeric(col)) mean(col))
```

---

#### `fmapg()`: Group-wise Map

* **Input**: A data.frame + grouping column(s) via `by = ...`
* **Behavior**: Applies `.f(group_df)` to each group
* **Analogy**: `dplyr::group_split()` + `map()`

```r
fmapg(iris, function(g) mean(g$Sepal.Length), by = "Species")
```

---

#### `frepeat()`: Repeat Expression or Function

* **Input**: Number of repetitions and expression or function
* **Behavior**: Repeats evaluation `n` times
* **Analogy**: `replicate()` but supports parallelism

```r
frepeat(5, rnorm(2))
frepeat(10, function() mean(rnorm(1000)))
```

---

#### `fcv()`: Cross-validation Mapping

* **Input**: A list of CV `splits` (e.g., from `rsample::vfold_cv`)
* **Behavior**: Applies `.f(split)` to each resample
* **Analogy**: like mapping over `vfold_cv$splits`

```r
fcv(cv$splits, function(split) mean(analysis(split)$Sepal.Length))
```

---

#### `fwalk()`: Side-effect Map

* **Input**: A list or vector
* **Behavior**: Applies `.f(x)` for side-effects only (e.g., plotting, logging)
* **Analogy**: `purrr::walk()`

```r
fwalk(1:3, print)
```

---

### `fmap` Family Overview

| Function    | Input Structure               | Function Signature    | Use Case                                    |
| ----------- | ----------------------------- | --------------------- | ------------------------------------------- |
| `fmap()`    | Vector / List                 | `.f(x)`               | Element-wise mapping                        |
| `fmapn()`   | List of vectors               | `.f(x1, x2, ..., xn)` | Zipped row-wise multiple args               |
| `fmapr()`   | Data frame (rows)             | `.f(row)`             | Row-wise data.frame mapping                 |
| `fmapc()`   | Data frame (columns)          | `.f(column, name)`    | Column-wise processing (e.g., mean, t-test) |
| `fmapg()`   | Grouped data.frame            | `.f(group)`           | Group-wise computation                      |
| `frepeat()` | Integer + function/expression | `expr or .f()`        | Repetition or simulation                    |
| `fcv()`     | CV splits from rsample        | `.f(split)`           | Functional CV evaluation                    |
| `fwalk()`   | Vector / List                 | `.f(x)`               | Execute for side-effects only               |
