# Functionality: Have the functional claims of the software been confirmed?

## 1 test failure

Firstly I'd like to thank the authors for writing tests, well done! Currently, this package does not currently pass checks from `devtools::check()`, this is due to one failing test:

```
══ Failed tests ════════════════════════════════════════════════════════════════
── Failure ('test-turf.R:216:3'): if 'fc' 'reach' and 'freq'  ──────────────────
base::all(t2$reach == (t2$freq * 100)) is not TRUE

`actual`:   FALSE
`expected`: TRUE

[ FAIL 1 | WARN 0 | SKIP 0 | PASS 835 ]
Error: Test failures
Execution halted

1 error ✖ | 0 warnings ✔ | 0 notes ✔
Error: R CMD check found ERRORs
Execution halted

Exited with status 1.

```

## goodpractice checks

Running `goodpractice::gp()` I get the following recommendations to change, which I believe the author should change, and they should also run `goodpractice::gp()` themselves.

```
── GP validateHOT ───────────────────────────────

It is good practice to

  ✖ write short and simple
    functions. These functions have high
    cyclomatic complexity (>50): createHOT
    (116), turf (60), zc_diffs (54). You
    can make them easier to reason about by
    encapsulating distinct steps of your
    function into subfunctions.
  ✖ avoid long code lines, it is
    bad for readability. Also, many people
    prefer editor windows that are about 80
    characters wide. Try make your lines
    shorter than 80 characters

    R/ACBC_interpolate.R:1:81
    R/Accuracy.R:15:81
    R/Accuracy.R:19:81
    R/Accuracy.R:20:81
    R/Accuracy.R:23:81
    ... and 677 more lines

  ✖ avoid 1:length(...),
    1:nrow(...), 1:ncol(...), 1:NROW(...)
    and 1:NCOL(...) expressions. They are
    error prone and result 1:0 if the
    expression on the right hand side is
    zero. Use seq_len() or seq_along()
    instead.

    R/Accuracy.R:143:13
    R/att_imp.R:108:15
    R/att_imp.R:131:16
    R/att_imp.R:156:16
    R/att_imp.R:238:15
    ... and 61 more lines

  ✖ checking tests ... Running
    ‘testthat.R’ ERROR Running the tests in
    ‘tests/testthat.R’ failed. Last 13
    lines of output: int [1:70] 2 2 3 3 3 3
    3 3 1 1 ... chr [1:32] "Group 1" "Group
    1" "Group 1" "Group 1" "Group 1" "Group
    1" ... chr [1:70] "Group 1" "Group 2"
    "Group 1" "Group 2" "Group 1" "Group 2"
    ... [ FAIL 1 | WARN 0 | SKIP 0 | PASS
    835 ] ══ Failed tests
    ════════════════════════════════════════════════════════════════
    ── Failure ('test-turf.R:216:3'): if
    'fc' 'reach' and 'freq'
    ────────────────── base::all(t2$reach
    == (t2$freq * 100)) is not TRUE
    `actual`: FALSE `expected`: TRUE [ FAIL
    1 | WARN 0 | SKIP 0 | PASS 835 ] Error:
    Test failures Execution halted
  ✖ avoid 'T' and 'F', as they are
    just variables which are set to the
    logicals 'TRUE' and 'FALSE' by default,
    but are not reserved words and hence
    can be overwritten by the user.  Hence,
    one should always use 'TRUE' and
    'FALSE' for the logicals.

    R/KL.R:NA:NA
    R/KL.R:NA:NA
    R/MAE.R:NA:NA
    R/MarkSim.R:NA:NA
    R/MarkSim.R:NA:NA
    ... and 2 more lines

─────────────────────────────────────────────────
```

## Data provided with the package

Great to see data sets provided in the package, and especially that the authors have documented the variables. Some small suggestions:

- [ ] Provide some example code with it that demonstrates what to do or what is expected to be done with the data.
- [ ] Document the type of variable for each variable. For example
- [ ] use lowercase snake_case for variable names. e.g., `Price_1.99` should be `price_1_99`.
- [ ] Provide a source for the data. Is it simulated? It it pulled from somewhere in particular? If so, it should be cited.

I've got some specific comments on some of the data

- ACBC_interpolate.R
  - `Price_6.99` says "Piecewise coding for price at $5." Why is it not `Price_4.99` or Price_5.99 or similar?
  - Similarly for the other variable names, `Price_10.99. Piecewise coding for price at $15`; `Price_12.99. Piecewise coding for price at $20.`; `Price_15.99. Piecewise coding for price at $25.`; `Price_25.99. Piecewise coding for price at $27.`
- ACBC.R - all good.
- CBC_lin.R - all good.
- CBC.R - all good.
- MaxDiff.R - all good

## Other notes on code

The authors can use markdown inside their roxygen code. This means they can use write:

```
`accuracy` is one of the 5 metrics of the confusion matrix
```

instead of:

```
\code{accuracy} is one of the 5 metrics of the confusion matrix
```

See https://github.com/r-lib/roxygen2md to assist doing this.

### Accuracy.R

Lines like

```r
#' @importFrom dplyr group_by summarise select pick
```

Can be replaced by putting `dplyr` in Imports in DESCRIPTION (with `usethis::use_package("dplyr")`). Using `@importFrom pkg fun` will mean that the package is loaded when the user loads the package - see https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-NAMESPACE-workflow for more detail.

In examples, 

```r
#' library(validateHOT)
```

Should be removed, the package is already loaded.

The pattern:

```r
data %>%
    dplyr::select(., {{ opts }})
```

Is unneeded, since by default `data` will be the first argument of `select`. It is equivalent to write:

```r
data %>%
    dplyr::select({{ opts }})
```

You have written a lot of tests to check for missing values and the correct types in the data, which is very thorough defensive programming! Would you consider wrapping these up into a couple of functions, so that we can get closer to the main function of accuracy? I've called this type of pattern an "error handling eclipse", and wrote about it here: https://www.njtierney.com/post/2023/12/06/long-errors-smell/

I want to emphasise that it is great that you have written so many checks! But this is just a suggestion to help make your code easier to reuse for yourself. You've written out a lot of these checks throughout your package, which is great, but it also creates a bit of extra work to keep them all in check. Let me know if you'd like some help wrapping up these checks into sets of functions?

This pattern:

```r
 # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    stop("Error: argument 'none' is missing!")
  }

  # check whether opts are defined
  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }
```

Is intended to find whether the argument "none" was provided, right? You can instead use `missing`, like so:

```r
if (missing(none)) {
    stop("Error: argument 'none' is missing!")
  }
```

The code below:

```r
  if (base::length(data %>% dplyr::select(., {{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }
```

I believe should be rewritten as follows:

```r
opts_cols <- dplyr::select(data, {{ opts }})
n_opts_cols <- ncol(opts_cols)
if (n_opts_cols <= 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }
```

This provides the if statement with an "explaining variable", which makes it
easier to understand what the longer statement does. In addition using `ncol` is more precise than using `length`, as length could imply to the reader that it is measuring the number of rows.

```r
if (!(data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' has to be part of 'opts'!")
  }
```

Should be rewritten as:

```r
names_none <- dplyr::select(data, {{ none }}) %>% names()
names_opts <- dplyr::select(data, {{ opts }}) %>% names()
none_in_opts <- names_none %in% names_opts
if (!none_in_opts) {
    stop("Error: 'none' has to be part of 'opts'!")
  }
```

These checks for missing values could be wrapped up in a function:

```r
  # grouping variable
  ## check for missings
  if (base::anyNA(data %>% dplyr::select(., {{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }
```

```r
any_var_na <- function(data, selector) {
  selected_vars <- data %>%
    dplyr::select( {{ selector }})
  
  anyNA(selected_vars)
}

group_contains_na <- any_var_na(data, {{ group }} )
  # grouping variable
  ## check for missings
  if (group_contains_na) {
    warning("Warning: 'group' contains NAs!")
  }
```

The following can be rewritten to use the already existing `opts_cols` data 

```r
  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select(., {{ opts }}) %>%
    base::colnames()

  ## check whether variable is numeric
  for (i in 1:base::length(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' has to be numeric!")
    }
  }
```

As:

```r
  is_opts_numeric <- vapply(
    X = opts_cols,
    FUN = is.numeric,
    FUN.VALUE = TRUE
    )
    
  any_opts_not_numeric <- any(!is_opts_numeric)

  if (any_opts_not_numeric) {
      stop("Error: 'opts' has to be numeric!")
  }
```

Similarly, these checks for missing values should be rewritten:

```r
## check for missings
  if (anyNA(data %>% dplyr::select(., {{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }
  
  # choice
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ choice }}))) {
    stop("Error: 'choice' contains NAs!")
  }

```

```r
opts_contains_na <- any_var_na(data, {{ opts }} )
choice_contains_na <- any_var_na(data, {{ choice }} )

if (opts_contains_na) {
  stop("Error: 'opts' contains NAs!")
}
if (choice_contains_na) {
  stop("Error: 'choice' contains NAs!")
}
```

Even better would be wrapping these up into functions:

```r
check_if_missing <- function(data, vars, arg){
  vars_have_missings <- any_var_na(data, {{ vars }} )
  if (vars_have_missings){
    stop(sprintf("%s contains NAs!", arg))
  }
}

check_if_missing(data, {{ opts }}, "opts")
check_if_missing(data, {{ choice }}, "choice")
```

UP TO

```r
  ## check for str
  choi <- data %>%
    dplyr::select(., {{ choice }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[choi]])) {
    stop("Error: 'choice' has to be numeric!")
  }
```


## using `base::fn`

You do not need to namespace `base::` calls, since the `base` package is always provided with R. I suggest removing all instances of this as it unncessarily clutters the code.
