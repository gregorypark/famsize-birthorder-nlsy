fe_specs
================

``` r
library(tidyverse)
library(fixest)
library(modelsummary)

fe <- read_rds("../birthorder_wages/analysis.rds") |>
  filter(fe)
```

Spec 4: family fixed effects on families with at least two siblings in
the analysis sample. Men and women pooled, birth order interacted with
`female`, so `bo = k` is the effect for men and `bo = k x female` is the
extra effect for women. Birth-year dummies absorbed alongside family FE.
Unweighted, SEs clustered by family. Family size and family background
are constant within a family and drop out. So does mother’s age at
birth: it equals the child’s birth year minus the mother’s birth year,
and those are absorbed by the birth-year dummies and the family FE.

`reg_table` prints coefficients with clustered SEs in parentheses. + p
\< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001.

``` r
reg_table <- function(models, ...) {
  modelsummary(models, output = "data.frame", stars = TRUE, fmt = 3,
               gof_map = c("nobs", "r.squared", "r2.within"),
               coef_rename = \(x) x |> str_replace("::", " = ") |> str_replace(":female", " x female"), ...) |>
    mutate(term = if_else(statistic == "std.error", "", term)) |>
    select(-part, -statistic) |>
    knitr::kable()
}
```

4a and 4b run on the same sample (no missing AFQT) so the change from 4a
to 4b is only from the mediators. Dropping missing AFQT leaves some
families with one sibling; `feols` drops these singletons since they add
no within-family variation.

``` r
fe <- drop_na(fe, afqt)
```

## Spec 4a

``` r
s4a <- feols(ln_wage ~ i(bo, ref = "1") + i(bo, female, ref = "1") + female | fam_id + byear,
             fe, cluster = ~fam_id)
```

    ## NOTE: 61/0 fixed-effect singletons were removed (61 observations).

## Spec 4b: with mediators

``` r
s4b <- feols(ln_wage ~ i(bo, ref = "1") + i(bo, female, ref = "1") + female + afqt + educ | fam_id + byear,
             fe, cluster = ~fam_id)
```

    ## NOTE: 61/0 fixed-effect singletons were removed (61 observations).

``` r
reg_table(list(`4a` = s4a, `4b` = s4b))
```

| term             | 4a           | 4b           |
|:-----------------|:-------------|:-------------|
| bo = 2           | -0.016       | -0.009       |
|                  | (0.036)      | (0.034)      |
| bo = 3           | -0.046       | -0.021       |
|                  | (0.044)      | (0.042)      |
| bo = 4           | -0.043       | -0.008       |
|                  | (0.054)      | (0.051)      |
| bo = 5+          | -0.038       | -0.005       |
|                  | (0.064)      | (0.059)      |
| bo = 2 x female  | -0.039       | -0.054       |
|                  | (0.054)      | (0.050)      |
| bo = 3 x female  | -0.067       | -0.080       |
|                  | (0.057)      | (0.055)      |
| bo = 4 x female  | -0.109+      | -0.105+      |
|                  | (0.059)      | (0.056)      |
| bo = 5+ x female | -0.040       | -0.051       |
|                  | (0.057)      | (0.054)      |
| female           | -0.164\*\*\* | -0.185\*\*\* |
|                  | (0.043)      | (0.041)      |
| afqt             |              | 0.142\*\*\*  |
|                  |              | (0.015)      |
| educ             |              | 0.041\*\*\*  |
|                  |              | (0.006)      |
| Num.Obs.         | 3342         | 3342         |
| R2               | 0.684        | 0.720        |
| R2 Within        | 0.077        | 0.181        |

## Effects for women

Birth order effect for women is `bo = k` + `bo = k x female`. SE from
the clustered vcov.

``` r
women <- function(m) {
  b <- coef(m)
  v <- vcov(m)
  map(c("2", "3", "4", "5+"), \(k) {
    x <- paste0("bo::", k)
    y <- paste0(x, ":female")
    tibble(bo = k, est = b[x] + b[y], se = sqrt(v[x, x] + v[y, y] + 2 * v[x, y]))
  }) |>
    bind_rows() |>
    mutate(p = 2 * pnorm(-abs(est / se)))
}

bind_rows(`4a` = women(s4a), `4b` = women(s4b), .id = "model") |>
  knitr::kable(digits = 3)
```

| model | bo  |    est |    se |     p |
|:------|:----|-------:|------:|------:|
| 4a    | 2   | -0.054 | 0.038 | 0.155 |
| 4a    | 3   | -0.113 | 0.047 | 0.017 |
| 4a    | 4   | -0.153 | 0.056 | 0.007 |
| 4a    | 5+  | -0.078 | 0.068 | 0.249 |
| 4b    | 2   | -0.062 | 0.036 | 0.082 |
| 4b    | 3   | -0.101 | 0.045 | 0.023 |
| 4b    | 4   | -0.113 | 0.053 | 0.032 |
| 4b    | 5+  | -0.056 | 0.063 | 0.374 |

## Checks

Drop families with inconsistent reports, kept in the main sample as in
Black et al. (2005). First only families where reported birth order
doesn’t match the siblings’ birth dates (error in the variable of
interest), then also families where siblings report different family
sizes (absorbed by the family FE, so it only matters through birth
order).

``` r
bad <- fe |>
  arrange(fam_id, birth_year, birth_month) |>
  summarise(bo_bad = any(diff(birth_order) <= 0),
            size_bad = n_distinct(fam_size) > 1, .by = fam_id)

bo_bad <- bad$fam_id[bad$bo_bad]
any_bad <- bad$fam_id[bad$bo_bad | bad$size_bad]

s4a_bo <- feols(ln_wage ~ i(bo, ref = "1") + i(bo, female, ref = "1") + female | fam_id + byear,
                filter(fe, !fam_id %in% bo_bad), cluster = ~fam_id)
```

    ## NOTE: 61/0 fixed-effect singletons were removed (61 observations).

``` r
s4a_clean <- feols(ln_wage ~ i(bo, ref = "1") + i(bo, female, ref = "1") + female | fam_id + byear,
                   filter(fe, !fam_id %in% any_bad), cluster = ~fam_id)
```

    ## NOTE: 61/0 fixed-effect singletons were removed (61 observations).

``` r
reg_table(list(`4a all` = s4a, `4a drop birth order mismatch` = s4a_bo,
               `4a drop any mismatch` = s4a_clean))
```

| term | 4a all | 4a drop birth order mismatch | 4a drop any mismatch |
|:---|:---|:---|:---|
| bo = 2 | -0.016 | 0.001 | -0.001 |
|  | (0.036) | (0.037) | (0.040) |
| bo = 3 | -0.046 | -0.017 | -0.016 |
|  | (0.044) | (0.048) | (0.053) |
| bo = 4 | -0.043 | -0.018 | -0.023 |
|  | (0.054) | (0.061) | (0.071) |
| bo = 5+ | -0.038 | -0.000 | -0.008 |
|  | (0.064) | (0.074) | (0.086) |
| bo = 2 x female | -0.039 | -0.050 | -0.048 |
|  | (0.054) | (0.054) | (0.056) |
| bo = 3 x female | -0.067 | -0.080 | -0.081 |
|  | (0.057) | (0.058) | (0.061) |
| bo = 4 x female | -0.109+ | -0.093 | -0.100 |
|  | (0.059) | (0.061) | (0.066) |
| bo = 5+ x female | -0.040 | -0.045 | -0.025 |
|  | (0.057) | (0.061) | (0.066) |
| female | -0.164\*\*\* | -0.156\*\*\* | -0.164\*\*\* |
|  | (0.043) | (0.043) | (0.045) |
| Num.Obs. | 3342 | 3076 | 2747 |
| R2 | 0.684 | 0.682 | 0.683 |
| R2 Within | 0.077 | 0.073 | 0.076 |
