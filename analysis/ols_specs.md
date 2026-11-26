ols_specs
================

``` r
library(tidyverse)
library(fixest)
library(modelsummary)

analysis <- read_rds("../birthorder_wages/analysis.rds") |>
  mutate(sex = if_else(female == 1, "women", "men"),
         mom_age_cat = cut(mom_age_birth, c(13, 19, 24, 29, 34, 50),
                           labels = c("<20", "20-24", "25-29", "30-34", "35+")),
         mom_age_cat = fct_na_value_to_level(mom_age_cat, "missing"))
```

Specs 1-3 and the Booth-Kee extension. All run separately by sex
(`split = ~sex`), weighted with the custom weights, birth-year dummies
and mother’s-age-at-birth dummies absorbed, SEs clustered by family.
Mother’s age dummies follow Black et al. (2005), who include them in
every regression: later-borns have older mothers, so without them birth
order picks up mother’s age. Missing mother’s age gets its own category,
as Black et al. do for missing father information. Firstborns are the
omitted birth order; only children are firstborns picked up by
`only_child`.

Table helpers. `by_sex` names each split model “spec men” / “spec
women”; `reg_table` prints coefficients with clustered SEs in
parentheses. + p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
0.001.

``` r
by_sex <- function(...) {
  imap(list(...), \(m, nm) set_names(as.list(m), paste(nm, c("men", "women")))) |>
    list_flatten(name_spec = "{inner}")
}

reg_table <- function(models, ...) {
  modelsummary(models, output = "data.frame", stars = TRUE, fmt = 3,
               gof_map = c("nobs", "r.squared"),
               coef_rename = \(x) str_replace(x, "::", " = "), ...) |>
    mutate(term = if_else(statistic == "std.error", "", term)) |>
    select(-part, -statistic) |>
    knitr::kable()
}
```

## Spec 1: Baseline

1a: family size only. 1b: birth order only.

``` r
s1a <- feols(ln_wage ~ fam_size + only_child | byear + mom_age_cat, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
s1b <- feols(ln_wage ~ i(bo, ref = "1") + only_child | byear + mom_age_cat, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`1a` = s1a, `1b` = s1b))
```

| term       | 1a men       | 1a women     | 1b men       | 1b women     |
|:-----------|:-------------|:-------------|:-------------|:-------------|
| fam_size   | -0.047\*\*\* | -0.037\*\*\* |              |              |
|            | (0.004)      | (0.005)      |              |              |
| only_child | -0.138\*\*   | -0.081       | -0.081       | -0.072       |
|            | (0.053)      | (0.054)      | (0.054)      | (0.056)      |
| bo = 2     |              |              | -0.069\*     | -0.070\*     |
|            |              |              | (0.028)      | (0.029)      |
| bo = 3     |              |              | -0.100\*\*   | -0.138\*\*\* |
|            |              |              | (0.031)      | (0.034)      |
| bo = 4     |              |              | -0.120\*\*\* | -0.203\*\*\* |
|            |              |              | (0.034)      | (0.036)      |
| bo = 5+    |              |              | -0.285\*\*\* | -0.268\*\*\* |
|            |              |              | (0.034)      | (0.038)      |
| Num.Obs.   | 3768         | 3727         | 3768         | 3727         |
| R2         | 0.063        | 0.052        | 0.045        | 0.051        |

## Spec 2: Birth order and family size

Key test: does the family size coefficient shrink from 1a once birth
order is held fixed?

``` r
s2 <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child | byear + mom_age_cat, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`1a` = s1a, `2` = s2), coef_omit = "^(?!fam_size)")
```

| term     | 1a men       | 1a women     | 2 men        | 2 women      |
|:---------|:-------------|:-------------|:-------------|:-------------|
| fam_size | -0.047\*\*\* | -0.037\*\*\* | -0.044\*\*\* | -0.025\*\*\* |
|          | (0.004)      | (0.005)      | (0.006)      | (0.006)      |
| Num.Obs. | 3768         | 3727         | 3768         | 3727         |
| R2       | 0.063        | 0.052        | 0.065        | 0.058        |

``` r
reg_table(by_sex(`2` = s2))
```

| term       | 2 men        | 2 women      |
|:-----------|:-------------|:-------------|
| bo = 2     | -0.055\*     | -0.060\*     |
|            | (0.028)      | (0.029)      |
| bo = 3     | -0.047       | -0.102\*\*   |
|            | (0.032)      | (0.034)      |
| bo = 4     | -0.016       | -0.144\*\*\* |
|            | (0.036)      | (0.037)      |
| bo = 5+    | -0.067       | -0.145\*\*   |
|            | (0.042)      | (0.046)      |
| fam_size   | -0.044\*\*\* | -0.025\*\*\* |
|            | (0.006)      | (0.006)      |
| only_child | -0.166\*\*   | -0.121\*     |
|            | (0.056)      | (0.057)      |
| Num.Obs.   | 3768         | 3727         |
| R2         | 0.065        | 0.058        |

Check: Spec 2 without the mother’s age dummies, to see how much they
move the birth order coefficients.

``` r
s2_nomom <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child | byear, analysis,
                  split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`2` = s2, `2 no mom age` = s2_nomom))
```

| term       | 2 men        | 2 women      | 2 no mom age men | 2 no mom age women |
|:-----------|:-------------|:-------------|:-----------------|:-------------------|
| bo = 2     | -0.055\*     | -0.060\*     | -0.017           | -0.015             |
|            | (0.028)      | (0.029)      | (0.026)          | (0.029)            |
| bo = 3     | -0.047       | -0.102\*\*   | 0.021            | -0.012             |
|            | (0.032)      | (0.034)      | (0.029)          | (0.032)            |
| bo = 4     | -0.016       | -0.144\*\*\* | 0.071\*          | -0.035             |
|            | (0.036)      | (0.037)      | (0.033)          | (0.034)            |
| bo = 5+    | -0.067       | -0.145\*\*   | 0.049            | 0.016              |
|            | (0.042)      | (0.046)      | (0.037)          | (0.041)            |
| fam_size   | -0.044\*\*\* | -0.025\*\*\* | -0.051\*\*\*     | -0.035\*\*\*       |
|            | (0.006)      | (0.006)      | (0.006)          | (0.006)            |
| only_child | -0.166\*\*   | -0.121\*     | -0.150\*\*       | -0.088             |
|            | (0.056)      | (0.057)      | (0.056)          | (0.056)            |
| Num.Obs.   | 3768         | 3727         | 3768             | 3727               |
| R2         | 0.065        | 0.058        | 0.050            | 0.030              |

Robustness (paper footnote): family size as dummies instead of linear.
Family size 1 is the omitted category, which is the same as
`only_child`, so `only_child` drops out.

``` r
s2_dum <- feols(ln_wage ~ i(bo, ref = "1") + i(fam_size_cat, ref = "1") | byear + mom_age_cat, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`2 dummies` = s2_dum))
```

| term              | 2 dummies men | 2 dummies women |
|:------------------|:--------------|:----------------|
| bo = 2            | -0.050+       | -0.062\*        |
|                   | (0.028)       | (0.029)         |
| bo = 3            | -0.033        | -0.112\*\*      |
|                   | (0.033)       | (0.036)         |
| bo = 4            | -0.004        | -0.146\*\*\*    |
|                   | (0.039)       | (0.040)         |
| bo = 5+           | -0.072+       | -0.160\*\*\*    |
|                   | (0.043)       | (0.046)         |
| fam_size_cat = 2  | 0.132\*       | 0.078           |
|                   | (0.058)       | (0.060)         |
| fam_size_cat = 3  | 0.061         | 0.089           |
|                   | (0.056)       | (0.059)         |
| fam_size_cat = 4  | 0.051         | 0.050           |
|                   | (0.059)       | (0.061)         |
| fam_size_cat = 5  | -0.052        | 0.034           |
|                   | (0.061)       | (0.063)         |
| fam_size_cat = 6+ | -0.134\*      | -0.042          |
|                   | (0.061)       | (0.063)         |
| Num.Obs.          | 3768          | 3727            |
| R2                | 0.063         | 0.056           |

Robustness (Black et al. 2005): birth order effects within each family
size, so family size is held fixed exactly. Family sizes 2-5; 6+ mixes
sizes. Family size and `only_child` are constant within each group, so
they drop out.

``` r
sizes <- filter(analysis, fam_size %in% 2:5)

size_men <- feols(ln_wage ~ i(bo, ref = "1") | byear + mom_age_cat, filter(sizes, sex == "men"),
                  split = ~fam_size, weights = ~wt, cluster = ~fam_id)
size_women <- feols(ln_wage ~ i(bo, ref = "1") | byear + mom_age_cat, filter(sizes, sex == "women"),
                    split = ~fam_size, weights = ~wt, cluster = ~fam_id)
```

Men:

``` r
reg_table(set_names(as.list(size_men), paste("size", 2:5)))
```

| term     | size 2  | size 3  | size 4  | size 5  |
|:---------|:--------|:--------|:--------|:--------|
| bo = 2   | -0.024  | -0.050  | -0.029  | -0.059  |
|          | (0.052) | (0.051) | (0.065) | (0.084) |
| bo = 3   |         | -0.030  | -0.016  | -0.087  |
|          |         | (0.062) | (0.067) | (0.082) |
| bo = 4   |         |         | 0.041   | -0.092  |
|          |         |         | (0.075) | (0.092) |
| bo = 5+  |         |         |         | -0.038  |
|          |         |         |         | (0.099) |
| Num.Obs. | 482     | 810     | 683     | 488     |
| R2       | 0.070   | 0.047   | 0.042   | 0.065   |

Women:

``` r
reg_table(set_names(as.list(size_women), paste("size", 2:5)))
```

| term     | size 2   | size 3  | size 4  | size 5  |
|:---------|:---------|:--------|:--------|:--------|
| bo = 2   | -0.120\* | -0.052  | -0.068  | -0.042  |
|          | (0.056)  | (0.056) | (0.065) | (0.106) |
| bo = 3   |          | -0.124+ | -0.088  | -0.191+ |
|          |          | (0.065) | (0.071) | (0.106) |
| bo = 4   |          |         | -0.123  | -0.137  |
|          |          |         | (0.078) | (0.103) |
| bo = 5+  |          |         |         | -0.188+ |
|          |          |         |         | (0.114) |
| Num.Obs. | 481      | 740     | 708     | 539     |
| R2       | 0.061    | 0.046   | 0.055   | 0.099   |

## Spec 3: Controls and mediators

Like Table IV of Black et al. (2005): each set of controls without and
then with birth order. 3a adds race and family background, 3b adds birth
order. 3c and 3d add AFQT and education, without and with birth order.

Missing controls: dropping anyone missing a control or AFQT loses about
20% of the sample (mostly father’s education, which is usually missing
when there’s no father in the home). That subsample isn’t random, and
the custom weights are built for the full analysis sample. So instead,
missing values are set to 0 and each gets a `_miss` dummy. All four
columns then run on the full analysis sample, the same one as Specs 1-2.

``` r
m <- analysis |>
  mutate(across(c(afqt, mom_educ, dad_educ, home_learning), list(miss = ~ as.integer(is.na(.x)))),
         across(c(afqt, mom_educ, dad_educ, home_learning), ~ replace_na(.x, 0)))

s3a <- feols(ln_wage ~ fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss | byear + mom_age_cat,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3b <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss | byear + mom_age_cat,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3c <- feols(ln_wage ~ fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss +
               afqt + educ + afqt_miss | byear + mom_age_cat,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3d <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss +
               afqt + educ + afqt_miss | byear + mom_age_cat,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3 <- by_sex(`3a` = s3a, `3b` = s3b, `3c` = s3c, `3d` = s3d)
```

Men:

``` r
reg_table(s3[str_ends(names(s3), " men")])
```

| term               | 3a men       | 3b men       | 3c men       | 3d men       |
|:-------------------|:-------------|:-------------|:-------------|:-------------|
| fam_size           | -0.014\*\*\* | -0.015\*\*   | -0.004       | -0.006       |
|                    | (0.004)      | (0.005)      | (0.004)      | (0.005)      |
| only_child         | -0.035       | -0.044       | -0.042       | -0.038       |
|                    | (0.049)      | (0.051)      | (0.045)      | (0.048)      |
| racehispanic       | 0.027        | 0.028        | 0.014        | 0.017        |
|                    | (0.026)      | (0.027)      | (0.026)      | (0.026)      |
| raceblack          | -0.169\*\*\* | -0.169\*\*\* | -0.096\*\*\* | -0.098\*\*\* |
|                    | (0.020)      | (0.020)      | (0.021)      | (0.021)      |
| mom_educ           | 0.016\*\*\*  | 0.016\*\*\*  | 0.004        | 0.004        |
|                    | (0.005)      | (0.005)      | (0.004)      | (0.004)      |
| dad_educ           | 0.017\*\*\*  | 0.017\*\*\*  | 0.005        | 0.005        |
|                    | (0.004)      | (0.004)      | (0.003)      | (0.003)      |
| parent_profmgr     | 0.102\*\*\*  | 0.102\*\*\*  | 0.039+       | 0.040+       |
|                    | (0.025)      | (0.025)      | (0.023)      | (0.023)      |
| home_learning      | 0.067\*\*\*  | 0.067\*\*\*  | 0.022\*      | 0.021\*      |
|                    | (0.010)      | (0.010)      | (0.010)      | (0.010)      |
| mom_educ_miss      | 0.090        | 0.096+       | 0.027        | 0.035        |
|                    | (0.058)      | (0.058)      | (0.055)      | (0.055)      |
| dad_educ_miss      | 0.101\*      | 0.099\*      | 0.018        | 0.016        |
|                    | (0.045)      | (0.045)      | (0.042)      | (0.041)      |
| home_learning_miss | 0.030        | 0.027        | -0.057       | -0.057       |
|                    | (0.084)      | (0.084)      | (0.085)      | (0.085)      |
| bo = 2             |              | -0.026       |              | -0.010       |
|                    |              | (0.026)      |              | (0.024)      |
| bo = 3             |              | -0.013       |              | 0.014        |
|                    |              | (0.029)      |              | (0.027)      |
| bo = 4             |              | 0.042        |              | 0.076\*      |
|                    |              | (0.033)      |              | (0.031)      |
| bo = 5+            |              | -0.022       |              | 0.011        |
|                    |              | (0.039)      |              | (0.036)      |
| afqt               |              |              | 0.130\*\*\*  | 0.130\*\*\*  |
|                    |              |              | (0.012)      | (0.012)      |
| educ               |              |              | 0.044\*\*\*  | 0.044\*\*\*  |
|                    |              |              | (0.005)      | (0.005)      |
| afqt_miss          |              |              | 0.019        | 0.019        |
|                    |              |              | (0.041)      | (0.041)      |
| Num.Obs.           | 3768         | 3768         | 3768         | 3768         |
| R2                 | 0.188        | 0.190        | 0.299        | 0.301        |

Women:

``` r
reg_table(s3[str_ends(names(s3), "women")])
```

| term               | 3a women    | 3b women    | 3c women    | 3d women    |
|:-------------------|:------------|:------------|:------------|:------------|
| fam_size           | -0.014\*\*  | -0.008      | -0.004      | -0.004      |
|                    | (0.005)     | (0.006)     | (0.004)     | (0.005)     |
| only_child         | -0.011      | -0.036      | -0.031      | -0.041      |
|                    | (0.051)     | (0.054)     | (0.048)     | (0.050)     |
| racehispanic       | 0.186\*\*\* | 0.177\*\*\* | 0.171\*\*\* | 0.169\*\*\* |
|                    | (0.026)     | (0.026)     | (0.025)     | (0.025)     |
| raceblack          | 0.016       | 0.013       | 0.062\*\*   | 0.061\*\*   |
|                    | (0.020)     | (0.020)     | (0.021)     | (0.021)     |
| mom_educ           | 0.024\*\*\* | 0.023\*\*\* | 0.005       | 0.005       |
|                    | (0.005)     | (0.005)     | (0.005)     | (0.005)     |
| dad_educ           | 0.018\*\*\* | 0.018\*\*\* | 0.007\*     | 0.007\*     |
|                    | (0.003)     | (0.003)     | (0.003)     | (0.003)     |
| parent_profmgr     | 0.096\*\*\* | 0.096\*\*\* | 0.031       | 0.031       |
|                    | (0.026)     | (0.026)     | (0.024)     | (0.024)     |
| home_learning      | 0.065\*\*\* | 0.066\*\*\* | 0.018+      | 0.019+      |
|                    | (0.011)     | (0.011)     | (0.010)     | (0.010)     |
| mom_educ_miss      | 0.204\*\*   | 0.200\*\*   | 0.057       | 0.059       |
|                    | (0.065)     | (0.066)     | (0.064)     | (0.064)     |
| dad_educ_miss      | 0.148\*\*\* | 0.141\*\*   | 0.075+      | 0.074+      |
|                    | (0.045)     | (0.045)     | (0.042)     | (0.042)     |
| home_learning_miss | -0.013      | -0.007      | -0.091      | -0.085      |
|                    | (0.074)     | (0.075)     | (0.070)     | (0.069)     |
| bo = 2             |             | -0.027      |             | 0.001       |
|                    |             | (0.028)     |             | (0.025)     |
| bo = 3             |             | -0.070\*    |             | -0.038      |
|                    |             | (0.033)     |             | (0.030)     |
| bo = 4             |             | -0.092\*    |             | -0.019      |
|                    |             | (0.036)     |             | (0.034)     |
| bo = 5+            |             | -0.071      |             | 0.008       |
|                    |             | (0.044)     |             | (0.041)     |
| afqt               |             |             | 0.139\*\*\* | 0.139\*\*\* |
|                    |             |             | (0.013)     | (0.013)     |
| educ               |             |             | 0.065\*\*\* | 0.064\*\*\* |
|                    |             |             | (0.005)     | (0.005)     |
| afqt_miss          |             |             | -0.066      | -0.070      |
|                    |             |             | (0.048)     | (0.049)     |
| Num.Obs.           | 3727        | 3727        | 3727        | 3727        |
| R2                 | 0.147       | 0.149       | 0.292       | 0.293       |

Change in each birth order coefficient when the mediators enter
(`b_3d - b_3b`). A share (`1 - b_3d / b_3b`) blows up when `b_3b` is
near zero, so report the difference.

``` r
map2(s3b, s3d, \(a, b) {
  k <- str_subset(names(coef(a)), "^bo::")
  tibble(term = k, b_3b = coef(a)[k], b_3d = coef(b)[k], change = b_3d - b_3b)
}) |>
  set_names(c("men", "women")) |>
  bind_rows(.id = "sex") |>
  knitr::kable(digits = 3)
```

| sex   | term   |   b_3b |   b_3d | change |
|:------|:-------|-------:|-------:|-------:|
| men   | bo::2  | -0.026 | -0.010 |  0.016 |
| men   | bo::3  | -0.013 |  0.014 |  0.027 |
| men   | bo::4  |  0.042 |  0.076 |  0.034 |
| men   | bo::5+ | -0.022 |  0.011 |  0.033 |
| women | bo::2  | -0.027 |  0.001 |  0.028 |
| women | bo::3  | -0.070 | -0.038 |  0.031 |
| women | bo::4  | -0.092 | -0.019 |  0.073 |
| women | bo::5+ | -0.071 |  0.008 |  0.078 |

Check: Spec 2 on the full sample vs only those with every control and
AFQT. If the coefficients barely move, dropping the missing would not
have mattered much either.

``` r
cc <- drop_na(analysis, afqt, mom_educ, dad_educ, home_learning)

s2_cc <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child | byear + mom_age_cat, cc,
               split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(full = s2, `complete case` = s2_cc))
```

| term       | full men     | full women   | complete case men | complete case women |
|:-----------|:-------------|:-------------|:------------------|:--------------------|
| bo = 2     | -0.055\*     | -0.060\*     | -0.071\*          | -0.073\*            |
|            | (0.028)      | (0.029)      | (0.030)           | (0.032)             |
| bo = 3     | -0.047       | -0.102\*\*   | -0.068\*          | -0.130\*\*\*        |
|            | (0.032)      | (0.034)      | (0.035)           | (0.036)             |
| bo = 4     | -0.016       | -0.144\*\*\* | -0.010            | -0.192\*\*\*        |
|            | (0.036)      | (0.037)      | (0.040)           | (0.040)             |
| bo = 5+    | -0.067       | -0.145\*\*   | -0.080+           | -0.162\*\*          |
|            | (0.042)      | (0.046)      | (0.047)           | (0.050)             |
| fam_size   | -0.044\*\*\* | -0.025\*\*\* | -0.044\*\*\*      | -0.023\*\*\*        |
|            | (0.006)      | (0.006)      | (0.006)           | (0.007)             |
| only_child | -0.166\*\*   | -0.121\*     | -0.181\*\*        | -0.108+             |
|            | (0.056)      | (0.057)      | (0.061)           | (0.059)             |
| Num.Obs.   | 3768         | 3727         | 2959              | 2993                |
| R2         | 0.065        | 0.058        | 0.066             | 0.057               |

Robustness (Black et al. 2005, Table IV): 3a and 3b with family size
dummies instead of linear family size. Family size 1 is the omitted
category, which is the same as `only_child`, so `only_child` drops out.

``` r
s3a_dum <- feols(ln_wage ~ i(fam_size_cat, ref = "1") +
                   race + mom_educ + dad_educ + parent_profmgr + home_learning +
                   mom_educ_miss + dad_educ_miss + home_learning_miss | byear + mom_age_cat,
                 m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3b_dum <- feols(ln_wage ~ i(bo, ref = "1") + i(fam_size_cat, ref = "1") +
                   race + mom_educ + dad_educ + parent_profmgr + home_learning +
                   mom_educ_miss + dad_educ_miss + home_learning_miss | byear + mom_age_cat,
                 m, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`3a dummies` = s3a_dum, `3b dummies` = s3b_dum),
          coef_omit = "race|educ|profmgr|home_learning")
```

| term | 3a dummies men | 3a dummies women | 3b dummies men | 3b dummies women |
|:---|:---|:---|:---|:---|
| fam_size_cat = 2 | 0.037 | 0.010 | 0.047 | 0.025 |
|  | (0.052) | (0.054) | (0.053) | (0.056) |
| fam_size_cat = 3 | -0.010 | -0.013 | -0.005 | 0.021 |
|  | (0.050) | (0.052) | (0.052) | (0.055) |
| fam_size_cat = 4 | 0.018 | -0.038 | 0.006 | 0.014 |
|  | (0.049) | (0.052) | (0.054) | (0.057) |
| fam_size_cat = 5 | -0.059 | -0.052 | -0.068 | 0.008 |
|  | (0.051) | (0.053) | (0.056) | (0.059) |
| fam_size_cat = 6+ | -0.060 | -0.085+ | -0.062 | -0.023 |
|  | (0.049) | (0.051) | (0.056) | (0.059) |
| bo = 2 |  |  | -0.021 | -0.027 |
|  |  |  | (0.026) | (0.028) |
| bo = 3 |  |  | 0.004 | -0.071\* |
|  |  |  | (0.030) | (0.034) |
| bo = 4 |  |  | 0.058+ | -0.092\* |
|  |  |  | (0.035) | (0.039) |
| bo = 5+ |  |  | -0.008 | -0.070 |
|  |  |  | (0.039) | (0.044) |
| Num.Obs. | 3768 | 3727 | 3768 | 3727 |
| R2 | 0.189 | 0.147 | 0.191 | 0.149 |

## Extension: Booth-Kee birth order index

Index = birth order / ((family size + 1) / 2), averages 1 within every
family size. Same controls as Spec 2.

``` r
ext <- feols(ln_wage ~ bk_index + fam_size + only_child | byear + mom_age_cat, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(ext = ext))
```

| term       | ext men      | ext women    |
|:-----------|:-------------|:-------------|
| bk_index   | -0.021       | -0.105\*\*\* |
|            | (0.027)      | (0.029)      |
| fam_size   | -0.047\*\*\* | -0.036\*\*\* |
|            | (0.004)      | (0.004)      |
| only_child | -0.139\*\*   | -0.086       |
|            | (0.053)      | (0.054)      |
| Num.Obs.   | 3768         | 3727         |
| R2         | 0.063        | 0.057        |

Robustness (Booth and Kee 2009). First, Spec 2 and the index model
without only children, whose birth order and index are fixed at 1.
Second, instead of a linear index, dummies for index \< 0.8 (early born)
and index \> 1.2 (late born), with 0.8-1.2 (middle) omitted, also
without only children.

``` r
no_only <- filter(analysis, only_child == 0) |>
  mutate(bk_early = as.integer(bk_index < 0.8),
         bk_late = as.integer(bk_index > 1.2))

s2_no <- feols(ln_wage ~ i(bo, ref = "1") + fam_size | byear + mom_age_cat, no_only,
               split = ~sex, weights = ~wt, cluster = ~fam_id)
ext_no <- feols(ln_wage ~ bk_index + fam_size | byear + mom_age_cat, no_only,
                split = ~sex, weights = ~wt, cluster = ~fam_id)
ext_dum <- feols(ln_wage ~ bk_early + bk_late + fam_size | byear + mom_age_cat, no_only,
                 split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`2 no only` = s2_no, `ext no only` = ext_no, `ext dummies` = ext_dum))
```

| term | 2 no only men | 2 no only women | ext no only men | ext no only women | ext dummies men | ext dummies women |
|:---|:---|:---|:---|:---|:---|:---|
| bo = 2 | -0.054+ | -0.062\* |  |  |  |  |
|  | (0.028) | (0.029) |  |  |  |  |
| bo = 3 | -0.045 | -0.105\*\* |  |  |  |  |
|  | (0.032) | (0.035) |  |  |  |  |
| bo = 4 | -0.014 | -0.149\*\*\* |  |  |  |  |
|  | (0.036) | (0.038) |  |  |  |  |
| bo = 5+ | -0.064 | -0.151\*\* |  |  |  |  |
|  | (0.042) | (0.047) |  |  |  |  |
| fam_size | -0.044\*\*\* | -0.025\*\*\* | -0.047\*\*\* | -0.036\*\*\* | -0.047\*\*\* | -0.036\*\*\* |
|  | (0.006) | (0.006) | (0.004) | (0.004) | (0.004) | (0.004) |
| bk_index |  |  | -0.019 | -0.109\*\*\* |  |  |
|  |  |  | (0.028) | (0.029) |  |  |
| bk_early |  |  |  |  | 0.037 | 0.092\*\*\* |
|  |  |  |  |  | (0.025) | (0.026) |
| bk_late |  |  |  |  | 0.011 | -0.011 |
|  |  |  |  |  | (0.023) | (0.025) |
| Num.Obs. | 3634 | 3604 | 3634 | 3604 | 3634 | 3604 |
| R2 | 0.067 | 0.060 | 0.065 | 0.059 | 0.066 | 0.060 |
