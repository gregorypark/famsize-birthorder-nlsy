ols_specs
================

``` r
library(tidyverse)
library(fixest)
library(modelsummary)

analysis <- read_rds("../birthorder_wages/analysis.rds") |>
  mutate(sex = if_else(female == 1, "women", "men"))
```

Specs 1-3 and the Booth-Kee extension. All run separately by sex
(`split = ~sex`), weighted with the custom weights, birth-year dummies
absorbed, SEs clustered by family. Firstborns are the omitted birth
order; only children are firstborns picked up by `only_child`.

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
s1a <- feols(ln_wage ~ fam_size + only_child | byear, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
s1b <- feols(ln_wage ~ i(bo, ref = "1") + only_child | byear, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`1a` = s1a, `1b` = s1b))
```

| term       | 1a men       | 1a women     | 1b men       | 1b women     |
|:-----------|:-------------|:-------------|:-------------|:-------------|
| fam_size   | -0.044\*\*\* | -0.033\*\*\* |              |              |
|            | (0.004)      | (0.004)      |              |              |
| only_child | -0.143\*\*   | -0.073       | -0.033       | -0.009       |
|            | (0.053)      | (0.053)      | (0.054)      | (0.054)      |
| bo = 2     |              |              | -0.022       | -0.022       |
|            |              |              | (0.027)      | (0.029)      |
| bo = 3     |              |              | -0.020       | -0.050       |
|            |              |              | (0.029)      | (0.032)      |
| bo = 4     |              |              | -0.027       | -0.102\*\*   |
|            |              |              | (0.032)      | (0.033)      |
| bo = 5+    |              |              | -0.175\*\*\* | -0.135\*\*\* |
|            |              |              | (0.030)      | (0.033)      |
| Num.Obs.   | 3768         | 3727         | 3768         | 3727         |
| R2         | 0.047        | 0.029        | 0.021        | 0.017        |

## Spec 2: Birth order and family size

Key test: does the family size coefficient shrink from 1a once birth
order is held fixed?

``` r
s2 <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child | byear, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`1a` = s1a, `2` = s2), coef_omit = "^(?!fam_size)")
```

| term     | 1a men       | 1a women     | 2 men        | 2 women      |
|:---------|:-------------|:-------------|:-------------|:-------------|
| fam_size | -0.044\*\*\* | -0.033\*\*\* | -0.051\*\*\* | -0.035\*\*\* |
|          | (0.004)      | (0.004)      | (0.006)      | (0.006)      |
| Num.Obs. | 3768         | 3727         | 3768         | 3727         |
| R2       | 0.047        | 0.029        | 0.050        | 0.030        |

``` r
reg_table(by_sex(`2` = s2))
```

| term       | 2 men        | 2 women      |
|:-----------|:-------------|:-------------|
| bo = 2     | -0.017       | -0.015       |
|            | (0.026)      | (0.029)      |
| bo = 3     | 0.021        | -0.012       |
|            | (0.029)      | (0.032)      |
| bo = 4     | 0.071\*      | -0.035       |
|            | (0.033)      | (0.034)      |
| bo = 5+    | 0.049        | 0.016        |
|            | (0.037)      | (0.041)      |
| fam_size   | -0.051\*\*\* | -0.035\*\*\* |
|            | (0.006)      | (0.006)      |
| only_child | -0.150\*\*   | -0.088       |
|            | (0.056)      | (0.056)      |
| Num.Obs.   | 3768         | 3727         |
| R2         | 0.050        | 0.030        |

Robustness (paper footnote): family size as dummies instead of linear.
Family size 1 is the omitted category, which is the same as
`only_child`, so `only_child` drops out.

``` r
s2_dum <- feols(ln_wage ~ i(bo, ref = "1") + i(fam_size_cat, ref = "1") | byear, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(`2 dummies` = s2_dum))
```

| term              | 2 dummies men | 2 dummies women |
|:------------------|:--------------|:----------------|
| bo = 2            | -0.013        | -0.014          |
|                   | (0.026)       | (0.029)         |
| bo = 3            | 0.037         | -0.012          |
|                   | (0.029)       | (0.034)         |
| bo = 4            | 0.089\*       | -0.020          |
|                   | (0.035)       | (0.036)         |
| bo = 5+           | 0.043         | 0.009           |
|                   | (0.037)       | (0.040)         |
| fam_size_cat = 2  | 0.119\*       | 0.049           |
|                   | (0.058)       | (0.059)         |
| fam_size_cat = 3  | 0.035         | 0.036           |
|                   | (0.056)       | (0.058)         |
| fam_size_cat = 4  | 0.008         | -0.027          |
|                   | (0.059)       | (0.059)         |
| fam_size_cat = 5  | -0.108+       | -0.058          |
|                   | (0.061)       | (0.061)         |
| fam_size_cat = 6+ | -0.202\*\*\*  | -0.152\*        |
|                   | (0.060)       | (0.061)         |
| Num.Obs.          | 3768          | 3727            |
| R2                | 0.048         | 0.029           |

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
               mom_educ_miss + dad_educ_miss + home_learning_miss | byear,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3b <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss | byear,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3c <- feols(ln_wage ~ fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss +
               afqt + educ + afqt_miss | byear,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3d <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child +
               race + mom_educ + dad_educ + parent_profmgr + home_learning +
               mom_educ_miss + dad_educ_miss + home_learning_miss +
               afqt + educ + afqt_miss | byear,
             m, split = ~sex, weights = ~wt, cluster = ~fam_id)
s3 <- by_sex(`3a` = s3a, `3b` = s3b, `3c` = s3c, `3d` = s3d)
```

Men:

``` r
reg_table(s3[str_ends(names(s3), " men")])
```

| term               | 3a men       | 3b men       | 3c men       | 3d men       |
|:-------------------|:-------------|:-------------|:-------------|:-------------|
| fam_size           | -0.012\*\*   | -0.017\*\*\* | -0.003       | -0.006       |
|                    | (0.004)      | (0.005)      | (0.004)      | (0.005)      |
| only_child         | -0.032       | -0.035       | -0.040       | -0.036       |
|                    | (0.049)      | (0.051)      | (0.046)      | (0.047)      |
| racehispanic       | 0.022        | 0.027        | 0.012        | 0.016        |
|                    | (0.026)      | (0.026)      | (0.026)      | (0.025)      |
| raceblack          | -0.177\*\*\* | -0.176\*\*\* | -0.100\*\*\* | -0.099\*\*\* |
|                    | (0.020)      | (0.020)      | (0.021)      | (0.021)      |
| mom_educ           | 0.016\*\*\*  | 0.017\*\*\*  | 0.004        | 0.004        |
|                    | (0.005)      | (0.005)      | (0.004)      | (0.004)      |
| dad_educ           | 0.017\*\*\*  | 0.017\*\*\*  | 0.005        | 0.005        |
|                    | (0.004)      | (0.004)      | (0.003)      | (0.003)      |
| parent_profmgr     | 0.107\*\*\*  | 0.107\*\*\*  | 0.041+       | 0.041+       |
|                    | (0.025)      | (0.025)      | (0.023)      | (0.023)      |
| home_learning      | 0.070\*\*\*  | 0.069\*\*\*  | 0.023\*      | 0.022\*      |
|                    | (0.010)      | (0.010)      | (0.010)      | (0.010)      |
| mom_educ_miss      | 0.086        | 0.093        | 0.026        | 0.034        |
|                    | (0.057)      | (0.058)      | (0.054)      | (0.054)      |
| dad_educ_miss      | 0.098\*      | 0.098\*      | 0.016        | 0.016        |
|                    | (0.045)      | (0.045)      | (0.042)      | (0.041)      |
| home_learning_miss | 0.024        | 0.025        | -0.057       | -0.055       |
|                    | (0.083)      | (0.083)      | (0.084)      | (0.084)      |
| bo = 2             |              | -0.012       |              | -0.005       |
|                    |              | (0.025)      |              | (0.023)      |
| bo = 3             |              | 0.011        |              | 0.020        |
|                    |              | (0.027)      |              | (0.025)      |
| bo = 4             |              | 0.075\*      |              | 0.084\*\*    |
|                    |              | (0.030)      |              | (0.028)      |
| bo = 5+            |              | 0.028        |              | 0.023        |
|                    |              | (0.035)      |              | (0.032)      |
| afqt               |              |              | 0.131\*\*\*  | 0.130\*\*\*  |
|                    |              |              | (0.012)      | (0.012)      |
| educ               |              |              | 0.044\*\*\*  | 0.044\*\*\*  |
|                    |              |              | (0.005)      | (0.005)      |
| afqt_miss          |              |              | 0.021        | 0.019        |
|                    |              |              | (0.041)      | (0.041)      |
| Num.Obs.           | 3768         | 3768         | 3768         | 3768         |
| R2                 | 0.183        | 0.186        | 0.297        | 0.300        |

Women:

``` r
reg_table(s3[str_ends(names(s3), "women")])
```

| term               | 3a women    | 3b women    | 3c women    | 3d women    |
|:-------------------|:------------|:------------|:------------|:------------|
| fam_size           | -0.010\*    | -0.012\*    | -0.002      | -0.006      |
|                    | (0.004)     | (0.006)     | (0.004)     | (0.005)     |
| only_child         | 0.005       | -0.006      | -0.023      | -0.028      |
|                    | (0.050)     | (0.053)     | (0.047)     | (0.049)     |
| racehispanic       | 0.178\*\*\* | 0.178\*\*\* | 0.167\*\*\* | 0.169\*\*\* |
|                    | (0.026)     | (0.027)     | (0.025)     | (0.025)     |
| raceblack          | 0.006       | 0.005       | 0.057\*\*   | 0.057\*\*   |
|                    | (0.020)     | (0.020)     | (0.021)     | (0.021)     |
| mom_educ           | 0.024\*\*\* | 0.024\*\*\* | 0.005       | 0.005       |
|                    | (0.005)     | (0.005)     | (0.005)     | (0.005)     |
| dad_educ           | 0.018\*\*\* | 0.018\*\*\* | 0.007\*     | 0.007\*     |
|                    | (0.004)     | (0.004)     | (0.003)     | (0.003)     |
| parent_profmgr     | 0.103\*\*\* | 0.103\*\*\* | 0.033       | 0.032       |
|                    | (0.027)     | (0.027)     | (0.024)     | (0.024)     |
| home_learning      | 0.071\*\*\* | 0.071\*\*\* | 0.021\*     | 0.020\*     |
|                    | (0.011)     | (0.011)     | (0.010)     | (0.010)     |
| mom_educ_miss      | 0.198\*\*   | 0.200\*\*   | 0.052       | 0.056       |
|                    | (0.066)     | (0.067)     | (0.065)     | (0.065)     |
| dad_educ_miss      | 0.141\*\*   | 0.141\*\*   | 0.072+      | 0.074+      |
|                    | (0.045)     | (0.045)     | (0.042)     | (0.042)     |
| home_learning_miss | -0.031      | -0.026      | -0.100      | -0.094      |
|                    | (0.073)     | (0.072)     | (0.069)     | (0.068)     |
| bo = 2             |             | 0.000       |             | 0.013       |
|                    |             | (0.027)     |             | (0.024)     |
| bo = 3             |             | -0.017      |             | -0.016      |
|                    |             | (0.031)     |             | (0.028)     |
| bo = 4             |             | -0.027      |             | 0.007       |
|                    |             | (0.033)     |             | (0.031)     |
| bo = 5+            |             | 0.030       |             | 0.046       |
|                    |             | (0.038)     |             | (0.036)     |
| afqt               |             |             | 0.139\*\*\* | 0.140\*\*\* |
|                    |             |             | (0.013)     | (0.013)     |
| educ               |             |             | 0.066\*\*\* | 0.066\*\*\* |
|                    |             |             | (0.005)     | (0.005)     |
| afqt_miss          |             |             | -0.070      | -0.073      |
|                    |             |             | (0.048)     | (0.048)     |
| Num.Obs.           | 3727        | 3727        | 3727        | 3727        |
| R2                 | 0.138       | 0.139       | 0.290       | 0.291       |

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
| men   | bo::2  | -0.012 | -0.005 |  0.007 |
| men   | bo::3  |  0.011 |  0.020 |  0.009 |
| men   | bo::4  |  0.075 |  0.084 |  0.009 |
| men   | bo::5+ |  0.028 |  0.023 | -0.004 |
| women | bo::2  |  0.000 |  0.013 |  0.013 |
| women | bo::3  | -0.017 | -0.016 |  0.002 |
| women | bo::4  | -0.027 |  0.007 |  0.034 |
| women | bo::5+ |  0.030 |  0.046 |  0.015 |

Check: Spec 2 on the full sample vs only those with every control and
AFQT. If the coefficients barely move, dropping the missing would not
have mattered much either.

``` r
cc <- drop_na(analysis, afqt, mom_educ, dad_educ, home_learning)

s2_cc <- feols(ln_wage ~ i(bo, ref = "1") + fam_size + only_child | byear, cc,
               split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(full = s2, `complete case` = s2_cc))
```

| term       | full men     | full women   | complete case men | complete case women |
|:-----------|:-------------|:-------------|:------------------|:--------------------|
| bo = 2     | -0.017       | -0.015       | -0.026            | -0.021              |
|            | (0.026)      | (0.029)      | (0.029)           | (0.032)             |
| bo = 3     | 0.021        | -0.012       | 0.009             | -0.031              |
|            | (0.029)      | (0.032)      | (0.031)           | (0.034)             |
| bo = 4     | 0.071\*      | -0.035       | 0.088\*           | -0.068+             |
|            | (0.033)      | (0.034)      | (0.036)           | (0.037)             |
| bo = 5+    | 0.049        | 0.016        | 0.052             | 0.018               |
|            | (0.037)      | (0.041)      | (0.042)           | (0.045)             |
| fam_size   | -0.051\*\*\* | -0.035\*\*\* | -0.052\*\*\*      | -0.034\*\*\*        |
|            | (0.006)      | (0.006)      | (0.006)           | (0.007)             |
| only_child | -0.150\*\*   | -0.088       | -0.158\*\*        | -0.068              |
|            | (0.056)      | (0.056)      | (0.061)           | (0.058)             |
| Num.Obs.   | 3768         | 3727         | 2959              | 2993                |
| R2         | 0.050        | 0.030        | 0.047             | 0.030               |

## Extension: Booth-Kee birth order index

Index = birth order / ((family size + 1) / 2), averages 1 within every
family size. Same controls as Spec 2.

``` r
ext <- feols(ln_wage ~ bk_index + fam_size + only_child | byear, analysis, split = ~sex, weights = ~wt, cluster = ~fam_id)
reg_table(by_sex(ext = ext))
```

| term       | ext men      | ext women    |
|:-----------|:-------------|:-------------|
| bk_index   | 0.057\*      | 0.004        |
|            | (0.022)      | (0.024)      |
| fam_size   | -0.046\*\*\* | -0.033\*\*\* |
|            | (0.004)      | (0.004)      |
| only_child | -0.143\*\*   | -0.073       |
|            | (0.053)      | (0.053)      |
| Num.Obs.   | 3768         | 3727         |
| R2         | 0.049        | 0.029        |
