summary_stats
================

``` r
library(tidyverse)

analysis <- read_rds("../birthorder_wages/analysis.rds") |>
  mutate(sex = if_else(female == 1, "women", "men"))
full <- read_rds("../birthorder_wages/sample_full.rds") |>
  mutate(sex = if_else(female == 1, "women", "men"))
```

Means, SDs, and shares use the custom weights. Counts are unweighted.

## Who each restriction drops

Same restrictions as `build_sample`, applied in order, split by sex and
birth order. Respondents without usable family info have no valid birth
order, so they show up as `missing` and all leave at `family_info`.
Weights only exist for the final sample, so these are raw counts.

``` r
flow <- full |>
  mutate(bo = fct_na_value_to_level(if_else(has_fam, bo, NA), "missing"),
         start = TRUE,
         family_info = has_fam,
         no_twins = family_info & !twin_fam,
         any_wage = no_twins & n_wage_years >= 1,
         three_wages = no_twins & n_wage_years >= 3) |>
  pivot_longer(start:three_wages, names_to = "step", values_to = "kept") |>
  mutate(step = fct_inorder(step)) |>
  summarise(n = sum(kept), .by = c(sex, bo, step)) |>
  arrange(sex, bo, step)
```

Remaining at each step:

``` r
flow |>
  pivot_wider(names_from = step, values_from = n) |>
  knitr::kable()
```

| sex   | bo      | start | family_info | no_twins | any_wage | three_wages |
|:------|:--------|------:|------------:|---------:|---------:|------------:|
| men   | 1       |  1088 |        1088 |     1076 |      996 |         912 |
| men   | 2       |  1119 |        1119 |     1098 |      977 |         894 |
| men   | 3       |   913 |         913 |      888 |      814 |         748 |
| men   | 4       |   600 |         600 |      577 |      525 |         474 |
| men   | 5+      |   952 |         952 |      905 |      821 |         740 |
| men   | missing |   165 |           0 |        0 |        0 |           0 |
| women | 1       |  1143 |        1143 |     1125 |     1031 |         916 |
| women | 2       |  1131 |        1131 |     1102 |      995 |         905 |
| women | 3       |   842 |         842 |      820 |      730 |         669 |
| women | 4       |   633 |         633 |      615 |      556 |         499 |
| women | 5+      |  1021 |        1021 |      974 |      848 |         738 |
| women | missing |   156 |           0 |        0 |        0 |           0 |

Dropped at each step:

``` r
flow |>
  mutate(dropped = lag(n) - n, .by = c(sex, bo)) |>
  filter(step != "start") |>
  select(-n) |>
  pivot_wider(names_from = step, values_from = dropped) |>
  knitr::kable()
```

| sex   | bo      | family_info | no_twins | any_wage | three_wages |
|:------|:--------|------------:|---------:|---------:|------------:|
| men   | 1       |           0 |       12 |       80 |          84 |
| men   | 2       |           0 |       21 |      121 |          83 |
| men   | 3       |           0 |       25 |       74 |          66 |
| men   | 4       |           0 |       23 |       52 |          51 |
| men   | 5+      |           0 |       47 |       84 |          81 |
| men   | missing |         165 |        0 |        0 |           0 |
| women | 1       |           0 |       18 |       94 |         115 |
| women | 2       |           0 |       29 |      107 |          90 |
| women | 3       |           0 |       22 |       90 |          61 |
| women | 4       |           0 |       18 |       59 |          57 |
| women | 5+      |           0 |       47 |      126 |         110 |
| women | missing |         156 |        0 |        0 |           0 |

Share of those with family info who make it to the analysis sample, to
check whether attrition differs by birth order:

``` r
flow |>
  filter(bo != "missing", step %in% c("family_info", "three_wages")) |>
  pivot_wider(names_from = step, values_from = n) |>
  mutate(kept = three_wages / family_info) |>
  select(sex, bo, kept) |>
  pivot_wider(names_from = sex, values_from = kept) |>
  knitr::kable(digits = 3)
```

| bo  |   men | women |
|:----|------:|------:|
| 1   | 0.838 | 0.801 |
| 2   | 0.799 | 0.800 |
| 3   | 0.819 | 0.795 |
| 4   | 0.790 | 0.788 |
| 5+  | 0.777 | 0.723 |

## Descriptive statistics

Analysis sample by sex: weighted mean and SD, unweighted N. Wage is the
geometric mean real hourly wage (2016 dollars), i.e. `exp(ln_wage)`.

``` r
analysis |>
  mutate(wage = exp(ln_wage),
         black = as.integer(race == "black"),
         hispanic = as.integer(race == "hispanic")) |>
  select(sex, wt, ln_wage, wage, n_wage_years, fam_size, birth_order, only_child,
         afqt, educ, black, hispanic, birth_year,
         mom_educ, dad_educ, parent_profmgr, home_learning) |>
  pivot_longer(-c(sex, wt)) |>
  filter(!is.na(value)) |>
  summarise(mean = weighted.mean(value, wt),
            sd = sqrt(sum(wt * (value - mean)^2) / sum(wt)),
            n = n(),
            .by = c(name, sex)) |>
  pivot_wider(names_from = sex, values_from = c(mean, sd, n)) |>
  knitr::kable(digits = 2)
```

| name           | mean_women | mean_men | sd_women | sd_men | n_women | n_men |
|:---------------|-----------:|---------:|---------:|-------:|--------:|------:|
| ln_wage        |       2.71 |     2.99 |     0.51 |   0.49 |    3727 |  3768 |
| wage           |      17.19 |    22.44 |     9.57 |  12.31 |    3727 |  3768 |
| n_wage_years   |       6.37 |     6.66 |     1.71 |   1.60 |    3727 |  3768 |
| fam_size       |       4.37 |     4.27 |     2.30 |   2.31 |    3727 |  3768 |
| birth_order    |       2.90 |     2.87 |     1.93 |   1.94 |    3727 |  3768 |
| only_child     |       0.03 |     0.04 |     0.18 |   0.19 |    3727 |  3768 |
| afqt           |       0.34 |     0.30 |     0.86 |   0.97 |    3593 |  3583 |
| educ           |      13.59 |    13.33 |     2.40 |   2.58 |    3727 |  3768 |
| black          |       0.14 |     0.13 |     0.35 |   0.34 |    3727 |  3768 |
| hispanic       |       0.06 |     0.06 |     0.24 |   0.24 |    3727 |  3768 |
| birth_year     |    1960.44 |  1960.53 |     2.28 |   2.27 |    3727 |  3768 |
| mom_educ       |      11.52 |    11.65 |     2.74 |   2.77 |    3533 |  3493 |
| dad_educ       |      11.71 |    11.87 |     3.48 |   3.65 |    3194 |  3238 |
| parent_profmgr |       0.27 |     0.29 |     0.44 |   0.45 |    3727 |  3768 |
| home_learning  |       2.26 |     2.24 |     0.91 |   0.91 |    3702 |  3734 |

## Birth order and family size

Birth order distribution by sex: unweighted N, weighted share.

``` r
analysis |>
  summarise(n = n(), w = sum(wt), .by = c(sex, bo)) |>
  mutate(share = w / sum(w), .by = sex) |>
  select(-w) |>
  arrange(sex, bo) |>
  pivot_wider(names_from = sex, values_from = c(n, share)) |>
  knitr::kable(digits = 3)
```

| bo  | n_men | n_women | share_men | share_women |
|:----|------:|--------:|----------:|------------:|
| 1   |   912 |     916 |     0.259 |       0.253 |
| 2   |   894 |     905 |     0.257 |       0.259 |
| 3   |   748 |     669 |     0.209 |       0.197 |
| 4   |   474 |     499 |     0.118 |       0.135 |
| 5+  |   740 |     738 |     0.156 |       0.157 |

Family size by birth order (pooled, raw counts). Birth order is only
separately identified from family size because each family size has
several birth orders in it, so this shows how thin the cells get.

``` r
analysis |>
  count(fam_size_cat, bo) |>
  pivot_wider(names_from = bo, values_from = n, values_fill = 0) |>
  knitr::kable()
```

| fam_size_cat |   1 |   2 |   3 |   4 |   5+ |
|:-------------|----:|----:|----:|----:|-----:|
| 1            | 257 |   0 |   0 |   0 |    0 |
| 2            | 484 | 479 |   0 |   0 |    0 |
| 3            | 482 | 553 | 515 |   0 |    0 |
| 4            | 300 | 356 | 377 | 358 |    0 |
| 5            | 154 | 188 | 238 | 245 |  202 |
| 6+           | 151 | 223 | 287 | 370 | 1276 |

## Means by birth order

Weighted means by birth order and sex, before any controls. Later-borns
come from bigger families, so this mixes birth order and family size.

``` r
analysis |>
  summarise(n = n(),
            ln_wage = weighted.mean(ln_wage, wt),
            afqt = weighted.mean(afqt, wt, na.rm = T),
            educ = weighted.mean(educ, wt),
            fam_size = weighted.mean(fam_size, wt),
            .by = c(sex, bo)) |>
  arrange(sex, bo) |>
  knitr::kable(digits = 2)
```

| sex   | bo  |   n | ln_wage |  afqt |  educ | fam_size |
|:------|:----|----:|--------:|------:|------:|---------:|
| men   | 1   | 912 |    3.02 |  0.46 | 13.74 |     2.92 |
| men   | 2   | 894 |    3.01 |  0.40 | 13.62 |     3.37 |
| men   | 3   | 748 |    3.01 |  0.35 | 13.43 |     4.06 |
| men   | 4   | 474 |    3.01 |  0.20 | 12.84 |     5.17 |
| men   | 5+  | 740 |    2.86 | -0.09 | 12.44 |     7.63 |
| women | 1   | 916 |    2.76 |  0.48 | 13.97 |     2.94 |
| women | 2   | 905 |    2.74 |  0.39 | 13.77 |     3.43 |
| women | 3   | 669 |    2.71 |  0.43 | 13.70 |     4.36 |
| women | 4   | 499 |    2.66 |  0.21 | 13.23 |     5.14 |
| women | 5+  | 738 |    2.63 |  0.01 | 12.88 |     7.56 |

## Missing controls

Controls missing in the analysis sample, and how many have every control
(the sample for the full-controls specification).

``` r
analysis |>
  summarise(n = n(),
            complete = sum(!is.na(afqt) & !is.na(mom_educ) & !is.na(dad_educ) & !is.na(home_learning)),
            afqt = sum(is.na(afqt)),
            mom_educ = sum(is.na(mom_educ)),
            dad_educ = sum(is.na(dad_educ)),
            home_learning = sum(is.na(home_learning)),
            .by = sex) |>
  knitr::kable()
```

| sex   |    n | complete | afqt | mom_educ | dad_educ | home_learning |
|:------|-----:|---------:|-----:|---------:|---------:|--------------:|
| women | 3727 |     2993 |  134 |      194 |      533 |            25 |
| men   | 3768 |     2959 |  185 |      275 |      530 |            34 |

## Birth cohorts

``` r
analysis |>
  count(birth_year, sex) |>
  pivot_wider(names_from = sex, values_from = n) |>
  knitr::kable()
```

| birth_year | men | women |
|-----------:|----:|------:|
|       1957 | 368 |   432 |
|       1958 | 413 |   431 |
|       1959 | 405 |   448 |
|       1960 | 528 |   501 |
|       1961 | 531 |   509 |
|       1962 | 553 |   529 |
|       1963 | 537 |   491 |
|       1964 | 433 |   386 |

# Specification 4: Sibling FE sample

Families with at least two siblings in the analysis sample. Spec 4 is
unweighted, so everything here is unweighted.

``` r
fe <- filter(analysis, fe)
```

Families, siblings, and families with mixed-sex siblings (these identify
the female main effect).

``` r
fe |>
  summarise(n = n(), mixed = n_distinct(female) > 1, .by = fam_id) |>
  summarise(families = n(), siblings = sum(n), mixed_sex = sum(mixed)) |>
  knitr::kable()
```

| families | siblings | mixed_sex |
|---------:|---------:|----------:|
|     1529 |     3532 |       831 |

Siblings per family.

``` r
fe |>
  count(fam_id) |>
  count(n, name = "families") |>
  rename(siblings = n) |>
  knitr::kable()
```

| siblings | families |
|---------:|---------:|
|        2 |     1136 |
|        3 |      323 |
|        4 |       60 |
|        5 |        9 |
|        6 |        1 |

Birth order by sex in the FE sample. Every birth order needs enough
siblings of each sex for the birth order × female interactions.

``` r
fe |>
  count(bo, sex) |>
  pivot_wider(names_from = sex, values_from = n) |>
  knitr::kable()
```

| bo  | men | women |
|:----|----:|------:|
| 1   | 327 |   306 |
| 2   | 491 |   426 |
| 3   | 393 |   306 |
| 4   | 245 |   253 |
| 5+  | 410 |   375 |

Unweighted means for the FE sample next to the full analysis sample. The
FE sample should look similar apart from coming from bigger families.

``` r
analysis |>
  mutate(black = as.integer(race == "black"),
         hispanic = as.integer(race == "hispanic"),
         sample = if_else(fe, "fe", "non_fe")) |>
  bind_rows(mutate(analysis, sample = "all",
                   black = as.integer(race == "black"),
                   hispanic = as.integer(race == "hispanic"))) |>
  select(sample, ln_wage, fam_size, birth_order, afqt, educ, female, black, hispanic,
         birth_year, mom_educ, dad_educ, parent_profmgr, home_learning) |>
  pivot_longer(-sample) |>
  summarise(mean = mean(value, na.rm = T), .by = c(name, sample)) |>
  pivot_wider(names_from = sample, values_from = mean) |>
  select(name, all, fe, non_fe) |>
  knitr::kable(digits = 2)
```

| name           |     all |      fe |  non_fe |
|:---------------|--------:|--------:|--------:|
| ln_wage        |    2.79 |    2.80 |    2.78 |
| fam_size       |    4.81 |    5.31 |    4.37 |
| birth_order    |    3.12 |    3.32 |    2.93 |
| afqt           |    0.04 |    0.02 |    0.06 |
| educ           |   13.21 |   13.28 |   13.15 |
| female         |    0.50 |    0.47 |    0.52 |
| black          |    0.29 |    0.32 |    0.27 |
| hispanic       |    0.19 |    0.18 |    0.20 |
| birth_year     | 1960.62 | 1960.98 | 1960.29 |
| mom_educ       |   10.84 |   10.87 |   10.80 |
| dad_educ       |   10.93 |   10.90 |   10.95 |
| parent_profmgr |    0.22 |    0.22 |    0.21 |
| home_learning  |    2.02 |    2.04 |    2.01 |

Gap between oldest and youngest sibling respondent’s birth year within
each family. All siblings were born 1957-1964, so gaps are at most 7
years, and birth year and birth order move closely together within a
family.

``` r
fe |>
  summarise(gap = max(birth_year) - min(birth_year), .by = fam_id) |>
  count(gap, name = "families") |>
  knitr::kable()
```

| gap | families |
|----:|---------:|
|   0 |        7 |
|   1 |      305 |
|   2 |      412 |
|   3 |      345 |
|   4 |      234 |
|   5 |      121 |
|   6 |       88 |
|   7 |       17 |

``` r
cor(fe$birth_order - ave(fe$birth_order, fe$fam_id),
    fe$birth_year - ave(fe$birth_year, fe$fam_id))
```

    ## [1] 0.7811892

Families where reported birth order doesn’t match the siblings’ birth
dates, or where siblings report different family sizes.

``` r
fe |>
  arrange(fam_id, birth_year, birth_month) |>
  summarise(bo_mismatch = any(diff(birth_order) <= 0, na.rm = T),
            size_mismatch = n_distinct(fam_size) > 1, .by = fam_id) |>
  summarise(bo_mismatch = sum(bo_mismatch), size_mismatch = sum(size_mismatch)) |>
  knitr::kable()
```

| bo_mismatch | size_mismatch |
|------------:|--------------:|
|         110 |           210 |
