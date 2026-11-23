build_sample
================

Builds the variables in Section 2 of the paper and the estimation
samples. Run `initial.Rmd` first.

``` r
library(tidyverse)

df <- read_rds("../birthorder_wages/nlsy_clean.rds") |>
  mutate(birth_year = 1900 + birth_year)
```

## Subsamples and weights

The military subsample was dropped after 1984 and the supplemental
poor-white subsample after 1990, so neither has wages at ages 30-40.
Drop both and use the 1979 base weight (stored with 2 implied decimals).

``` r
df <- df |>
  filter(!sample_id %in% c(9, 12, 15:20)) |>
  mutate(wt = weight_1979 / 100)
```

## Family ID

Two respondents are siblings if they lived in the same 1979 household
and list each other as brother or sister (codes 6, 7). Family ID =
smallest respondent ID among the siblings.

``` r
sib_links <- df |>
  select(id, matches("^hh_yth\\d_(id|rel)$")) |>
  rename_with(~ str_replace(.x, "_id$", "_oid"), matches("^hh_yth\\d_id$")) |>
  pivot_longer(-id, names_to = c(NA, ".value"), names_pattern = "hh_yth(\\d)_(oid|rel)") |>
  filter(rel %in% 6:7)

df <- df |>
  left_join(summarise(sib_links, fam_id = min(id, oid), .by = id), by = "id") |>
  mutate(fam_id = coalesce(fam_id, id))
```

## Family size and birth order

From the 1979 sibling questions. Only children are coded birth order 1
and picked up by `only_child`. A few people report more older siblings
than total siblings; they count as missing family info.

``` r
df <- df |>
  mutate(fam_size = n_sibs_1979 + 1,
         birth_order = if_else(n_sibs_1979 == 0, 1, n_older_sibs_1979 + 1),
         only_child = as.integer(fam_size == 1),
         bo = factor(pmin(birth_order, 5), levels = 1:5, labels = c("1", "2", "3", "4", "5+")),
         fam_size_cat = factor(pmin(fam_size, 6), levels = 1:6, labels = c("1", "2", "3", "4", "5", "6+")),
         bk_index = birth_order / ((fam_size + 1) / 2),
         has_fam = !is.na(fam_size) & !is.na(birth_order) & birth_order <= fam_size)
```

## Twins

A family has twins if any member reports a twin/triplet (1993 roster or
1994 question), or two respondents in the family share a birth month and
year. Drop the whole family.

``` r
df <- df |>
  mutate(twin = multiple_birth_type %in% 1:2 |
           if_any(matches("^sib(\\d+|_youngest)_older$"), ~ .x %in% 3)) |>
  add_count(fam_id, birth_year, birth_month, name = "same_bday") |>
  mutate(twin_fam = any(twin | same_bday > 1), .by = fam_id)
```

## Wages

Hourly pay at job \#1, converted to 2016 dollars with the CPI-U (annual
average). Keep survey years where R was 30-40 and the real wage is
between 1 and 250, then average the log wage.

``` r
cpi <- tibble(
  year = c(1979:1994, seq(1996, 2016, 2)),
  cpi = c(72.6, 82.4, 90.9, 96.5, 99.6, 103.9, 107.6, 109.6, 113.6, 118.3, 124.0, 130.7,
          136.2, 140.3, 144.5, 148.2, 156.9, 163.0, 172.2, 179.9, 188.9, 201.6, 215.3,
          218.1, 229.6, 236.7, 240.0))

wages <- df |>
  select(id, birth_year, matches("^wage_\\d{4}$")) |>
  pivot_longer(matches("^wage_"), names_to = "year", names_prefix = "wage_",
               names_transform = as.integer, values_to = "wage") |>
  left_join(cpi, by = "year") |>
  mutate(age = year - birth_year,
         real_wage = wage * 240.0 / cpi) |>
  filter(between(age, 30, 40), between(real_wage, 1, 250)) |>
  summarise(ln_wage = mean(log(real_wage)), n_wage_years = n(), .by = id)

df <- df |>
  left_join(wages, by = "id") |>
  mutate(n_wage_years = replace_na(n_wage_years, 0))
```

## AFQT and education

Raw 1980 AFQT = arithmetic + word knowledge + paragraph comp + half of
numeric operations (tests with altered procedures are dropped). Everyone
was tested in 1980, so age at test is just birth year. Residualize on
birth-year dummies and standardize.

Education = highest grade completed reported at ages 25-40.

``` r
df <- df |>
  mutate(afqt_raw = if_else(asvab_altered == 67, NA,
                            asvab_arith + asvab_word + asvab_paragraph + asvab_numops / 2))

df$afqt <- as.numeric(scale(resid(lm(afqt_raw ~ factor(birth_year), df, na.action = na.exclude))))

educ <- df |>
  select(id, birth_year, matches("^educ_\\d{4}$")) |>
  pivot_longer(matches("^educ_"), names_to = "year", names_prefix = "educ_",
               names_transform = as.integer, values_to = "educ") |>
  filter(between(year - birth_year, 25, 40)) |>
  summarise(educ = max(educ, na.rm = T), .by = id) |>
  filter(is.finite(educ))
```

    ## Warning: There were 226 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `educ = max(educ, na.rm = T)`.
    ## ℹ In group 1: `id = 1`.
    ## Caused by warning in `max()`:
    ## ! no non-missing arguments to max; returning -Inf
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 225 remaining warnings.

``` r
df <- left_join(df, educ, by = "id")
```

## Controls

Professional/managerial = 1970 Census occupation codes 1-245 for the
adult woman or man in the household at 14 (no working adult counts as
0).

``` r
df <- df |>
  mutate(female = as.integer(sex == 2),
         race = factor(race, levels = c(3, 1, 2), labels = c("nbnh", "hispanic", "black")),
         byear = factor(birth_year),
         parent_profmgr = as.integer(adult_fem_occ_age14 %in% 1:245 | adult_male_occ_age14 %in% 1:245),
         home_learning = magazines_age14 + newspapers_age14 + libcard_age14)
```

## Samples

Sample sizes by sex at each restriction, to check how many women the
wage measure loses. We see that how I’ve defined wage does not lose a
huge mass of women in the sample.

``` r
df |>
  summarise(start = n(),
            family_info = sum(has_fam),
            no_twins = sum(has_fam & !twin_fam),
            any_wage = sum(has_fam & !twin_fam & n_wage_years >= 1),
            three_wages = sum(has_fam & !twin_fam & n_wage_years >= 3),
            .by = female)
```

    ## # A tibble: 2 × 6
    ##   female start family_info no_twins any_wage three_wages
    ##    <int> <int>       <int>    <int>    <int>       <int>
    ## 1      1  4926        4770     4636     4160        3727
    ## 2      0  4837        4672     4544     4133        3768

`main` is the cross-section sample (at least 3 wage years);
`n_wage_years >= 1` gives the robustness sample. `fe` keeps families
with at least two siblings in `main`.

``` r
analysis <- df |>
  filter(has_fam, !twin_fam, n_wage_years >= 1) |>
  mutate(main = n_wage_years >= 3,
         fe = main & sum(main) >= 2, .by = fam_id) |>
  select(id, fam_id, wt, ln_wage, n_wage_years, main, fe,
         fam_size, fam_size_cat, birth_order, bo, only_child, bk_index,
         afqt, educ, female, race, birth_year, byear, birth_month,
         mom_educ, dad_educ, parent_profmgr,
         magazines_age14, newspapers_age14, libcard_age14, home_learning)
```

Sibling FE sample: families, siblings, and families with mixed-sex
siblings. Also families where reported birth order doesn’t match the
siblings’ birth dates, or where siblings report different family sizes.

``` r
analysis |>
  filter(fe) |>
  arrange(fam_id, birth_year, birth_month) |>
  summarise(n = n(), mixed = n_distinct(female) > 1,
            bo_mismatch = any(diff(birth_order) <= 0, na.rm = T),
            size_mismatch = n_distinct(fam_size) > 1, .by = fam_id) |>
  summarise(families = n(), siblings = sum(n), mixed_sex = sum(mixed),
            bo_mismatch = sum(bo_mismatch), size_mismatch = sum(size_mismatch))
```

    ## # A tibble: 1 × 5
    ##   families siblings mixed_sex bo_mismatch size_mismatch
    ##      <int>    <int>     <int>       <int>         <int>
    ## 1     1529     3532       831         110           210

``` r
write_rds(analysis, "../birthorder_wages/analysis.rds")
```
