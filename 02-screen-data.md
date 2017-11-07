Eyetracking data screening
================
Tristan Mahr
2017-11-07

-   [Setup](#setup)
-   [Find unreliable trials](#find-unreliable-trials)
-   [Find unreliable blocks](#find-unreliable-blocks)
-   [Data screening counts](#data-screening-counts)
    -   [Missing data stats](#missing-data-stats)
    -   [Clean up](#clean-up)

Setup
-----

Load the looks.

``` r
library(dplyr, warn.conflicts = FALSE)
library(littlelisteners)

wd <- rprojroot::find_rstudio_root_file()
looks <- readr::read_csv(file.path(wd, "data-raw", "looks.csv.gz"))
```

Data screening settings.

``` r
screening <- list(
  # determine amount of missing data between:
  min_time = -20,
  max_time = 2020,
  # remove trials with more than ... proportion of missing data
  max_na = .5,
  # blocks should have at least this many trials
  min_blocks = 12
)
```

Define a response code for the `aggregate_looks()` function.

``` r
resp_def <- create_response_def(
  primary = "Target",
  others = c("PhonologicalFoil", "SemanticFoil", "Unrelated"),
  elsewhere = "tracked",
  missing = NA
)  
```

Find unreliable trials
----------------------

Excessive missing data is defined as having more than 50% missing data between 0 and 2000 ms (relative to target onset). Determine the amount of missing data during this window for each trials.

``` r
trial_quality <- looks %>% 
  filter(screening$min_time <= Time, Time <= screening$max_time) 

range(trial_quality$Time)
#> [1]  -16.6546 2015.2100
      
trial_quality <- trial_quality %>% 
  aggregate_looks(resp_def, Study + ResearchID + ChildDialect + 
                    BlockDialect + TrialID ~ GazeByImageAOI) %>% 
  select(Study:TrialID, PropNA)
```

Count the number of bad trials that need to be excluded.

``` r
# Using UQ to unquote `screening$max_na` so that the created column is
# *not* named `PropNA > screening$max_na` but instead uses the value of
# `screening$max_na` in the column name.
trial_quality %>% 
  count(PropNA > UQ(screening$max_na)) %>% 
  rename(`Num Trials` = n) %>% 
  knitr::kable()
```

| PropNA &gt; 0.5 |  Num Trials|
|:----------------|-----------:|
| FALSE           |        2446|
| TRUE            |         338|

``` r

trial_quality %>% 
  count(ChildDialect, BlockDialect, PropNA > UQ(screening$max_na)) %>%  
  rename(`Num Trials` = n) %>% 
  knitr::kable()
```

| ChildDialect | BlockDialect | PropNA &gt; 0.5 |  Num Trials|
|:-------------|:-------------|:----------------|-----------:|
| AAE          | AAE          | FALSE           |         455|
| AAE          | AAE          | TRUE            |          49|
| AAE          | SAE          | FALSE           |         443|
| AAE          | SAE          | TRUE            |          61|
| MAE          | AAE          | FALSE           |         782|
| MAE          | AAE          | TRUE            |         106|
| MAE          | SAE          | FALSE           |         766|
| MAE          | SAE          | TRUE            |         122|

Remove the bad trials.

``` r
bad_trials <- trial_quality %>% 
  filter(PropNA >= screening$max_na)

looks_clean_trials <- anti_join(looks, bad_trials)
```

Find unreliable blocks
----------------------

Count the number of trials leftover in each block.

``` r
# Narrow down to one row per trial
trial_counts <- looks_clean_trials %>% 
  select(ChildID:ResearchID, BlockDialect, ChildDialect) %>% 
  distinct() %>% 
  # Count rows in each group
  count(ChildID, ChildStudyID, BlockID, 
        ResearchID, ChildDialect, BlockDialect) %>% 
  arrange(n) %>% 
  rename(`Num Trials in Block` = n)
```

We need to remove these children. They have too few trials in one of the blocks, so all their data should be removed.

``` r
children_to_drop <- trial_counts %>% 
  filter(`Num Trials in Block` < screening$min_blocks) 

children_to_drop %>% 
  knitr::kable()
```

|  ChildID|  ChildStudyID|  BlockID| ResearchID | ChildDialect | BlockDialect |  Num Trials in Block|
|--------:|-------------:|--------:|:-----------|:-------------|:-------------|--------------------:|
|      359|           879|     1294| 454D       | MAE          | AAE          |                    5|
|      360|           880|     1301| 455D       | MAE          | SAE          |                    7|

Remove the children.

``` r
looks_clean_blocks <- looks_clean_trials %>% 
  anti_join(children_to_drop %>% select(ChildID) ) 
```

Data screening counts
---------------------

Count the number of children and trials at each stage in data screening.

``` r
cleaning_progression <- list(
  `a. raw data` = looks,
  `b. drop bad trials` = looks_clean_trials, 
  `c. drop children w sparse blocks` = looks_clean_blocks) %>% 
  bind_rows(.id = "Stage")

cleaning_progression %>% 
  group_by(Stage) %>% 
  summarise(
    `Num Children` = n_distinct(ChildID),
    `Num Blocks` = n_distinct(BlockID),
    `Num Trials` = n_distinct(TrialID)) %>% 
  knitr::kable()
```

| Stage                            |  Num Children|  Num Blocks|  Num Trials|
|:---------------------------------|-------------:|-----------:|-----------:|
| a. raw data                      |            58|         116|        2784|
| b. drop bad trials               |            58|         116|        2446|
| c. drop children w sparse blocks |            56|         112|        2396|

``` r

cleaning_progression %>% 
  group_by(
    Stage, `Native Dialect` = ChildDialect) %>% 
  summarise(
    `Num Children` = n_distinct(ChildID),
    `Num Blocks` = n_distinct(BlockID),
    `Num Trials` = n_distinct(TrialID)) %>% 
  knitr::kable()
```

| Stage                            | Native Dialect |  Num Children|  Num Blocks|  Num Trials|
|:---------------------------------|:---------------|-------------:|-----------:|-----------:|
| a. raw data                      | AAE            |            21|          42|        1008|
| a. raw data                      | MAE            |            37|          74|        1776|
| b. drop bad trials               | AAE            |            21|          42|         898|
| b. drop bad trials               | MAE            |            37|          74|        1548|
| c. drop children w sparse blocks | AAE            |            21|          42|         898|
| c. drop children w sparse blocks | MAE            |            35|          70|        1498|

``` r

cleaning_progression %>% 
  group_by(
    Stage,
    `Native Dialect` = ChildDialect,
    `Block Dialect` = BlockDialect) %>% 
  summarise(
    `Num Children` = n_distinct(ChildID),
    `Num Blocks` = n_distinct(BlockID),
    `Num Trials` = n_distinct(TrialID)) %>% 
  knitr::kable()
```

| Stage                            | Native Dialect | Block Dialect |  Num Children|  Num Blocks|  Num Trials|
|:---------------------------------|:---------------|:--------------|-------------:|-----------:|-----------:|
| a. raw data                      | AAE            | AAE           |            21|          21|         504|
| a. raw data                      | AAE            | SAE           |            21|          21|         504|
| a. raw data                      | MAE            | AAE           |            37|          37|         888|
| a. raw data                      | MAE            | SAE           |            37|          37|         888|
| b. drop bad trials               | AAE            | AAE           |            21|          21|         455|
| b. drop bad trials               | AAE            | SAE           |            21|          21|         443|
| b. drop bad trials               | MAE            | AAE           |            37|          37|         782|
| b. drop bad trials               | MAE            | SAE           |            37|          37|         766|
| c. drop children w sparse blocks | AAE            | AAE           |            21|          21|         455|
| c. drop children w sparse blocks | AAE            | SAE           |            21|          21|         443|
| c. drop children w sparse blocks | MAE            | AAE           |            35|          35|         756|
| c. drop children w sparse blocks | MAE            | SAE           |            35|          35|         742|

### Missing data stats

Data quality stats for remaining children.

``` r
looks_clean_blocks %>%
  filter(between(Time, -20, 2020)) %>% 
  aggregate_looks(resp_def, ChildDialect + BlockDialect + 
                    ResearchID + TrialID ~ GazeByImageAOI) %>% 
  group_by(ChildDialect, BlockDialect, ResearchID) %>% 
  summarise(
    nGoodTrials = n(),
    Mean_Prop_NA = mean(PropNA)) %>% 
  summarise(
    `N Children` = n(), 
    `Total Useable Trials` = sum(nGoodTrials),
    `Mean N of Useable Trials` = mean(nGoodTrials) %>% round(1), 
    `SD Trials` = sd(nGoodTrials) %>% round(1),
    `Min Trials` = min(nGoodTrials),
    `Max Trials` = max(nGoodTrials),
    `Mean Prop of Missing Data` = mean(Mean_Prop_NA) %>% round(3), 
    `SD Prop Missing` = sd(Mean_Prop_NA) %>% round(3),
    `Min Prop Missing` = min(Mean_Prop_NA) %>% round(3),
    `Max Prop Missing` = max(Mean_Prop_NA) %>% round(3)) %>% 
  knitr::kable()
```

| ChildDialect | BlockDialect |  N Children|  Total Useable Trials|  Mean N of Useable Trials|  SD Trials|  Min Trials|  Max Trials|  Mean Prop of Missing Data|  SD Prop Missing|  Min Prop Missing|  Max Prop Missing|
|:-------------|:-------------|-----------:|---------------------:|-------------------------:|----------:|-----------:|-----------:|--------------------------:|----------------:|-----------------:|-----------------:|
| AAE          | AAE          |          21|                   455|                      21.7|        2.6|          15|          24|                      0.081|            0.048|             0.016|             0.190|
| AAE          | SAE          |          21|                   443|                      21.1|        2.9|          12|          24|                      0.086|            0.054|             0.024|             0.216|
| MAE          | AAE          |          35|                   756|                      21.6|        3.6|          12|          24|                      0.087|            0.066|             0.002|             0.278|
| MAE          | SAE          |          35|                   742|                      21.2|        3.2|          12|          24|                      0.082|            0.051|             0.011|             0.237|

### Clean up

Double check the eyetracking experiment versions.

``` r
looks_clean_blocks %>% 
  distinct(StimulusSet, Version)
#> # A tibble: 1 x 2
#>    Version StimulusSet
#>      <chr>       <chr>
#> 1 Standard         TP2
```

Save clean data.

``` r
looks_clean_blocks %>% 
  select(Study, ResearchID, Basename, ChildDialect, BlockDialect, Block_Age, 
         TrialNo, Time, GazeByImageAOI) %>% 
  readr::write_csv(file.path(wd, "data", "screened.csv.gz"))
```

Update participants data to only have children with eyetracking data.

``` r
readr::read_csv(file.path(wd, "data-raw", "child-info.csv")) %>% 
  semi_join(looks_clean_blocks) %>% 
  readr::write_csv(file.path(wd, "data", "scores.csv"))
```

------------------------------------------------------------------------

``` r
sessioninfo::session_info()
#> - Session info -----------------------------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.4.1 (2017-06-30)
#>  os       Windows 7 x64 SP 1          
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  tz       America/Chicago             
#>  date     2017-11-07                  
#> 
#> - Packages ---------------------------------------------------------------------------------------
#>  package         * version    date       source                                 
#>  assertthat        0.2.0      2017-04-11 CRAN (R 3.3.2)                         
#>  backports         1.1.1      2017-09-25 CRAN (R 3.4.1)                         
#>  bindr             0.1        2016-11-13 CRAN (R 3.4.0)                         
#>  bindrcpp        * 0.2        2017-06-17 CRAN (R 3.4.0)                         
#>  clisymbols        1.2.0      2017-08-04 Github (gaborcsardi/clisymbols@e49b4f5)
#>  digest            0.6.12     2017-01-27 CRAN (R 3.3.2)                         
#>  dplyr           * 0.7.4      2017-09-28 CRAN (R 3.4.2)                         
#>  evaluate          0.10.1     2017-06-24 CRAN (R 3.4.1)                         
#>  glue              1.2.0      2017-10-29 CRAN (R 3.4.2)                         
#>  highr             0.6        2016-05-09 CRAN (R 3.2.3)                         
#>  hms               0.3        2016-11-22 CRAN (R 3.3.2)                         
#>  htmltools         0.3.6      2017-04-28 CRAN (R 3.4.0)                         
#>  knitr           * 1.17       2017-08-10 CRAN (R 3.4.2)                         
#>  littlelisteners * 0.0.0.9000 2017-09-22 Github (tjmahr/littlelisteners@44e87a4)
#>  magrittr          1.5        2014-11-22 CRAN (R 3.1.2)                         
#>  pkgconfig         2.0.1      2017-03-21 CRAN (R 3.3.3)                         
#>  purrr             0.2.4      2017-10-18 CRAN (R 3.4.2)                         
#>  R6                2.2.2      2017-06-17 CRAN (R 3.4.0)                         
#>  Rcpp              0.12.13    2017-09-28 CRAN (R 3.4.2)                         
#>  readr             1.1.1      2017-05-16 CRAN (R 3.4.0)                         
#>  rlang             0.1.4      2017-11-05 CRAN (R 3.4.2)                         
#>  rmarkdown         1.6        2017-06-15 CRAN (R 3.4.2)                         
#>  rprojroot         1.2        2017-01-16 CRAN (R 3.3.2)                         
#>  sessioninfo       1.0.1      2017-09-13 Github (r-lib/sessioninfo@e813de4)     
#>  stringi           1.1.5      2017-04-07 CRAN (R 3.3.3)                         
#>  stringr           1.2.0      2017-02-18 CRAN (R 3.3.2)                         
#>  tibble            1.3.4      2017-08-22 CRAN (R 3.4.1)                         
#>  tidyr             0.7.2      2017-10-16 CRAN (R 3.4.2)                         
#>  tidyselect        0.2.3      2017-11-06 CRAN (R 3.4.2)                         
#>  withr             2.1.0.9000 2017-11-02 Github (jimhester/withr@8ba5e46)       
#>  yaml              2.1.14     2016-11-12 CRAN (R 3.4.2)
```
