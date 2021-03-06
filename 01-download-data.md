Get raw data from the database
================
Tristan Mahr
2017-11-07

-   [Do a little setup](#do-a-little-setup)
-   [Find the eyetracking blocks](#find-the-eyetracking-blocks)
    -   [Determine which eyetracking blocks used which stimulus recordings](#determine-which-eyetracking-blocks-used-which-stimulus-recordings)
-   [Download block and child demographic information](#download-block-and-child-demographic-information)
-   [Download the gaze data](#download-the-gaze-data)

Do a little setup
-----------------

Create data directories if they don't exist yet.

``` r
library(dplyr, warn.conflicts = FALSE)
library(L2TDatabase)

# Work relative to RStudio project
wd <- rprojroot::find_rstudio_root_file()

# Create a directory to store the data
create_directory <- function(path) {
  if (!dir.exists(path)) {
    message("Creating directory: ", path)
    dir.create(path, showWarnings = FALSE)
  }
  invisible(path)
}

create_directory(file.path(wd, "data-raw"))
create_directory(file.path(wd, "data"))
```

Connect to the database.

``` r
# Try to find the file with L2T database information in user's home 
# folder or in this repository
find_database_config <- function() {
  home_config <- path.expand("~/l2t_db.cnf")
  repo_config <- rprojroot::find_rstudio_root_file("l2t_db.cnf")

  if (file.exists(home_config)) {
    home_config
  } else if (file.exists(repo_config)) {
    repo_config
  } else {
    stop("Cannot find `l2t_db.cnf` file")
  }
}

# Connect to the individual databases
l2t_main <- l2t_connect(find_database_config(), "l2t")
l2t_backend <- l2t_connect(find_database_config(), "backend")
l2t_eyetracking <- l2t_connect(find_database_config(), "eyetracking")
```

Find the eyetracking blocks
---------------------------

### Determine which eyetracking blocks used which stimulus recordings

JE says that we should only use the experiments that used the re-recorded stimuli for TimePoint2. We modified the stimuli at TimePoint2 so that the two dialect versions had similar durations.

These are the tables in the eyetracking database.

``` r
src_tbls(l2t_eyetracking)
#>  [1] "BlockAttributes"          "Blocks"                   "Looks"                   
#>  [4] "TrialAttributes"          "Trials"                   "q_BlockAttributesByStudy"
#>  [7] "q_BlocksByStudy"          "q_LooksByStudy"           "q_MissingDataByBlock"    
#> [10] "q_TrialAttributesByStudy" "q_TrialsByStudy"
```

Tables with `q_` at the front are queries which contain helpful information (like Study and ResearchID) alongside the main data in the table. We use the `q_TrialAttributesByStudy` table to get information about each eyetracking trial.

``` r
# Dialects used in each block
q_block_dialects <- l2t_eyetracking %>% 
  tbl("q_BlockAttributesByStudy") %>% 
  filter(
    Task == "RWL", 
    BlockAttribute_Name == "Dialect", 
    Study %in% c("DialectSwitch", "MaternalEd")) %>% 
  select(BlockID, BlockAttribute_Value) %>% 
  rename(Dialect = BlockAttribute_Value)

q_block_ages <- l2t_eyetracking %>% 
  tbl("Blocks") %>% 
  select(BlockID, Block_Age)

# Find the blocks that used TP2 stimuli
q_blocks_to_use <- l2t_eyetracking %>% 
  tbl("q_BlockAttributesByStudy") %>% 
  filter(
    Task == "RWL", 
    BlockAttribute_Name == "StimulusSet", 
    BlockAttribute_Value == "TP2",
    Study %in% c("DialectSwitch", "MaternalEd")) %>% 
  select(-BlockAttribute_Name) %>% 
  rename(StimulusSet = BlockAttribute_Value) %>% 
  left_join(q_block_dialects) %>% 
  left_join(q_block_ages)
```

Count the number of blocks in the dialect-varying studies.

``` r
q_blocks_to_use %>% 
  count(Study, Dialect, StimulusSet) %>% 
  rename(`Num Blocks` = n, `Stimulus Set` = StimulusSet) %>% 
  ungroup() %>% 
  collect() %>% 
  knitr::kable()
```

| Study         | Dialect | Stimulus Set |  Num Blocks|
|:--------------|:--------|:-------------|-----------:|
| DialectSwitch | AAE     | TP2          |          42|
| DialectSwitch | SAE     | TP2          |          41|
| MaternalEd    | AAE     | TP2          |          20|
| MaternalEd    | SAE     | TP2          |          19|

Download block and child demographic information
------------------------------------------------

Download child-level information.

``` r
df_child_info <- tbl(l2t_backend, "Child") %>% 
  left_join(tbl(l2t_backend, "ChildStudy")) %>% 
  left_join(tbl(l2t_backend, "Study")) %>% 
  filter(Study %in% c("DialectSwitch", "MaternalEd")) %>% 
  rename(ResearchID = ShortResearchID) %>% 
  collect() %>% 
  left_join(collect(tbl(l2t_main, "Maternal_Education"))) %>% 
  select(ChildID, ChildStudyID, Study, ResearchID, 
         Female, AAE, LateTalker, CImplant, 
         Maternal_Education, Maternal_Education_Level) 

# Add some helper columns
df_medu_scheme <- data_frame(
  Maternal_Education_Level = c(NA, 1:7),
  Maternal_Education_Group = c(NA, "Low", "Low", "Low", "Mid", 
                               "Mid", "High", "High")
)

df_child_info <- df_child_info %>% 
  left_join(df_medu_scheme) %>%
  mutate(
    Dialect = ifelse(AAE, "AAE", "MAE"),
    Gender = ifelse(Female, "Girl", "Boy")) 

df_child_info$Maternal_Education_Group <- factor(
  df_child_info$Maternal_Education_Group, 
  levels = c("Low", "Mid", "High")
)

df_evt <- tbl(l2t_main, "EVT") %>% 
  select(Study, ResearchID, EVT_Age:EVT_Standard) %>% 
  collect()

df_ppvt <- tbl(l2t_main, "PPVT") %>% 
  select(Study, ResearchID, PPVT_Age:PPVT_Standard) %>% 
  collect()

df_child_vars <- df_child_info %>% 
  select(Study, ResearchID, ChildStudyID, Female:Gender) %>% 
  distinct() %>% 
  left_join(df_evt) %>% 
  left_join(df_ppvt)
```

Attach research IDs and study names to the non-TP1 blocks.

``` r
df_rwl_blocks <- q_blocks_to_use %>% 
  rename(BlockDialect = Dialect) %>% 
  collect() %>% 
  left_join(df_child_info)
```

Now we count the number of dialects presented to each child to find out who received more than one version of the experiment.

``` r
df_children_who_got_multiple_dialects <- df_rwl_blocks %>% 
  select(ChildID, AAE, BlockDialect) %>% 
  distinct() %>% 
  # Count the dialects presented to each child. Find multiple dialect children
  count(AAE, ChildID) %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  select(ChildID)

df_blocks_to_keep <- df_rwl_blocks %>% 
  inner_join(df_children_who_got_multiple_dialects)
```

One child apparently got three blocks. Figure out which block to manually exclude.

``` r
q_blocks_to_use %>% 
  group_by(ResearchID) %>% 
  tally() %>% 
  collect() %>% 
  filter(n > 2) %>% 
  rename(`Num Blocks` = n)
#> # A tibble: 1 x 2
#>   ResearchID `Num Blocks`
#>        <chr>        <dbl>
#> 1       436D            3

q_blocks_to_use %>% 
  collect() %>% 
  filter(ResearchID == "436D")
#> # A tibble: 3 x 10
#>           Study ResearchID  Task  Version             Basename            DateTime BlockID
#>           <chr>      <chr> <chr>    <chr>                <chr>               <chr>   <int>
#> 1 DialectSwitch       436D   RWL Standard RWL_Block1_436D53FA2 2014-04-01 09:41:07    1229
#> 2 DialectSwitch       436D   RWL Standard RWL_Block1_436D53FS2 2014-03-28 09:50:09    1230
#> 3 DialectSwitch       436D   RWL Standard RWL_Block2_436D53FA1 2014-04-01 09:52:14    1231
#> # ... with 3 more variables: StimulusSet <chr>, Dialect <chr>, Block_Age <int>

# Skip the latest redundant block
df_blocks_to_keep <- df_blocks_to_keep %>% 
  filter(DateTime != "2014-04-01 09:52:14")

# Update the server-side query to use the restricted subset of blocks
q_blocks_to_use <- q_blocks_to_use %>% 
  filter(BlockID %in% df_blocks_to_keep$BlockID)
```

Download the gaze data
----------------------

Download the eyetracking data.

``` r
# Keep just the trials in the blocks we want
q_rwl_trials <- tbl(l2t_eyetracking, "Trials") %>% 
  semi_join(q_blocks_to_use)

# Download the gazes in those trials
df_raw_rwl_looks <- tbl(l2t_eyetracking, "Looks") %>% 
  inner_join(q_rwl_trials) %>% 
  select(TrialID, Time, GazeByImageAOI) %>% 
  collect(n = Inf) 

df_rwl_looks <- df_raw_rwl_looks %>% 
  inner_join(collect(q_rwl_trials)) %>% 
  inner_join(df_blocks_to_keep) %>% 
  rename(TrialNo = Trial_TrialNo) %>% 
  select(ChildID, ChildStudyID, BlockID, TrialID, 
         Study, ResearchID, ChildDialect = Dialect, 
         Version, Basename, StimulusSet, BlockDialect, 
         Block_Age, TrialNo, Time, GazeByImageAOI)
```

Save our work. The eyetracking data has `nrow(df_rwl_looks)` rows, so we save it in a compressed csv file. The readr package automatically compresses and uncompresses `csv.gz` files.

``` r
readr::write_csv(df_child_vars, file.path(wd, "data-raw", "child-info.csv"))
readr::write_csv(df_rwl_looks, file.path(wd, "data-raw", "looks.csv.gz"))
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
#>  package     * version    date       source                                     
#>  assertthat    0.2.0      2017-04-11 CRAN (R 3.3.2)                             
#>  backports     1.1.1      2017-09-25 CRAN (R 3.4.1)                             
#>  bindr         0.1        2016-11-13 CRAN (R 3.4.0)                             
#>  bindrcpp    * 0.2        2017-06-17 CRAN (R 3.4.0)                             
#>  clisymbols    1.2.0      2017-08-04 Github (gaborcsardi/clisymbols@e49b4f5)    
#>  DBI           0.7        2017-06-18 CRAN (R 3.4.0)                             
#>  dbplyr        1.1.0      2017-06-27 CRAN (R 3.4.1)                             
#>  digest        0.6.12     2017-01-27 CRAN (R 3.3.2)                             
#>  dplyr       * 0.7.4      2017-09-28 CRAN (R 3.4.2)                             
#>  evaluate      0.10.1     2017-06-24 CRAN (R 3.4.1)                             
#>  glue          1.2.0      2017-10-29 CRAN (R 3.4.2)                             
#>  highr         0.6        2016-05-09 CRAN (R 3.2.3)                             
#>  hms           0.3        2016-11-22 CRAN (R 3.3.2)                             
#>  htmltools     0.3.6      2017-04-28 CRAN (R 3.4.0)                             
#>  knitr       * 1.17       2017-08-10 CRAN (R 3.4.2)                             
#>  L2TDatabase * 0.1        2017-08-25 Github (LearningToTalk/L2TDatabase@18c957b)
#>  lubridate     1.7.1      2017-11-03 CRAN (R 3.4.2)                             
#>  magrittr      1.5        2014-11-22 CRAN (R 3.1.2)                             
#>  pkgconfig     2.0.1      2017-03-21 CRAN (R 3.3.3)                             
#>  R6            2.2.2      2017-06-17 CRAN (R 3.4.0)                             
#>  Rcpp          0.12.13    2017-09-28 CRAN (R 3.4.2)                             
#>  readr         1.1.1      2017-05-16 CRAN (R 3.4.0)                             
#>  rlang         0.1.4      2017-11-05 CRAN (R 3.4.2)                             
#>  rmarkdown     1.6        2017-06-15 CRAN (R 3.4.2)                             
#>  RMySQL        0.10.13    2017-08-14 CRAN (R 3.4.1)                             
#>  rprojroot     1.2        2017-01-16 CRAN (R 3.3.2)                             
#>  sessioninfo   1.0.1      2017-09-13 Github (r-lib/sessioninfo@e813de4)         
#>  stringi       1.1.5      2017-04-07 CRAN (R 3.3.3)                             
#>  stringr       1.2.0      2017-02-18 CRAN (R 3.3.2)                             
#>  tibble        1.3.4      2017-08-22 CRAN (R 3.4.1)                             
#>  withr         2.1.0.9000 2017-11-02 Github (jimhester/withr@8ba5e46)           
#>  yaml          2.1.14     2016-11-12 CRAN (R 3.4.2)
```
