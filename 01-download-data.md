Get raw data from the database
================
Tristan Mahr
2017-10-17

Do a little setup
-----------------

Create data directories.

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

Connect to database

``` r
# Try to find the file with L2T database information in user's home folder or in
# this repository
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

### Determine which blocks of RWL used which stimuli

JE says that we should only use the experiments that used the re-recorded stimuli for TimePoint2. We modified the stimuli at TimePoint2 so that the two dialect versions had similar durations.

These are the tables in the eyetracking database.

``` r
src_tbls(l2t_eyetracking)
#>  [1] "BlockAttributes"          "Blocks"                  
#>  [3] "Looks"                    "TrialAttributes"         
#>  [5] "Trials"                   "q_BlockAttributesByStudy"
#>  [7] "q_BlocksByStudy"          "q_LooksByStudy"          
#>  [9] "q_MissingDataByBlock"     "q_TrialAttributesByStudy"
#> [11] "q_TrialsByStudy"
```

Tables with `q_` at the front are queries which contain helpful information (like Study and ResearchID) alongside the main data in the table. We use the `q_TrialAttributesByStudy` table to get information about each eyetracking trial. I use `q_` for queries that developed in these scripts as well.

``` r
# Dialects used in each block
q_block_dialects <- l2t_eyetracking %>% 
  tbl("q_BlockAttributesByStudy") %>% 
  filter(
    Task == "RWL", 
    BlockAttribute_Name == "Dialect", 
    Study %in% c("DialectSwitch", "MaternalEd")) %>% 
  select(BlockID, Dialect = BlockAttribute_Value)

q_block_ages <- l2t_eyetracking %>% 
  tbl("Blocks") %>% 
  select(BlockID, Block_Age)

# Find the blocks that used TP2 stimuli.
q_blocks_to_use <- l2t_eyetracking %>% 
  tbl("q_BlockAttributesByStudy") %>% 
  filter(
    Task == "RWL", 
    BlockAttribute_Name == "StimulusSet", 
    BlockAttribute_Value == "TP2",
    Study %in% c("DialectSwitch", "MaternalEd")) %>% 
  select(-BlockAttribute_Name, StimulusSet = BlockAttribute_Value) %>% 
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
  knitr::kable(caption = "Number of available blocks before data screening")
```

| Study         | Dialect | Stimulus Set |  Num Blocks|
|:--------------|:--------|:-------------|-----------:|
| DialectSwitch | AAE     | TP2          |          42|
| DialectSwitch | SAE     | TP2          |          41|
| MaternalEd    | AAE     | TP2          |          20|
| MaternalEd    | SAE     | TP2          |          19|
