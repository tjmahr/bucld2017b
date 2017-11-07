Understanding the effects of dialect familiarity on lexical processing 
efficiency in preschool children using the visual world paradigm
======================================================================

## Repository contents

### Analysis notebooks

`01-download-data.Rmd` ([output](./01-download-data.md)) downloads the raw data
from our lab's internal database. It cannot be reproduced by people outside our
lab. Therefore, I try to keep the script brief and have it save the needed data
into the folder `data-raw`. Later processing steps can be reproduced.

`02-screen-data.Rmd` ([output](./02-screen-data.md)) screens the raw 
eyetracking data, removing trials and participants with excessive missing data.

`03-eyetracking-data.Rmd` ([output](./03-eyetracking-data.md)) provides summary 
statistics about the participants and includes various plots of the eyetracking 
data from the experiment.

`04-gca-models.Rmd` ([output](./04-gca-models.md)) runs growth curve analyses
for the data.

### Directories

`data-raw/`: Raw data downloaded from the database.

`data/`: Screened data ready for plotting or modeling.

Large csv files are saved as compressed `.csv.gz` files. The readr package,
specifically the function `readr::read_csv()`, can automatically uncompress
these files.

`assets/`: The image files and model caches for the notebooks are stored here.

### Miscellany

TK TK TK poster file

`Makefile` is a makefile to automate running and rendering the notebook files.

The code's `LICENSE` is the GPL-3, but I don't think that should matter because 
the code is really tailored for this data-set. The
data is copyrighted and belongs to the University of Wisconsin–Madison, 
I think. (We collected data at two different university labs.) I don't know.


## Additional information

### Reproducibility tips

Don't hesitate to ask here with an issue or by emailing Tristan. These things 
can be finicky on other people's computers.

This code should install most of the needed packages. Otherwise, the 
session-info at the bottom of each script has the version and provenance of 
all R packages used.

```r
install.packages(c("knitr", "rmarkdown", "tidyverse", "lme4", 
                 "yaml", "polypoly", "devtools", "rprojroot",
                 "viridis", "hrbrthemes"))
devtools::install_github("tjmahr/littlelisteners")
```

The easiest way to reproduce the analysis is to clone the repository from GitHub
into an RStudio project. In RStudio: File > New Project \> Version Control > Git
\> paste in the URL of this repository. You can "Knit" the individual Rmd files
to run and compile the notebooks, or you can use the Build tab to run the 
Makefile which updates any files that need to be updated.


### Abstract from conference handbook

> Understanding the effects of dialect familiarity on lexical processing 
> efficiency in preschool children using the visual world paradigm
>
> _Michelle Erskine (University of Maryland)_ \
> _Tristan Mahr (University of Wisconsin–Madison)_ \
> _Jan Edwards (University of Maryland)_
>
> Successful language learning relies on children’s ability to
recognize familiar words quickly and accurately. Children
who speak a nonmainstream dialect at home experience the
added challenge of recognizing words in both their familiar
home dialect and the unfamiliar school dialect when they begin
school. This study used the visual world paradigm to evaluate
the effect of dialect familiarity on spoken word recognition in
young children who spoke either a nonmainstream dialect of
English, African American English (AAE), or a mainstream
dialect of English, General American English (GAE). Our
results suggest children, as early as age 4, have flexible
representations and can reliably adapt to some forms of
linguistic variation such as dialect. This result was consistently
observed for preschool children who were speakers of AAE,
who have some experience with GAE, as well as for children
who speak GAE, who have very little or no experience with
AAE.
