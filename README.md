# bucld2017b

## contents

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


