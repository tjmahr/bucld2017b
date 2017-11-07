# Use this as the command for running R scripts
rscriptv := Rscript --vanilla

database-results = \
	data-raw/child-info.csv \
	data-raw/looks.csv.gz

screening-results = \
	data/scores.csv \
	data/screened.csv.gz

all: 04-gca-models.md 03-eyetracking-data.md 02-screen-data.md 01-download-data.md

04-gca-models.md: 04-gca-models.Rmd data/modeling.csv
	$(rscriptv) -e 'rmarkdown::render("$<")'

03-eyetracking-data.md: 03-eyetracking-data.Rmd $(screening-results)
	$(rscriptv) -e 'rmarkdown::render("$<")'

02-screen-data.md: 02-screen-data.Rmd $(database-results)
	$(rscriptv) -e 'rmarkdown::render("$<")'

01-download-data.md: 01-download-data.Rmd
	$(rscriptv) -e 'rmarkdown::render("$<")'

data/modeling.csv: 03-eyetracking-data.md
$(screening-results): 02-screen-data.md
$(database-results): 01-download-data.md

check: $()
	$(rscriptv) -e 'list.files(pattern = "unnamed", recursive = TRUE, full.names = TRUE)'

clean:
	rm -f *.html;
	rm -f 0*.md;
	rm -f assets/figure/*.png;
