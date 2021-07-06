# Taxonomic Harmonization

## Description

This repository contains the companion code and the associated shiny app `taxharmonizexplorer` of Grenié et al. (2021, in prep.). It contains the associated figures of the paper, the code to generate the ones made from code, as well as the workflow code.

## Running `taxharmonizexplorerr`

The companion shiny app has a hosted version on https://mgrenie.shinyapps.io/taxtool-selecter/
The development version of the shiny app can be run with the following command in RStudio:

```
shiny::runGitHub("Rekyt/taxo_harmonization", subdir = "taxharmonizexplorer")
```


## Citation

This repository accompanies the following article:

```
Grenié M., Berti E., Carvajal-Quintero J., Winter M., Sagouis A., Matching species names in biodiversity databases: database relationships, tools, pitfalls and best practices. In preparation. doi: XXXX/XXXX 2021.
```


## BioTIME harmonization

The script `harmonize.R` perform the two workflows (called respectivaly torino and bogota). Paths are still relative to my machine. 
Results from cleaning are saved in the folder biotime_results as csv. Two columns are present in the csv, the "parsed" is the input name as obtained using rgnparser on BioTIME species names. The second column is the match in the specific database.
*biotime_common.csv* is the original biotime names with the parsed version from rngparser and the class and phylum to which they belong obtained using rgbif. The last colmn "common" refer to how the high taxonomies are referred (e.g. "vascular plants" for "Trachephyta").
