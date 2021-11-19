# Taxonomic Harmonization

<!--Badges: starts-->
[![Code DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5121244.svg)](https://doi.org/10.5281/zenodo.5121244)
[![EcoevoRxiv Preprint](https://img.shields.io/badge/EcoEvoRxiv%20preprint-10.32942%2Fosf.io%2Fe3qnz-brightgreen)](https://doi.org/10.32942/osf.io/e3qnz)
<!--Badges: end-->

## Description

This repository contains the companion code and the associated shiny app `taxharmonizexplorer` of Grenié et al. (2021, in revision). It contains the associated figures of the paper, the code to generate the ones made from code, as well as the code to run the taxonomic harmonization workflows used in the paper.


## Citation

This repository accompanies the following article:

> Grenié, M., Berti, E., Carvajal-Quintero, J. D., Sagouis, A., Mona, G. M. L., & Winter, M. (2021, September 3). Harmonizing taxon names in biodiversity data: a review of tools, databases, and best practices. https://doi.org/10.32942/osf.io/e3qnz

Version of the code available in this repository have been archived on Zenodo: 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5121244.svg)](https://doi.org/10.5281/zenodo.5121244)

Refer to the above mentioned DOI to get the last up-to-date version or use the following citation:

> Matthias Grenié, Alban Sagouis, & Emilio Berti. (2021, July 22). Rekyt/taxo_harmonization: Submitted version (Version submitted). Zenodo. https://doi.org/10.5281/zenodo.5380285.


## Running `taxharmonizexplorer` (companion shiny app)

The shiny app is available at https://mgrenie.shinyapps.io/taxharmonizexplorer/

The development version of the shiny app can be run with the following command in RStudio:

```r
shiny::runGitHub("Rekyt/taxo_harmonization", subdir = "taxharmonizexplorer")
```

If you have a **local copy** of the repository you can run the app with:

```r
shiny::runApp("taxharmonizexplorer")
```


### Scripts used to build the network of tools

The scrips used to build the networks of tools are available in the `scripts` folder. They are numbered from `01` to `03` to be run sequentially. They all used the raw data available in the Excel sheet available in `data/data_raw/Table comparing taxonomic tools.xlsx`.


## Example harmonization workflows (BioTIME species data)

This repository also contains scripts to harmonize the taxonomy of [the BioTIME database](https://biotime.st-andrews.ac.uk/). We here present four different workflows on the raw taxonomy from BioTIME (available at `data/data_raw/biotime.txt`):

1. Workflow 1 (**Torino**): morking with higher taxonomic groups (standardize species names through higher taxonomic group assignation).
1. Workflow 2 (**Bogota**): matching the entire list of species on different taxon-specific databases.
1. Workflow 3 (**GBIF only with pre-processed**): pre-process the species names and then match the obtained list against GBIF.
1. Wrofklow 4 (**GBIF only no pre-processing**): match raw species list on GBIF.

Results from cleaning are saved in the folder `data/data_cleaned/biotime_results/` as csv files. Two columns are present in the csv, `parsed` is the input name as obtained using `rgnparser` on BioTIME species names; the second column is the named matched in a specific database.
`biotime_common.csv` is the original BioTIME names with the parsed version from `rgnparser` and the class and phylum to which they belong obtained using `rgbif`. The last column `common` refer to how the high taxonomies are referred (e.g. "vascular plants" for "Trachephyta").


### Running the workflow

You can run the entire workflow thanks to the `harmonize.R` script available in the *scripts* folder.
To run the workflow run the following code:

```{r}
source("scripts/harmonize.R")
```
