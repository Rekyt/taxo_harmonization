# Taxonomic Harmonization

<!--Badges: starts-->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5121244.svg)](https://doi.org/10.5281/zenodo.5121244)
<!--Badges: end-->


## Description

This repository contains the companion code and the associated shiny app `taxharmonizexplorer` of Grenié et al. (2021, in prep.). It contains the associated figures of the paper, the code to generate the ones made from code, as well as the workflow code.

## Running `taxharmonizexplorerr` (companion shiny app)

The shiny app is hosted on [shinyapps.io](https://shinyapps.io) on https://mgrenie.shinyapps.io/taxharmonizexplorer/

The development version of the shiny app can be run with the following command in RStudio:

```r
shiny::runGitHub("Rekyt/taxo_harmonization", subdir = "taxharmonizexplorer")
```

If you have a **local copy** of the repository you can run the app with:

```r
shiny::runApp("taxharmonizexplorer")
```

## Harmonizing BioTIME species data

This repository also contains the script `harmonize.R` perform two different workflows (called respectively "Torino" and "Bogota"") to harmonize the taxonomy of BioTIME. Results from cleaning are saved in the folder `biotime_results/` as csv files. Two columns are present in the csv, `parsed` is the input name as obtained using `rgnparser` on BioTIME species names; the second column is the named matched in a specific database.
`biotime_common.csv` is the original BioTIME names with the parsed version from `rngparser` and the class and phylum to which they belong obtained using `rgbif`. The last column `common` refer to how the high taxonomies are referred (e.g. "vascular plants" for "Trachephyta").


## Citation

This repository accompanies the following article:

> Grenié M., Berti E., Carvajal-Quintero J., Winter M., Sagouis A., Matching species names in biodiversity databases: database relationships, tools, pitfalls and best practices. In preparation. doi: XXXX/XXXX 2021.

Version of the code available in this repository have been archived on Zenodo: 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5121244.svg)](https://doi.org/10.5281/zenodo.5121244)

Refer to the above mentioned DOI to get the last up-to-date version or use the following citation:

> Matthias Grenié, Alban Sagouis, & Emilio Berti. (2021, July 22). Rekyt/taxo_harmonization: First release (Version v0.1). Zenodo. http://doi.org/10.5281/zenodo.5121245.

