# Taxonomic Harmonization

## Description

This repository contains the companion code and the associated shiny app `taxtool-selecter` of Grenié et al. (2021, in prep.). It contains the associated figures of the paper, the code to generate the ones made from code, as well as the workflow code.

## Running `taxtool selecter`

The companion shiny app has a hosted version on https://mgrenie.shinyapps.io/taxtool-selecter/
The development version of the shiny app can be run with the following command in RStudio:

```
shiny::runGitHub("Rekyt/taxo_harmonization", subdir = "taxtool-selecter")
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


## File Organization

Here is the complete file list

```
.
+-- build_pkg_network.R
+-- data_cleaned
|   +-- all_pkgs_df.Rds
|   +-- db_igraph.Rds
|   \-- taxo_pkgs_igraph.Rds
+-- data_raw
|   \-- Table comparing taxonomic tools.xlsx
+-- figures
|   +-- db_relationships.drawio
|   +-- Different_types_of_databases.drawio
|   +-- Different_types_of_databases.png
|   +-- Harmonization_workflow.drawio
|   +-- Harmonization_workflow.png
|   +-- Packages_classification.drawio
|   +-- Packages_classification.png
|   \-- Relationships_between_taxo_dbs.png
+-- notes.md
+-- README.md
+-- retrieve_pkgs.R
\-- taxo_harmonization.Rproj
```

`build_pkg_network.R` contains the main script that aims to build a dependency network between taxonomic harmonization packages and taxonomic databases.

The `data_cleaned` folder holds a copy of some of the objects generated through the `build_pkg_network.R`.

The `data_raw` folder holds a dumped copy of the table of taxonomic packages available as a Google Sheet.

The `figures` contains different figues made using draw.io (now diagrams.net) at different points of the projects.

The `notes.md` contains some notes taken from the first taxonomic harmonization workshop.

The `retrieve_pkgs.R` file tries to automatically retrieve taxonomic packages from CRAN and other sources automatically through some name matching. **/!\ NB: This is highly experimental and unfinished /!\**.
