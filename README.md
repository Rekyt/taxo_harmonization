# Taxonomic Harmonization code

This repository contains some code to extract information on taxonomic harmonization for a upcoming review.

## Organization

Here is the complete file list

```
+-- build_pkg_network.R                       
+-- data
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

The `data` folder holds a dumped copy of the table of taxonomic packages available as a Google Sheet.

The `figures` contains different figues made using draw.io (now diagrams.net) at different points of the projects.

The `notes.md` contains some notes taken from the first taxonomic harmonization workshop.

The `retrieve_pkgs.R` file tries to automatically retrieve taxonomic packages from CRAN and other sources automatically through some name matching. **/!\ NB: This is highly experimental and unfinished /!\**.