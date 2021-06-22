# Taxonomic organisation
## Authors

This app is a common effort of:
Emilio Berti, Juan Carvajal, Matthias Grenié, Alban Sagouis and Marten Winter, all members of iDiv.

## Description

The taxonomy harmonization project aims at 
* illustrating the complexity of taxonomy related R packages and databases
* providing insight and tools to navigate among them
* compare the rationale and efficiency of various harmonisation workflows

The Shiny app presented here illustrate the links between packages and databases.

### Packages
In R, packages can be used to correct species names, check their existence, 
spelling or validity against online taxonomic databases, or to built local taxonomic 
databases. Some of these packages depend on each other for specific tasks: 
taxize depends on many API packages to access online resources.  

The Package network interactively shows these links.


### Databases
Numerous online databases allow users to improve the quality and inter-operability 
of their ecological data. Some databases are restricted to a specific taxonomic 
group while other cover several. Some databases are restricted to a country or 
a continent while other are global.  

Often, the global databases are aggregates of several smaller databases and this 
app helps navigating these links.


## Source code
The code is openly stored here: https://github.com/Rekyt/taxo_harmonization/ and the app can easily be ran with this command:  
shiny::runGitHub("Rekyt/taxo_harmonization")

## License
Open