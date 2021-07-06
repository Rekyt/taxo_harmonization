# `taxharmonizexplorer` - Navigate the Taxonomic Harmonization landscape

## Description

Taxonomic harmonization is the needed step when merging dataset that were matched against different taxonomies. It consists in homogenizing the list of species names from all datasets against a common reference. However, understanding what tools can be used to do can be cumbersome for users.
`taxharmonizexplorer` has been developed to help users of taxonomic information better understand the links that exist between taxonomic reference databases, R packages that manipulate taxonomy, and both of them.

You can access a view of the full network of tools in the <a onclick="customHref(&#39;network&#39;)">next tab</a> available on the top of this page called <a onclick="customHref(&#39;network&#39;)">"Network"</a>.

You can get guidance in how to read the Network panel in the <a onclick="customHref(&#39;help&#39;)">"Help" tab</a>.

## Running the app

The app can be used through [shinyapps.io](https://shinyapps.io) at the following address: https://mgrenie.shinyapps.io/taxharmonizexplorer/.

The code and data of the app is stored at (https://github.com/Rekyt/taxo_harmonization/).
The shiny app can also be run using the following command.

```r
shiny::runGitHub("Rekyt/taxo_harmonization", subdir = "taxharmonizexplorer")
```


## Data used

The data used in the network presented in the next tab has been manually curated. Please refer to the [companion article](https://doi.org/XXXX) for more details on data acquisition.

### Taxonomic R Packages

The R packages referenced in the network only concern package in relationship with taxonomic information. The list was obtained to query from CRAN, GitHub, and manual additions. The dependency network was obtained through the [`pkgdepends` package](https://cran.r-project.org/package=pkgdepends).

### Taxonomic Reference Databases

The databases depicted in the network are the ones accessed by the referenced packages. The list of included databases is by no mean exhaustive but indicates the potential links between them.

Bigger taxonomic reference databases often aggregates smaller and specific databases. The links shown in the network between databases represent the reliance of a given database onto another one. The links have been manually curated are only few databases provide the full list of sources they use when relying on other databases. Thus some links may be missing, if so please refer to the next section.


## Adding a/my package or a/my database?

Our network is constantly evolving as we are adding more packages and databases. It tries to be as exhaustive as possible, but only with the information we have. If you would like to add a package and/or a database to the network please [open an issue](https://github.com/taxo_harmonization/issues/new) or write an email to [matthias.grenie[at]idiv.de](mailto:matthias.grenie@idiv.de).


## Citation

The development of this shiny app has been led by Alban Sagouis and Matthias Grenié.

If you use this shiny app in academic work and/or want to reference it please cite the following:

```
Grenié M., Berti E., Carvajal-Quintero J., Winter M., Sagouis A., Matching species names in biodiversity databases: database relationships, tools, pitfalls and best practices. doi: XXXX/XXXX 2021.
```

We also provide the BibTeX citation

```
@article {GrenieMatching2021,
	author = {Grenié, Matthias and Berti, Emilio and Carvajal-Quintero, Juan and Winter, Marten and Sagouis, Alban},
	title = {Matching species names in biodiversity databases: database relationships, tools, pitfalls and best practices},
	year = {2021},
	doi = {10.XXXX/XXXX},
	abstract = {},
	URL = {},
	eprint = {},
	journal = {XX}
}

```


## License

MIT License

Copyright 2021 iDiv

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.