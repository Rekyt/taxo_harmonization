# Notes on Lab Meeting on Taxonomic Name Harmonization

Taxonomic referential are hard. They are a necessary tool built by taxonomic, but it's an always changing tool (taxonomists are **DOING THEIR JOB**!). As we refer to species in checklists/databases we also need to refer to which taxonomic reference it was matched against (which version?).

Managing this task manually is too cumbersome, error-prone, and of course imperfect as taxonomic references are always changing. As users of taxonomic reference systems we should focus on their **usage** and not the nitty-picky details in each one of them.

One challenge with working with multiple sources of information is that each one of them can refer to different taxonomical references. One important thing is that databases provide their taxonomic backbones and do the effort of matching against worldwide accepted references.

Furthermore integrating datasets from different areas that don't share similar taxonomical references make it difficult to work with.
One solution is to create an aggregated list of all species referenced and compare to worldwide if available references.

Another issue is the fact that we sometimes record information that is coarser scale than at the species-level. How can we still the information in our systems? Should we adopt a purely bayesian view? For example in TRY XX species are unresolved at beyond Genus or even Family levels! However, their traits is measured so should we assign their characteristics based on probability densities? This is especially true when sampling organisms that come up at different phenological stages. This problem is worsened in the Tropics where many species are unknown or very difficult to identify.


## Tools

Turboveg2/Tuboveg3 are tools built for vegetation relevés and they have an integration of taxonomic concepts such as species names, genera, etc. so they make data aggregation easier (and species assignment easier as well).

taxonstand is a package that helps standardizing names against a given reference.

taxize is the swiss army knife of taxonomic name cleaning.

ropensci/rgnparser

TNRS is great but has many unresolved names that can be resolved from local sources.

Fishbase, WOrRms, Avibase, are taxon-specific tools that provide references. However, locally produced datasets may not match against these references. One key thing is that taxonomic information should always be given against a given taxonomical reference.

For plants we have different worldwide references: World of Plant, Plants of the World Online, LCVP, The Plant List (which is outdated).


## What to do

A review of tools and strategies for automatic name cleaning could be useful to point to tools used in the literature as well as what is missing.

One idea (Carsten Meyer) because species names are generally thought to refer to the modern concept of the species, is to propagate the uncertainty around certain names, this could be doable if users have access to previous datasets and historical taxonomical references. A species names can thus be resolved into several names and we consider this uncertainty at each step of the analysis.
