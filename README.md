# porTools

Sage portals require content management of publications, people, data, studies and grants stored in Synapse. This package helps maintain that content with constrained formatting.


[[[[work in-progress]]]]

`devtools::install_github('Sage-Bionetworks/porTools')`

## Installation instructions

These are the instructions for installing the dependencies for this project. You will need to have R and RStudio installed on your computer. You will also need to have an account on Synapse.

```
install.packages('remotes')
remotes::install_cran('rentrez')
remotes::install_cran('librarian')
remotes::install_version('rjson', version='0.2.21')
remotes::install_version('reticulate', version='1.28')
reticulate::install_miniconda()
remotes::install_cran("synapser", repos = c("http://ran.synapse.org", "https://cloud.r-project.org"))
```

## Usage
You will want to run this tool via the command line in the terminal.

```
Rscript ./inst/scripts/query-pubmed-grants.R \
    --grant_table syn51209786 \
    --parent syn51317180 \
    --pub_table syn51407023
```

## Updates
**2023-10-10**
- If the grant serial number overlaps with annother for example `UH2AG064706` and `UH3AG064706` then a different call to get the search results must be made and the previously developed functions do not work 
- Found the NIH library for R is much faster than python 
