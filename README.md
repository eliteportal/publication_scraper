# porTools

Sage portals require content management of publications, people, data, studies and grants stored in Synapse. This package helps maintain that content with constrained formatting.


[[[[work in-progress]]]]

`devtools::install_github('Sage-Bionetworks/porTools')`

## Usage
You will want to run this tool via the command line in the terminal.

```
Rscript ./inst/scripts/query-pubmed-grants.R --grant_table syn51209786 --parent syn51317180 --pub_table syn51407023
```

## Updates
**2023-10-10**
- If the grant serial number overlaps with annother for example `UH2AG064706` and `UH3AG064706` then a different call to get the search results must be made and the previously developed functions do not work 
- Found the NIH library for R is much faster than python 
