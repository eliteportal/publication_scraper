library(dccvalidator)
library(dplyr)
library(easyPubMed)
library(readr)
library(reticulate)
library(porTools)

# Login to synapse
source("~/Projects/ELITE/porTools/R/synapseLogin.R")

# The study folders get annotated with phase, grants, tissue, species, diagnosis, study type, study description, nucleic acid source and contributing institution.
#
# - The keys *grants*, *tissue*, *species*, *diagnosis* and *nucleicAcidSource* follow the constrained vocabulary of the [synapseAnnotations repo](https://github.com/sage-bionetworks/synapseannotations).
# - *contributingInstitution* is very case sensitive, look at the existing syntax in the contributing institution facet -  https://psychencode.synapse.org/Explore/Studies.
# - adapt *studyDescription* from study description in the folder wiki. 1-2 sentences. Make sure to define the study name in the first sentence.
# - *methods* correspond to the folder Synapse Id for each assay (where the methods descriptions live).

# define inputs - diagnosis, number of individuals, tissue, relatedStudies, modelSystem are not required keys
# read study and methods description, use NIMH tracking table and provided metadata to fill in these values
folder_id <- "syn21392931"
phase <- "II"
grants <- "U01MH116438"
tissue <- c("prefrontal cortex", "cerebral cortex")
species <- c("Human")
# use the dataType values from synapseAnnotations schema, but not in camel case (e.g. geneExpression)
dataTypes <- c("Chromatin Activity", "Gene Expression")
# leave blank if control/normal/neurotypical donors in study:
diagnosis <- NA
# most likely, all studies will be "Individual"
studyType <- "Individual"
# adapt from study description. 1-2 sentences. Make sure to define the study name in the first sentence:
studyDescription <- "The neuronal regulatory elements (NeuRE) study aims to annotate cell-type-specific gene regulatory networks in the developing brain. This resource integrates analysis of the single cell transcriptome and chromatin states."
nucleicAcidSource <- c("single cell", "bulk cell")
# Very case sensitive, look at the existing syntax in the contributing institution facet https://psychencode.synapse.org/Explore/Studies:
contributingInstitution <- "University of California, San Francisco"
numberInd <- "16"
methods <- c("syn21995633", "syn25874242", "syn22098180", "syn26009957")
relatedStudies <- NA
# boolean
modelSystem <- NA

# get folder to set annotations on
folder <- syn$get_annotations(folder_id)
folder['phase'] <- phase
folder['grants'] <- grants
# conditional
if (!is.na(tissue)) {
  folder['tissue'] <- tissue
}
folder['species'] <- species
folder['dataTypes'] <- dataTypes
# conditional
if (!is.na(numberInd)) {
  folder['numberOfIndividuals'] <- numberInd
}
# conditional
if (!is.na(diagnosis)) {
  folder['diagnosis'] <- diagnosis
}
# conditional
if (!is.na(relatedStudies)) {
  folder['relatedStudies'] <- relatedStudies
}
# conditional
if (!is.na(modelSystem)) {
  folder['isModelSystem'] <- modelSystem
}
folder['methods'] <- methods
folder['studyType'] <- studyType
folder['studyDescription'] <- studyDescription
folder['contributingInstitution'] <- contributingInstitution
output <- syn$set_annotations(folder)
