# porTools

Sage portals require content management of publications, people, data, studies and grants stored in Synapse. This package helps maintain that content with constrained formatting.

[[[[work in-progress]]]]

`devtools::install_github('Sage-Bionetworks/porTools')`

## Table of Contents

* [Installation instructions](#installation-instructions)
* [Usage](#usage)
  * [Prerequisites](#prerequisites)
  * [Run Locally](#run-locally)
  * [Run Manually with GitHub Actions](#run-manually-with-github-actions)
* [Automation](#automation)
* [Updates](#updates)
* [Troubleshooting](#troubleshooting)
  * [PubMed Abstract Retrieval Failures](#pubmed-abstract-retrieval-failures)
  * [Publications Folder / File View Not Updated](#publications-folder--file-view-not-updated)
  * [Synapse Authentication Failure](#synapse-authentication-failure)
  * [Workflow Stalling](#workflow-stalling)
  * [Publication Annotation Validation Failures](#publication-annotation-validation-failures)
    * [Where to Find Annotation Validation Results](#where-to-find-annotation-validation-results)

## Installation instructions

These are the instructions for installing the dependencies for this project. You will need to have R and RStudio installed on your computer. You will also need to have an account on Synapse.

```R
install.packages('remotes')
remotes::install_cran('rentrez')
remotes::install_cran('librarian')
remotes::install_version('rjson', version='0.2.21')
remotes::install_version('reticulate', version='1.28')
reticulate::install_miniconda()
remotes::install_cran("synapser", repos = c("http://ran.synapse.org", "https://cloud.r-project.org"))
```

## Usage

The publication update can be run either locally from the command line or manually through GitHub Actions.

### Prerequisites

Before running the publication update, verify the Synapse IDs defined in `R/global-hard-coded-variables.R`.

The publication script currently uses the following hard-coded variables rather than the corresponding command-line arguments:

* `sid_pub_folder`: Synapse folder where new publication entities are stored.
* `sid_projects_table`: Synapse table containing the grant, program, and study metadata used to identify publications.
* `sid_pub_table`: Synapse file view used to identify publications that already exist in the portal.

Although `query-pubmed-grants.R` currently accepts `--parent`, `--grant_table`, and `--pub_table` command-line arguments, these values are not used by the publication processing logic. The values in `R/global-hard-coded-variables.R` determine which Synapse resources are used.

Before running against a different portal or Synapse location, update these variables as needed.

### Run Locally

After confirming the Synapse IDs in `R/global-hard-coded-variables.R`, run the script from the command line:

```bash
Rscript ./inst/scripts/query-pubmed-grants.R \
    --grant_table syn51209786 \
    --parent syn51317180 \
    --pub_table syn51407023
```

> **Note:** `--grant_table`, `--parent`, and `--pub_table` are currently accepted by the CLI but are not used by the script. The corresponding `sid_projects_table`, `sid_pub_folder`, and `sid_pub_table` values in `R/global-hard-coded-variables.R` are used instead.

### Run Manually with GitHub Actions

The [`updated-publications.yaml`](.github/workflows/updated-publications.yaml) workflow supports `workflow_dispatch`, allowing it to be run manually from the GitHub Actions tab.

To manually run the workflow:

1. Open the [Actions tab](https://github.com/eliteportal/publication_scraper/actions).
2. Select the [Update Publications workflow](https://github.com/eliteportal/publication_scraper/actions/workflows/update-publications.yaml).
3. Select **Run workflow**.

![Alt text](img/manual_workflow_dispatch_example.png)

This is also the recommended way to re-run the publication update after resolving a workflow failure.

## Automation

The [`updated-publications.yaml`](.github/workflows/updated-publications.yaml) GitHub Actions workflow runs the publication update automatically on a monthly schedule. The workflow runs the same publication query described above using the Synapse service user `synapse-service-dpe-team`.

Important notes about the GitHub Action:

* GitHub automatically disables scheduled workflows in public repositories after 60 days of repository inactivity. See [Publications Folder / File View Not Updated](#publications-folder--file-view-not-updated) for troubleshooting.
* Review the **Query PubMed and upload results** step in the Actions run to determine whether publications were updated. If there are no new PMIDs to add, the output will include `[1] "All pmids already in the portal"`.

## Updates

**2023-10-10**

* If the grant serial number overlaps with another, for example `UH2AG064706` and `UH3AG064706`, then a different call to get the search results must be made and the previously developed functions do not work.
* Found the NIH library for R is much faster than Python.

## Troubleshooting

### PubMed Abstract Retrieval Failures

When retrieving abstracts by PubMed ID using the `get_abstract` function, you may occasionally encounter an `HTTP failure: 404` error. If the same request succeeds when retried without any changes, the error may be caused by a transient issue with the NCBI service or HTTP request.

The `get_abstract` function includes retry logic to handle temporary request failures. However, the retry interval may not always be long enough for the issue to resolve. If the request continues to fail after all retry attempts, wait a few minutes and [manually run the workflow](#run-manually-with-github-actions).

### Publications Folder / File View Not Updated

If the publications folder and subsequent file view monitoring have not been updated as expected, check whether the scheduled workflow is still enabled.

GitHub automatically disables scheduled workflows in public repositories after 60 days of repository inactivity. Check the [Actions tab](https://github.com/eliteportal/publication_scraper/actions) to confirm that the [`updated-publications.yaml` workflow](https://github.com/eliteportal/publication_scraper/actions/workflows/update-publications.yaml) is enabled.

If it has been disabled (there will be a note specifying it has been disabled with an option to enable), manually re-enable it and then [run the workflow](#run-manually-with-github-actions).

### Synapse Authentication Failure

If the workflow fails with the following error:

```text
Error: synapseclient.core.exceptions.SynapseAuthenticationError:
You are not logged in and do not have access to a requested resource
```

the `SYNAPSE_PAT` GitHub secret may have expired, been revoked, or otherwise become invalid.

The workflow authenticates to Synapse using a Personal Access Token (PAT) associated with the DPE Synapse service user. If the token is no longer valid (there is a 180-day inactivity expiration policy with Synapse PATs), log in to Synapse using the DPE service user account and create a new [Personal Access Token](https://docs.synapse.org/synapse-docs/managing-your-account#Personal-Access-Tokens-(PATs)).

After creating the new token:

1. Update the repository's `SYNAPSE_PAT` [GitHub Actions repository secret](https://github.com/eliteportal/publication_scraper/settings/secrets/actions) with the new token.
2. Manually run the workflow using [GitHub Actions](#run-manually-with-github-actions).
3. Confirm that the workflow can successfully authenticate to Synapse and complete the publication update.

### Workflow Stalling

If the workflow appears to stall without producing additional output , check the Actions logs to determine the last step or message that completed.

The workflow may occasionally appear to stall while communicating with an external service, particularly during:

* Synapse authentication (`syn$login()`)
* PubMed metadata retrieval (`pub_query()`)

If the workflow stalls during Synapse login, would cancel the stalled workflow, wait a few minutes, and then [manually run the workflow again](#run-manually-with-github-actions).

If the workflow stalls during retrieving the publications externally (e.g: `pub_query()`), the issue may be related to a temporary PubMed/NCBI service or network issue. You could continued to wait (it may take up to 30 minutes or longer) or cancel the stalled workflow, wait a few minutes, and then [manually run the workflow again](#run-manually-with-github-actions).

### Publication Annotation Validation Failures

Before new publication entities are saved to Synapse, the workflow validates the annotation values that will be added to each publication. This validation is intended to catch upstream metadata parsing or transformation issues before incorrect annotations are stored in Synapse.

For each annotation, the workflow logs the number of publications with missing values.

For example, successful validation may look like the following:

```text
Annotation validation: Authors - OK (0/25 missing)
Annotation validation: Journal - OK (0/25 missing)
Annotation validation: Grant - OK (2/25 missing)
Annotation validation: Program - OK (0/25 missing)
Annotation validation: publicationDate - OK (0/25 missing)
```

Some publications may actually be missing an annotation, so individual missing values do not cause the workflow to fail. However, if an annotation is missing for **all** publications, validation fails and the workflow stops before the publications are saved to Synapse.

For example:

```text
Error: VALIDATION FAILED: annotation 'publicationDate' is missing for ALL 25 publications. Aborting Synapse upload.
Execution halted
```

This generally indicates an issue earlier in the publication metadata processing rather than an issue with Synapse itself. For example, a change in the format returned by PubMed may cause an annotation transformation to produce `NA` for every publication.

**Steps to resolve**

If validation fails, use the annotation reported in the error to determine which part of `query-pubmed-grants.R` to inspect:

* **PubMed-derived annotations (`publicationDate`, `Year`, `Journal`, `Authors`, `Title`, `DOI`)**: Start with the PubMed metadata returned by `pub_query()` and the section where those values are extracted, cleaned, or transformed. Compare the raw PubMed value with the resulting annotation value. For example, if `publicationDate` is missing, inspect the raw `pubdate` values and the date parsing logic.

* **Grant-derived annotations (`Grant`, `Program`)**: Inspect the grant metadata retrieved from the Synapse projects table, the joins between grants and publications, and the subsequent formatting of these columns. These annotations are derived from the grant/project metadata rather than directly from the PubMed article.

* **Derived annotations (`Name`, `preprint`)**: Inspect the functions or transformations that generate these values from other publication metadata. For example, `Name` is generated using `make_entity_name()`, while `preprint` is derived from the publication's journal.

After identifying the affected annotation, trace its value through the subsequent formatting and renaming steps and confirm that the expected column name still exists before `purrr::transpose()` and `store_as_annotations()` are called.

#### Where to Find Annotation Validation Results

For an automated or manually triggered GitHub Actions run:

1. Open the [Actions tab](https://github.com/eliteportal/publication_scraper/actions).
2. Select the relevant **Update Publications** workflow run.
3. Open the **Query PubMed and upload results** step.
4. Search the log output for `Annotation validation`.

The validation output immediately before the Synapse upload will show which annotations contain missing values and whether validation passed.

For example:

```text
[2026-09-12 10:15:22] Annotation validation: Authors - OK (0/18 missing)
[2026-09-12 10:15:22] Annotation validation: Journal - OK (0/18 missing)
[2026-09-12 10:15:22] Annotation validation: Grant - OK (1/18 missing)
[2026-09-12 10:15:22] Annotation validation: Program - OK (0/18 missing)
[2026-09-12 10:15:22] Annotation validation: publicationDate - OK (0/18 missing)
[2026-09-12 10:15:22] All annotation validation checks passed.
```