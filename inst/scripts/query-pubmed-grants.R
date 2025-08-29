# ----setup, include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
# options(repos = c(CRAN = "https://cloud.r-project.org/"))
# remotes::install_cran('rentrez')
# remotes::install_cran('librarian')
# remotes::install_version('rjson', version='0.2.21')
# remotes::install_version('reticulate', version='1.28')
# remotes::install_cran("synapser", repos = c("http://ran.synapse.org", "https://cloud.r-project.org"))

# install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))

librarian::shelf(
  optparse,
  rmarkdown,
  reticulate,
  janitor,
  dplyr,
  readr,
  stringr,
  easyPubMed,
  synapser,
  comprehenr,
  httr,
  tidyr,
  lubridate,
  XML,
  rentrez
)

# library('synapser')

# nolint start
option_list <- list(
  make_option(
    "--auth_token",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse Personal Access Token. If not given, assumes local .synapseConfig."
  ),
  make_option(
    "--grant_table",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID for table with grants to query for. Requires columns `grant`, `grantSerialNumber`, `Program`. grants are queried by serial number."
  ),
  make_option(
    "--parent",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of parent folder to store publication entities to."
  ),
  make_option(
    "--pub_table",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of file view scoped to publication folder (`parent`)."
  )
)
opts <- parse_args(OptionParser(option_list = option_list))

# get the base working directory to make it work on others systems
base_dir <- gsub('vignettes', '', getwd())
source(glue::glue("{base_dir}/R/pubmed.R"))
source(glue::glue("{base_dir}/R/text-cleaning.R"))
source(glue::glue("{base_dir}/R/annotation.R"))
source(glue::glue("{base_dir}/R/global-hard-coded-variables.R"))

# Login to synapse
## Synapse client and logging in
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")

syn <- synapseclient$Synapse()
if (!is.na(opts$auth_token)) {
  syn$login(authToken = opts$auth_token)
} else {
  syn$login()
}

## ----functions------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hacky_cleaning <- function(text) {
  conv <- convert_to_ascii(text = text)
  conv <- remove_hmtl_formatting(text = conv)
  conv <- gsub("&amp;|amp;", "and", conv)
  conv <- gsub("&#39;|&quot;", "'", conv)
  conv <- gsub("&gt;", "Greater Than ", conv)
  conv <- gsub("[[:punct:]]+", "", conv)
  conv <- gsub("\\s+", " ", conv)
  conv <- str_trunc(text, width = 500)
  return(conv)
}


## ----vars, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# table_id <- "syn51209786" # ELITE Portal Projects Table

# Gather list of grants from synapse
grants <-
  syn$tableQuery(glue::glue("SELECT grant, program, name FROM {sid_projects_table}"))$asDataFrame()

# convert grant numbers into string
grantNumbers <-
  to_list(for (g in grants$grant)
    for (y in g)
      y)

# expand rows with multiple grantNumbers
grants$grant <-
  purrr::map(grants$grant, function(x) {
    paste(unlist(x), collapse = ",")
  })

grants <- grants %>%
  separate_rows(grant)

## ----scrape pubmed ids from grant numbers---------------------------------------------------------------------------------------------------------------------------------------------
get_pub_details <- function(request_body) {
  # Make the POST request
  response <-
    POST(
      url = API_URL,
      headers = headers,
      body = request_body,
      encode = "json"
    )
  return (response)
}

process_response <- function(response) {
  if (response$status_code == 200) {
    # Success!
    data <- content(response, "parsed")

    df <- as.data.frame(do.call(rbind, data$results))

    return (df)
  }
}

# works for project Numbers instead of project serial numbers
# Set the API URL
API_URL <- "https://api.reporter.nih.gov/v2/publications/search"

# Set the headers
headers <- list(accept = "application/json",
                "Content-Type" = "application/json")

# Set the request body
request_body <- list(
  criteria = list(core_project_nums = grants$grant),
  offset = 0,
  limit = 50,
  sort_field = "core_project_nums",
  sort_order = "desc"
)

# Make the POST request
response <-
  POST(
    url = API_URL,
    headers = headers,
    body = request_body,
    encode = "json"
  )

# Check the response status code
if (response$status_code == 200) {
  # Success!
  pmids <- list()

  # get results as dataframe
  pmids_temp <- process_response(response)

  data <- content(response, "parsed")

  total <- data$meta$total

  results <- process_response(response)

  pmids[[length(pmids) + 1]] <- results

  request_body$offset <- request_body$offset + request_body$limit

  while (nrow(results) > 0) {
    response <- get_pub_details(request_body)

    results <- process_response(response)

    # extend pmids list
    pmids[[length(pmids) + 1]] <- results

    # update offset in request
    request_body$offset <-
      request_body$offset + request_body$limit
  }
} else {
  # Something went wrong
  print("Error:", response$status_code)
}

# create dataframe with pmids
pmids_df <- do.call(rbind, pmids)

pmids_df <- pmids_df %>% rename('grant' = 'coreproject')

# for joining
pmids_df$grant <- as.character(pmids_df$grant)

# remove exisiting entities in portal
pubs_exisiting <-
  syn$tableQuery(
    glue::glue(
      "SELECT id, Name, PubmedId, PMID, Title, grant, Program FROM {sid_pub_table}"
    )
  )$asDataFrame()

# remove duplicate no name entities
# for (i in as.list(pubs_exisiting[grep("NA NA NA", pubs_exisiting$Name), "id"])) {
#   print(i)
#   tryCatch({
#     syn$delete(i)
#   },
#   error = function(e) {
#     print(glue::glue('error deleting {i}'))
#   })
# }

# collapse rows by grouping by pmids since some publications can be assoicated with multiple grants
pmids_df <- pmids_df %>% group_by(pmid) %>% reframe(
  grant = paste0(grant, collapse = ","),
  applid = paste0(unique(applid), collapse = ",")
)

# Take only pmids not in the portal already
pmids_df <-
  pmids_df[!(pmids_df$pmid %in% pubs_exisiting$PubmedId),]

if (nrow(pmids_df) == 0) {
  print("All pmids already in the portal")
} else {
  ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # one eternity later....
  pmid_metadata <- pub_query(pmids_df$pmid)
  ## ----query----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # create complete dataset
  dat <- dplyr::right_join(grants, pmids_df, by = "grant")

  dat$pmid <- as.character(dat$pmid)

  dat <- dplyr::inner_join(dat, pmid_metadata, by = "pmid")

  # clean column names
  dat <- janitor::clean_names(dat, "lower_camel")

  # ---- get abstract function ----------------------------------------------------------------------------------------
    get_abstract <- function(pmid) {
      # Function to get abstracts per pubmed id: https://stackoverflow.com/questions/77211966/r-how-to-extract-a-pubmed-abstract-using-rentrez
    record <- rentrez::entrez_fetch(db = "pubmed", id = pmid, rettype = "xml", parsed = TRUE)
    
    abstract_nodes <- XML::xpathSApply(record, "//AbstractText", XML::xmlValue)
    
    if (length(abstract_nodes) > 0) {
      abstract_text <- abstract_nodes[[1]]
      return(abstract_text)
    } else {
      print("No abstract found.")
    }
  }
  
  ## ----hacky----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Included in hacky_cleaning is conversion to ascii and removing html formatting
  dat$year <- stringr::str_extract(dat$pubdate, "\\d{4}")
  dat$year <- as.integer(dat$year)
  dat$title <- hacky_cleaning(dat$title)
  dat$authors <- hacky_cleaning(dat$authors)
  dat$journal <- remove_unacceptable_characters(dat$fulljournalname)
  dat$publicationDate <- stringr::str_extract(dat$pubdate, "\\d{4}-\\d{2}-\\d{2}")
  dat$abstract = purrr::map(dat$pmid, get_abstract)

  # dat$abstract <- hacky_cleaning(dat$abstract)

  # fix publication dates
  dat <- dat %>%
  mutate(
    publicationDate_clean = case_when(
      grepl("^\\d{4}$", publicationDate) ~ paste0("01 Jan ", publicationDate),
      grepl("^[A-Za-z]{3} \\d{4}$", publicationDate) ~ paste0("01 ", publicationDate),
      TRUE ~ publicationDate
    ),
    publicationDate_clean = parse_date_time(
      publicationDate_clean,
      orders = c("d b Y", "b Y", "Y", "Y-m-d")
    ),
    publicationDate_clean = format(publicationDate_clean, "%m/%d/%Y")
  ) %>%
  select(-publicationDate) %>%
  rename(publicationDate = publicationDate_clean)

dat <- dat %>%
  mutate(publicationDate = format(as.Date(publicationDate, format = "%m/%d/%Y"), "%Y-%m-%d"))
  
  # drop unnecessary columns
  dat <- dat %>% select(-c('applid', 'result'))
  cat(
    'Total rows: ',
    nrow(dat),
    '\n',
    'Duplicates: ',
    sum(dat %>% duplicated()),
    '\n',
    'Rows after duplicate remove: ',
    nrow(dat) - sum(dat %>% duplicated())
  )

  # Need to remove duplicates, but keep all grants and consortium
  # Includes some renaming and dropping of columns
  dat <- dat %>%
    group_by(pmid) %>%
    mutate(grant = glue::glue_collapse(unique(.data$`grant`), ", ")) %>%
    mutate(consortium = glue::glue_collapse(unique(.data$program), ", ")) %>%
    mutate(name = glue::glue_collapse(unique(.data$name), ", ")) %>%
    select(!c(program)) %>%
    rename(
      pubmed_id = pmid,
      DOI = doi,
      program = consortium,
      study = name
    ) %>%
    distinct()

  dat <- dat %>% rename('pmid' = 'pubmed_id')
  dat$entity_name <- make_entity_name(dat)
  dat$Name <- make_entity_name(dat)


  #Using rename()
  dat <- dat %>% rename(
    "Authors" = "authors",
    "Journal" = "journal",
    "PubmedId" = "pmid",
    "Title" = "title",
    "Year" = "year",
    "Program" = "program",
  )

  # Remove common, unallowed characters from entity name; includes hacky_cleaning
  dat$entity_name <- remove_unacceptable_characters(dat$entity_name)

  # add preprint annotation
  dat <- dat %>%
  mutate(
    preprint = if_else(
      str_detect(Journal, regex("rxiv", ignore_case = TRUE)) |
        str_to_lower(Journal) == "research square",
      "yes",
      "no"
    )
  )

  ## ----columns--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dat <- set_up_multiannotations(dat, "grant")
  dat <- set_up_multiannotations(dat, "Program")
  dat <- set_up_multiannotations(dat, "Authors")

  ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  store_as_annotations <- function(parent, list) {
  purrr::map(
    list,
    function(x) {
      file <- synapseclient$File(
        path = glue::glue("http://doi.org/{x$DOI}"),
        name = x$entity_name,
        parent = parent,
        synapseStore = FALSE
      )
      
      file$annotations <- reticulate::dict(
        Authors = x$Authors,
        Journal = x$Journal,
        PubmedId = x$PubmedId,
        Title = x$Title,
        Year = x$Year,
        Grant = x$Grant,
        Program = x$Program,
        publicationDate = x$publicationDate,
        DOI = x$DOI,
        Name = x$Name,
        preprint = x$preprint
      )
      
      file$annotations[["__annotations__"]] <- reticulate::dict(
        publicationDate = "DATE"
      )
      
      syn$store(file, forceVersion = FALSE)
      # make the wiki with abstract
      if (!is.null(x$abstract) && nchar(x$abstract) > 0) {
        wiki <- synapseclient$Wiki(
          owner = entity$id,
          markdown = x$abstract
        )
        syn$store(wiki)
}
      
    }
  )
}

  ## ----store, message=FALSE, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
  # parent = "syn51317180" # ELITE publications folder
  dat_list <- purrr::transpose(dat)
  # another eternity
  store_as_annotations(parent = sid_pub_folder, dat_list=dat_list)
}

# For some reason there is a warning about leaded semaphore that is
# leading to the process to never finish.
reticulate::py_run_string("
import warnings

# Suppress specific multiprocessing warnings
warnings.filterwarnings('ignore', message='resource_tracker: There appear to be .*')
")
