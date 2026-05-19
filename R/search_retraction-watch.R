# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for searching relevant article from Retraction Watch database (version
# submitted to Github on 2025-10-20), excluding manual validation)

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages
if(!require("pacman")) {
  install.packages("pacman")
  library(pacman) }

# Load packages
pacman::p_load(dplyr,      # For data manipulation
               tidyr,      # For data manipulation
               purrr,      # For data manipulation
               jsonlite,   # For data manipulation
               readr,      # For data manipulation
               httr2,      # For retrieving Retraction Watch data
               polite)     # For retrieving Retraction Watch data

sessionInfo()

# POLITELY QUERY RETRACTION WATCH DATABASE #####################################

# Be polite: wrap request function inside politely
politely_req <- politely(httr2::request, verbose = TRUE)

# Request Semantic Scholar's Academic Graph API
req <- politely_req("https://gitlab.com/crossref/")

git_version <- "c5fef343d1cb198cfcf39fb6b4dd1ad240f3d8a5" # data for 2025-10-20

# Build the query
query <- req %>%

  req_url_path_append(
    "retraction-watch-data",
    "-",
    "raw",
    git_version,
    "retraction_watch.csv") %>%

  req_user_agent(
    paste("polite",
          "(email:tuomas.k.heikkila@helsinki.fi)", # Replace with personal email
          getOption("HTTPUserAgent"))) %>%

  req_progress() %>%                      # Monitor progress
  req_throttle(rate = 1/5) %>%            # Limit requests to 1 per 1 seconds
  req_retry(max_tries = 4, backoff = ~30) # Cap maximum tries, sleep between

query %>% req_dry_run()

# Perform the request
resp <- query %>%
  req_perform()

# Parse the response
resp_status(resp)
resp_content_type(resp)

database <- resp_body_string(resp) %>%
  readr::read_csv()

glimpse(database)

# WRANGLE DATA #################################################################

names(database) <- names(database) %>% gsub(" ", "_", .)

database <- database %>%
  mutate(RetractionDate = gsub(" AM", "", RetractionDate)) %>%
  mutate(RetractionDate = as.Date(RetractionDate, format="%m/%d/%Y"))

glimpse(database)

View(database)

database <- database %>%
  mutate(RetractionDOI = ifelse(
    RetractionDOI == "unavailable", NA, RetractionDOI))

# Clean "unavailable" Retraction DOIs with unique identifiers

database$RetractionDOI[is.na(database$RetractionDOI)] <-
  sample(seq_along(database$RetractionDOI[is.na(database$RetractionDOI)]),
         sum(is.na(database$RetractionDOI)),replace = FALSE)

# Filtering: retraction date between 2020-01-01 to 2022-10-31, inclusive
database <- database %>%
  filter(between(RetractionDate, as.Date("2020-01-01"), as.Date("2022-10-31")))

glimpse(database)

# SEARCH THE DATABASE ##########################################################

# Filtering: Type of retraction is "Retraction" or "Expression of concern")
retraction_nature <- c("Retraction", "Expression of concern")

database <- database %>% filter(
  grepl(paste(retraction_nature, collapse = "|"),
        RetractionNature, ignore.case = TRUE))

glimpse(database)

# Filtering: Case insensitive COVID-19 related keywords:
# (covid* OR "sars-*cov-2" OR coronavirus OR "2019-nCoV"): 268 records

covid_keywords <- c("covid", "sars-.*cov-2", "corona-*virus", "2019-ncov")

database <- database %>% filter(
  grepl(paste(covid_keywords, collapse = "|"), Title, ignore.case = TRUE))

glimpse(database)

# Filtering: Case insensitive COVID-19 related keywords:
# ((hydroxychloroquine* OR "hcq") OR ("ivermectin* OR "ivm"))

drug_keywords <- c("hydroxychloroquine", "hcq", "ivermectin", "ivm")

database <- database %>% filter(
  grepl(paste(drug_keywords, collapse = "|"), Title, ignore.case = TRUE))

glimpse(database)

# Filtering by Article type: ("Clinical Study" OR "Review Article"
# OR "Case Report" OR "Meta-Analysis" OR "Research Article")

article_type <- c("Clinical Study",
                  "Review Article",
                  "Case Report",
                  "Meta-Analysis",
                  "Research Article")

database <- database %>% filter(
  grepl(paste(article_type, collapse = "|"), ArticleType, ignore.case = TRUE))

glimpse(database)

# Removing duplicates:
database <- database %>% distinct(RetractionDOI, .keep_all = TRUE)

glimpse(database)
