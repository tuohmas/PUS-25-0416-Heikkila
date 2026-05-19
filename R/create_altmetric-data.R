# Tuomas Heikkilä, University of Helsinki
# tuomas.k.heikkila@helsinki.fi

# Script for gathering Altmetric data about included studies

### PREPARATIONS ###############################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation TEMP IS THIS NEEDED?
# options(scipen = 999)

if(!requireNamespace("pacman")) {
  install.packages("pacman")
  library(pacman)}

# Load packages
pacman::p_load(dplyr,
               tidyr,
               readr,
               httr2,
               polite,
               data.table,
               dtplyr,
               clipr)

sessionInfo()

### GATHER DATA ################################################################

# Original exports from Altmetric Explorer available upon reasonable request

dois <- scan("data/included_studies.txt", what = "character")

altmetric_data <- vector(mode = "list", length = length(dois))

altmetric_col_types <- "??c??c???????????????????????" # stringify id numbers

for (i in seq_along(dois)) {

  # Show progress
  cat(paste0("\nFetching Altmetric data from DOI: ",dois[i]," [",i,"/",
             length(dois),"]"))

  altmetric_study <- dois[i] %>%
    gsub("/", "_", .) %>%       # doi to data path
    paste0("data/altmetric/", ., ".csv") %>%
    read_csv(col_types = altmetric_col_types) %>%
    mutate(altmetric_id =
             gsub("https://www.altmetric.com/details/",
                  "",`Details Page URL`),
           doi = dois[i]) %>%

    select(`External Mention ID`, `Outlet or Author`, doi,
           altmetric_id, `Altmetric Attention Score`)

  colnames(altmetric_study) <- c("tweet_id", "tweet_author_id", "doi",
                                 "altmetric_id", "altmetric_attention_score")

  glimpse(altmetric_study)

  altmetric_data[[i]] <- altmetric_study

}

altmetric_data <- do.call("rbind", altmetric_data)
nrow(altmetric_data)

# Remove duplicates
altmetric_data <- altmetric_data %>% distinct(tweet_id, .keep_all = TRUE)
altmetric_data

altmetric_data %>% nrow()

# Save tweets and altmetric data
write_csv(altmetric_data, "data/altmetric_data.csv")
write_csv(altmetric_data[, c("tweet_id", "doi")], "data/altmetric_tweets.csv")

names(altmetric_data)

# Study summaries
altmetric_summary <- altmetric_data %>%
  mutate(
    doi = case_when(
      grepl("^10\\.21203/rs\\.3\\.rs-100956.+", doi) ~ "10.21203/rs.3.rs-100956/v4",
      grepl("10\\.31219/osf\\.io/wx3zn", doi) ~ "10.3389/fphar.2021.643369",
      .default = doi)
  ) %>%
  group_by(doi, altmetric_id, altmetric_attention_score) %>%
  summarise(tweet_count = n_distinct(tweet_id)) %>%
  ungroup()

altmetric_summary <- altmetric_summary %>%
  group_by(doi) %>%
  summarise(attention_score = sum(altmetric_attention_score),
            tweet_count     = sum(tweet_count))

# Save Altmetric summary
write_csv(altmetric_summary, "data/altmetric_summary.csv")

mean(altmetric_summary$attention_score) %>% round(0)
sd(altmetric_summary$attention_score) %>% round(0)
range(altmetric_summary$attention_score)
