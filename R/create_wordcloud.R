# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
# Script for creating smart word clouds; Illustrate differences in vocabularies
# Inspired by Johannes Gruber: https://www.johannesbgruber.eu/post/smarter-wordclouds/
#
# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages
if(!requireNamespace("pacman")) {
  install.packages("pacman")
  library(pacman) }

# Load packages
pacman::p_load(dplyr,               # For data manipulation
               tidyr,               # For data manipulation
               purrr,               # For data manipulation
               readr,               # For data manipulation
               quanteda,            # For corpus linguistics
               quanteda.textstats,  # For calculating keyness
               ggplot2,             # For visualisations
               ggwordcloud)         # For word cloud plots

sessionInfo()

# Load data (original data available upon request)
labeled_tweets <- readRDS("data/clean_labeled_tweets.rds")

# Anonymised quanteda DFM subsetted to adjectives available on results folder
dfm_adjectives <- readRDS("results/dfm_adjectives.rds")
docnames(dfm_adjectives)

# Skip to WORDCLOUD section when using anonymised data

# TEXT PREPROCESS ##############################################################

# Define a function to tokenize and lemmatize tweet corpus; additional, optional
# functionalities for removing stopwords, trimming corpus, and collocating
# compound words

text_cleanup <- function(
    data,  # Exported and annotated df with text and doc id
    remove_stopwords = FALSE,
    stop_words = stopwords::stopwords("en"),
    word_collocations = FALSE,
    collocations_min_count = 3,
    collocations_p = 0.001,
    dfm_trim = FALSE,
    dfm_tfidf = FALSE
    ) {

  # Reduce and filter data frame
  keep_cols <- c("tweet_id", "text", "label", "code")

  data <- data %>%
    select(all_of(keep_cols))

  # Clean, tokenize and pre-process
  tokens <- data %>%

    # Clean Twitter specific expressions and badly encoded characters
    mutate(text = gsub("[AT MULTIPLE USERS]", "", text, fixed = TRUE)) %>%
    mutate(text = gsub("[LINK]", "", text, fixed = TRUE)) %>%
    mutate(text = gsub("#", "", text)) %>%
    mutate(text = gsub("@.+?\\b", "", text)) %>%
    mutate(text = gsub("&lt;", ">", text)) %>%   # Less than
    mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
    mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
    mutate(text = gsub("’", "'", text)) %>%   # Greater than
    mutate(text = gsub("=", " = ", text)) %>%   # Equal than
    mutate(text = gsub("\\?|\\)|\\(", "", text)) %>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub(
      paste(clean_study_names$phrase, collapse = "|"), "", text)) %>%

    # Separate words inside hashtags ("#IvermectinSavesLives")
    mutate(text = gsub("((?<=[a-z]|[1-9])[A-Z])", " \\1", text, perl = TRUE)) %>%

    # Create corpus with quanteda package
    quanteda::corpus(docid_field = "tweet_id", text_field = "text") %>%

    # Remove punctuation, symbols, and numbers; retain padding for later
    quanteda::tokens(what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     padding = word_collocations)

  # Optional: Remove stop words; reserve padding for later
  if(remove_stopwords) {

    tokens <- tokens %>%
      tokens_remove(stop_words, padding = word_collocations)

    }

  # Optional: Collocate compound words that appear together often with
  # statistically significant regularity (Note: may cause trouble when stop words
  # are not removed)

  if(word_collocations) {

  collocations <-
    textstat_collocations(tokens,
                          min_count = collocations_min_count, # abs. threshold
                          size = 2) %>%       # bigrams only

    # Keep collocations above critical value (defaults to 99% confidence level)
    filter(z > qnorm(p = collocations_p / 2, lower.tail = FALSE) |
           z < qnorm(p = collocations_p / 2)) %>%

    # Arrange by descending word frequency
    arrange(desc(count))

  # Replace multi-token sequences with newly created compound tokens
  tokens <- tokens_compound(tokens,
                            pattern = collocations,
                            concatenator = "_",
                            keep_unigrams = FALSE)

  }

  # Create a Document-Feature Matrix
  dfm <-
    tokens %>%

    # Create equivalency classes: Lemmatize tokens
    tokens_replace(pattern = lexicon::hash_lemmas$token,
                   replacement = lexicon::hash_lemmas$lemma) %>%

    # Custom collocations: same root, e.g., peer review and peer reviewed
    tokens_replace(pattern = "peer_reviewed", replacement = "peer_review") %>%
    tokens_replace(pattern = "clinical_trials", replacement = "clinical_trial") %>%
    tokens_replace(pattern = "meta-analysis", replacement = "meta_analysis") %>%

    # Lemmatize data as data:
    tokens_replace(pattern = "datum", replacement = "data") %>%

    # Remove padding
    dfm(remove_padding = TRUE)


  # Optional: Trim terms appreaing in less than 1% or more than 99% of documents
  if(dfm_trim) {

    dfm <- dfm_trim(dfm,  min_termfreq = 0.01, max_docfreq = 0.99,
                  docfreq_type = "prop", verbose = TRUE)
  }

  # Optional: Weight a dfm by term frequency-inverse document frequency
  if(dfm_tfidf) {

    dfm <- dfm_tfidf(dfm)

    }

  return(dfm)

}

# Define custom stop words: include verbatim quotes of studies and their names
study_data <- read_csv("data/retracted_studies.csv")

# Add when was the tweet posted relative to the study's publication date
study_names <- study_data %>%
  select(title) %>%
  mutate(title = gsub("retracted: |retracted article: ", "", title,
                      ignore.case = TRUE)) %>%
  distinct(title)

new_name <- tibble(
  title = "Ivermectin for Prevention and Treatment of COVID-19 Infection: A Systematic Review, Meta-analysis, and Trial Sequential Analysis to Inform Clinical Guidelines"
)

study_names <- study_names %>%
  rbind(new_name, .)

clean_study_names <- study_names %>%
  separate_longer_delim(title, delim = ": ") %>%
  rbind(study_names, .) %>%
  mutate(title = tolower(title)) %>%
  distinct(title) %>%
  mutate(title = gsub("\\?|\\)|\\(", "", title))

clean_study_names <- clean_study_names %>%
  mutate(trunc70 = ifelse(nchar(title) > 70, stringr::str_trunc(title, 70), NA),
         trunc60 = ifelse(nchar(title) > 60, stringr::str_trunc(title, 60), NA),
         trunc50 = ifelse(nchar(title) > 50, stringr::str_trunc(title, 50), NA),
         trunc40 = ifelse(nchar(title) > 40, stringr::str_trunc(title, 40), NA))

clean_study_names <- clean_study_names %>%
  gather(type, phrase, title:trunc40, factor_key = TRUE) %>%
  select(phrase) %>%
  filter(!is.na(phrase))

clean_study_names

journal_names <- data.frame(
  phrase = tolower(
    c("American Journal of Therapeutics",
      "IJAA",
      "open forum infectious diseases",
      "Oxford Academic",
      "Research Square",
      "The Journal of Antibiotics",
      "SocArXiv Papers",
      "Elsevier Enhanced Reader"))
  )

verbatim_quotes <- data.frame(
  phrase = tolower(
  c("Moderate-certainty evidence finds that",
    "large reductions in COVID-19 deaths are possible using",
    "Meta-analyses based on 18 randomized controlled treatment trials",
    "have found large statistically significant reductions in mortality",
    "time to clinical recovery", "time to viral clearance",
    "Ivermectin was associated with reduced inflammatory markers",
    "faster viral clearance by PCR",
    "Using ivermectin early in the clinical course may reduce numbers progressing to severe disease",
    "The apparent safety and low cost suggest",
    "ivermectin is likely to have a significant impact on the SARS-CoV-2 pandemic globally"
    )
  )
)

clean_study_names <- clean_study_names %>%
  rbind(journal_names) %>%
  rbind(verbatim_quotes) %>%
  distinct(phrase)

clean_study_names

# Combine multiple stop words dictionaries
my_stopwords <-
  stopwords::stopwords("en", source = "snowball") %>%      # Append two
  append(stopwords::stopwords("en", source = "nltk")) %>%  # stopword libraries

  append(c("one", "two", "three", "four", "five",          # and numbers 1–10
           "six", "seven", "eight", "nine", "ten")) %>%

  append(c("…", "”", "“", "‘", "•")) %>% # Append exotic punctuation

  unique() %>%
  sort()

# Keep stopwords that potentially encode meaning
# (e.g. "why was it pulled?", "no benefit", "most doctors disagree")
keep_stopwords = c("no", "not", "more", "most", "less", "why")

my_stopwords <- my_stopwords[!my_stopwords %in% keep_stopwords]

# Use functions
dfm_tweets <- labeled_tweets %>%
  text_cleanup(remove_stopwords = TRUE,
               stop_words = my_stopwords,
               word_collocations = TRUE,
               dfm_trim = FALSE
               )

# Filter to adjectives only
pos_adjectives <- tidytext::parts_of_speech %>%
  filter(pos == "Adjective")

# Discard misclassified POS
discard_adjectives <- c(
  "meta", "paper", "pandemic", "mean", "u", "spread", "associate",
  "deal", "abstract", "able", "august", "editorial", "blind", "tony",
  "read", "like", "even", "still", "japan", "subject", "much", "self",
  "proof", "side", "otherwise", "aware", "shut", "likely", "quit",
  "inform", "sick", "unlike", "patent")

dfm_adjectives_tiny <-
  tidytext::tidy(dfm_tweets) %>%
  left_join(pos_adjectives, by = c("term" = "word")) %>%
  filter(!is.na(pos)) %>%
  filter(!term %in% discard_adjectives) %>%
  left_join(labeled_tweets[, c("tweet_id", "code")],
            by = c("document" = "tweet_id"))

dfm_adjectives <-
  tidytext::cast_dfm(dfm_adjectives_tiny,
    term = term, document = document, value = count)

docnames(dfm_adjectives)

# Join label data with tweet id as a key
data.frame(docs = docnames(dfm_adjectives)) %>%
  left_join(labeled_tweets[, c("tweet_id", "code")],
            by = c("docs" = "tweet_id"))  %>%
  pull(code)

dfm_adjectives$code <-
  data.frame(docs = docnames(dfm_adjectives)) %>%
  left_join(
    labeled_tweets[, c("tweet_id", "code")], by = c("docs" = "tweet_id")) %>%
  pull(code)

# Inspect DFMs
dfm_tweets
topfeatures(dfm_tweets, n = 200)
textstat_frequency(dfm_tweets, n = 50, groups = code, ties_method = "max")

# Inspect DFM containing only adjectivess
dfm_adjectives
topfeatures(dfm_adjectives, n = 200)
textstat_frequency(dfm_adjectives, n = 50, groups = code, ties_method = "max")

# Anonymise tweet ids from the dfm and save
docnames(dfm_adjectives) <- paste("Tweet", seq_along(docnames(dfm_adjectives)))

# Uncomment
dfm_adjectives %>% saveRDS("results/dfm_adjectives.rds")

# WORDCLOUD ####################################################################

# Define function for calculating text keyness
calculate_keyness <- function(dfm,
                              target_code,
                              subset_by = vector(),
                              textstat_frequency = 100) {

  # Use subset to filter out codes
  if(length(subset_by) > 0) {

    dfm <- dfm_subset(dfm, ! code %in% subset_by)

    }

  keyness <- dfm %>%
    textstat_keyness(
      target = which(docvars(., "code") == target_code),
      measure = "chi2")

  # Maybe do slicing here: select 50/75/100 top features/group (some mutual)
  keyness <- dfm %>%
    textstat_frequency(n = textstat_frequency, groups = code) %>%
    select(feature) %>%
    left_join(keyness, by = "feature") %>%
    distinct(feature, .keep_all = TRUE) %>%
    arrange(desc(n_target))

  # Encode "overrepresentation" as relative term frequencies in target divided
  # over reference
  keyness_over <- keyness %>%
    mutate(
      frequency = (n_target + n_reference),
      relfreq_target =  (n_target + 0.1) / (frequency + 0.1),
      relfreq_reference = (n_reference + 0.1) / (frequency + 0.1),
      overrepresentation = log10((relfreq_target) / (relfreq_reference)))

  # Add a new column for words pasted with significance level indicators
  plot_data <- keyness_over %>%

    # Encode significance level to the feature displayed
    mutate(feature_sig = case_when(
      p < 0.001 ~ paste0(feature, "***"),
      p < 0.01 ~ paste0(feature, "**"),
      p < 0.05 ~ paste0(feature, "*"),
      .default = feature), .after = feature) %>%

    # Add boolean variable to highlight statistically significant terms
    mutate(significant = ifelse(p < 0.05, TRUE, FALSE))

  return(plot_data)

}

keyness_tweets <- dfm_adjectives %>%
  calculate_keyness(target_code = "P", # Compare positive to the rest
                    textstat_frequency = 200) # choose 200 most common terms

keyness_tweets <- keyness_tweets %>%
  arrange(p) %>%
  slice_max(frequency, n = 200, with_ties = FALSE) # up to 200 most common terms

glimpse(keyness_tweets)

keyness_tweets %>% slice_min(p, n = 10)

# Custom size breaks for the word cloud
size_breaks <- c(min(keyness_tweets$frequency),
                 seq(from = 25, to = max(keyness_tweets$frequency), by = 25),
                 max(keyness_tweets$frequency))

# Plot wordcloud
wordplot <- keyness_tweets %>%
  ggplot(aes(
    x = overrepresentation,
    y = chi2,
    label = feature_sig,
    size = frequency,
    colour = overrepresentation)) +

  geom_text_wordcloud(
    aes(alpha = 1 - p),
    grid_margin = 0.75, # default 1
    seed = 1234,
    eccentricity = 0.75,
    max_steps = 5,
    shape = "diamond", # default "circle"
    show.legend = TRUE) +

  scale_size_area(
    max_size = 10,
    breaks = size_breaks) +

  scale_colour_gradient(low =  "red", high = "blue") +


  # Group 1: right-pointing arrow and label
  annotate("segment", x = 0.3, xend = 1.15, y = -18, colour = "black",
           lineend = "butt", linejoin = "round", linewidth = 0.5,
           arrow = arrow(length = unit(4, "pt"), type = "closed")) +

  annotate("text", x = 1.2, y = -18, size = 3, hjust = "left",
           label = "Words more common in \n positive-stance tweets (blue)") +

  # Group 2: left-pointing arrow and label
  annotate("segment", x = -0.3, xend = -1.15, y = -18, colour = "black",
           lineend = "butt", linejoin = "round", linewidth = 0.5,
           arrow = arrow(length = unit(4, "pt"), type = "closed")) +

  annotate("text", x = -1.2, y = -18, size = 3, hjust = "right",
           label = "Words more common \nin rest of the tweets (red)") +

  theme_minimal() +

  labs(y = expression("Keyness (" ~ chi^{2} * ")"),
       x = expression("Overrepresentation (" * log[10] * ")")) +

  guides(colour = "none", size = "legend", Overrepresentation = "none",
         alpha = "none") +

  coord_cartesian(xlim = c(-3, 3), ylim = c(-20, 20))

# Save plotted files

ggsave(
  "plots/word_cloud_200adj_pos.jpg",
  plot = get_last_plot(),
  device = "jpeg",
  scale = 1,
  width = 170,
  height = 127,
  units = "mm",
  dpi = 800
)

ggsave(
  "plots/word_cloud_200adj_pos.tiff",
  plot = get_last_plot(),
  device = "tiff",
  scale = 1,
  width = 170,
  height = 127,
  units = "mm",
  dpi = 800
)

ggsave(
  "plots/word_cloud_200adj_pos1.svg",
  plot = get_last_plot(),
  device = "svg",
  scale = 1,
  width = 170,
  height = 127,
  units = "mm",
  dpi = 800
)
