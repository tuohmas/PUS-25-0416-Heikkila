# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
#
# Script for creating smart word clouds; Illustrate differences in vocabularies
# Inspired by Johannes Gruber: https://www.johannesbgruber.eu/post/smarter-wordclouds/
#
#
# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages
if(!requireNamespace("pacman")) {
  install.packages("pacman")
  library(pacman) }

# Load packages
pacman::p_load(dplyr,      # For data manipulation
               tidyr,      # For data manipulation
               purrr,      # For data manipulation
               readr,      # For data manipulation
               quanteda,
               spacyr,
               textstem,   # TEMP?
               quanteda.textstats,  # textstat_frequency()
               ggplot2,
               ggwordcloud)

sessionInfo()

# Load data
labeled_tweets <- readRDS("data/clean_labeled_tweets.rds")
# labeled_replies <- readRDS("data/clean_labeled_replies.rds")

  # read_csv("2 REPLIES (social correction) - codes_correction.csv") %>%
  # filter(!grepl("del|discard", flag, ignore.case = T))

# BRANCH DEL

# ggplot(labeled_replies, aes(label = term, size = freq, color = term)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = 15, trans = power_trans(1/.5)) +
#   theme_minimal()
#
# library(ggwordcloud)
#
# # Data
# df <- thankyou_words_small
#
# set.seed(1)
# ggplot(df, aes(label = word, size = speakers, color = name)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = 20) +
#   theme_minimal()

# / BRANCH

# TEXT PREPROCESS ##############################################################

# Define a function to tokenize and lemmatize tweet corpus; additional, optional
# functionalities for removing stopwords, trimming corpus, and collocating
# compound words

# data <- labeled_tweets

text_cleanup <- function(data,  # Exported and annotated df with text and doc id
                         remove_stopwords = FALSE,
                         stop_words = stopwords::stopwords("en"),
                         word_collocations = FALSE,
                         collocations_min_count = 3,
                         collocations_p = 0.001,
                         dfm_trim = FALSE,
                         dfm_tfidf = FALSE) {

  # Reduce and filter data frame
  keep_cols <- c("tweet_id", "text", "label", "code")

  data <- data %>%
    # filter(check == "labeled") %>%
    select(all_of(keep_cols))

  # Clean, tokenize and pre-process
  tokens <- data %>%
  # tc <- data %>%

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

    # TEMP TEMP TEMP

    # Create corpus
    # corpustools::udpipe_tcorpus(
    #   x = .$text,
    #   model = "english-ewt",
    #   doc_id = .$tweet_id)

  # tc = create_tcorpus(sotu_texts[c(1:5,801:805),], doc_col='id')

  # tcs1 = subset(tc, token_id < 20)

  # tc_adjectives = subset(tc, POS == 'ADJ')

  # dfm = corpustools::get_dfm(tc_adjectives, "lemma")

    # Create corpus
    corpus(docid_field = "tweet_id", text_field = "text") %>%


    # Remove punctuation, symbols, and numbers; retain padding for later
    quanteda::tokens(what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     padding = word_collocations)

    # # Lowercase
    # tokens_tolower()

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

    # Keep collocations above critical value for 99.9% confidence level
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

# Remove
study_data <- read_csv("data/retracted_studies.csv")

included_studies <- read_lines("data/included_studies.txt") %>%
  gsub("10.21203/rs\\.3\\.rs-100956/.*", "10.21203/rs.3.rs-10095", .) %>%
  unique()

# discard_doi = c("10.1016/S0140-6736(20)31180-6",
#                 "10.21203/rs.3.rs-109670/v1",
#                 "10.4269/ajtmh.20-0873")

# Add when was the tweet posted relative to the study's publication date
study_names <- study_data %>%
  # filter(!doi %in% discard_doi)) %>%
  filter(doi %in% included_studies) %>%
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

glimpse(clean_study_names)

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

# TEMP DISCARD
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1435759330368835587")
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1412763555020128266")
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1429759785268883461")
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1395782723076337673")
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1413420493240217604")
# test_tweet <- labeled_tweets %>% filter(tweet_id == "1427712948584206342")
#
# test_tweet %>%
#
# # Clean Twitter specific expressions and badly encoded characters
# mutate(text = gsub("[AT MULTIPLE USERS]", "", text, fixed = TRUE)) %>%
#   mutate(text = gsub("[LINK]", "", text, fixed = TRUE)) %>%
#   mutate(text = gsub("#", "", text)) %>%
#   mutate(text = gsub("@.+?\\b|@.+?\\.|@.+?,", "", text)) %>%
#   mutate(text = gsub("&lt;", ">", text)) %>%   # Less than
#   mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
#   mutate(text = gsub("&gt;", "<", text)) %>%   # Greater than
#   mutate(text = gsub("’", "'", text)) %>%   # Greater than
#
#   mutate(text = gsub("\\?|\\)|\\(", "", text)) %>%
#   mutate(text = tolower(text)) %>%
#   mutate(text = gsub(
#     paste(clean_study_names$title, collapse = "|"), "", text)) %>%
#
#   select(text)

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

# TEMP: See whether there are any adjectives we should keep (None found)
my_stopwords %>% data.frame(word = .) %>%
  left_join(tidytext::parts_of_speech) %>%
  filter(pos %in% "Adjective") %>%
  pull(word) %>%
  unique

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

data.frame(docs = docnames(dfm_adjectives)) %>%
  left_join(labeled_tweets[, c("tweet_id", "code")],
            by = c("docs" = "tweet_id")) %>%
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

# dfm_replies
# topfeatures(dfm_replies, n = 100)
# textstat_frequency(dfm_replies, n = 50, groups = code, ties_method = "max")

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
      relfreq_target =  (n_target + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
      relfreq_reference = (n_reference + 0.1) / (frequency + 0.1), # xlims: -2, 2 (log10)
      overrepresentation = log10((relfreq_target) / (relfreq_reference)))

  # Add a new column for words pasted with significance level indicators
  plot_data <- keyness_over %>%
    # slice_max(frequency, n = 150, with_ties = FALSE) %>% # Top words up to N # Redundant
    # Encode significance level to the feature displayed
    mutate(feature_sig = case_when(
      p < 0.001 ~ paste0(feature, "***"),
      p < 0.01 ~ paste0(feature, "**"),
      p < 0.05 ~ paste0(feature, "*"),
      .default = feature), .after = feature) %>%

    # Add boolean variable to indicate significance (to highlight w/ disc. alpha)
    mutate(significant = ifelse(p < 0.05, TRUE, FALSE))

  return(plot_data)

}

# Apply to tweets
# keyness_tweets <- dfm_tweets %>%
#   calculate_keyness(target_code = "P", # Compare negative to positive (reference)
#                     # subset_by = c("A", "U"), # Remove Neutral and Unclassified
#                     textstat_frequency = 200)

keyness_tweets <- dfm_adjectives %>%
  calculate_keyness(target_code = "P", # Compare negative to positive (reference)
                    # subset_by = c("A", "U"), # Remove Neutral and Unclassified
                    textstat_frequency = 200)

keyness_tweets <- keyness_tweets %>%
  arrange(p) %>%
  slice_max(frequency, n = 200, with_ties = FALSE)

glimpse(keyness_tweets)

keyness_tweets %>% slice_min(p, n = 10)

# keyness_tweets %>%
#   arrange(desc(chi2)) %>%
#   head(n = 50)
#
# keyness_tweets %>%
#   arrange(chi2) %>%
#   head(n = 50)

# Apply to relies
# keyness_replies <- dfm_replies %>%
#   calculate_keyness(target_code = "1",
#                     textstat_frequency = 100) %>%
#   # arrange(desc(relfreq_target))
#   arrange(desc(relfreq_reference))

# head(keyness_replies, n = 100)

# plot_data <- keyness_tweets

# PLOT AS A FUNCTION
wordcloud_plot <- function(plot_data) {

  # TEMP
  # size_breaks <- c(min(plot_data$frequency), 50,
  #                  seq(from = 100, to = max(plot_data$frequency), by = 100),
  #                  max(plot_data$frequency))

  size_breaks <- c(min(plot_data$frequency),
                   seq(from = 25, to = max(plot_data$frequency), by = 25),
                   max(plot_data$frequency))


  # Plot
  wordplot <- plot_data %>%
    ggplot(aes(
      x = overrepresentation,
      y = chi2,
      label = feature_sig,
      size = frequency,
      colour = overrepresentation)) +

    # Plot word cloud
    geom_text_wordcloud(
      # aes(alpha = significant), # TEMP
      aes(alpha = 1 - p),
                        grid_margin = 0.75, # default 1
                        seed = 1234,
                        eccentricity = 0.75,
                        max_steps = 5,
                        shape = "diamond", # default "circle"
                        show.legend = TRUE) +

    # TEMP
    # "Highlight" statistically significant values of chi2 (alpha = 0.05)
    # scale_alpha_discrete(range = c(0.5, 1)) +

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
           alpha = "none"
           # alpha = guide_legend("p-value") # Not working, wrong order
           ) +

    coord_cartesian(xlim = c(-3, 3), ylim = c(-20, 20))

  return(wordplot)

}

# set.seed(4321)

# data("love_words_latin_small")
#
# ggplot(love_words_latin_small, aes(label = word, size = speakers)) +
#   geom_text_wordcloud(eccentricity = 0.75, max_steps = 5) +
#   scale_size_area(max_size = 20) +
#   theme_minimal()

# Optional: Trim outliers
# keyness_tweets <- keyness_tweets %>% filter(between(chi2, -100, 100))
# keyness_tweets <- keyness_tweets %>% filter(frequency > 2)
# keyness_tweets <- keyness_tweets %>% filter(frequency < 300)

# keyness_tweets %>% nrow()

# Plot data
wordcloud <- wordcloud_plot(keyness_tweets)

wordcloud

wordcloud$data

discard_data <- c("feature", "n_target", "n_reference",
                  "relfreq_target", "relfreq_reference", "significant")

figure_raw_data <-  wordcloud$data %>%
  select(-all_of(discard_data)) %>%
  rename("feature" = "feature_sig") %>%
  write_excel_csv("data/figure_raw_data.csv")

figure_raw_data

figure_plotted_data <- wordcloud$data %>%
  rename("y" = "chi2",
         "size" = "frequency",
         "x" = "overrepresentation") %>%
  mutate(alpha = 1 - p) %>%
  select(feature, size, x, y, alpha) %>%
  write_excel_csv("data/figure_plotted_data.csv")

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

# FINISHED
