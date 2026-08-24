# Tuomas Heikkilä, University of Helsinki
# tuomas.k.heikkila@helsinki.fi

# Preprocess tweets and replies

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# Uncomment when working in Puhti environment
# # In R, add the folder you created above to the list of directories where R will look for packages:
# .libPaths(c("/projappl/project_2010556/project_rpackages", .libPaths()))
#
# # Assign libpath
# libpath <- .libPaths()[1]

# Load packages
# install.packages("pacman")
pacman::p_load(dplyr,
               tidyr,
               readr,
               purrr,
               dtplyr,    # for lazy_dt
               textclean) # for cleanemojis

# Load and join datasets and 800 sampled labels
tweets <- readRDS("data/tweet_data_final.rds")
glimpse(tweets)

coded_tweets <- read_csv("data/labeled_tweets.csv", na = c("", "NA", "#N/A"))
glimpse(coded_tweets)

# Randomly select 800 tweets to form the training set
set.seed(987654321)

labeled_tweets <- coded_tweets %>%
  filter(check == "labeled") %>%
  filter(!grepl("!|\\?",flag)) %>%
  group_by(code) %>%
  # Sample 800 tweets equally distributed from each code
  slice_sample(prop = 0.597) %>%
  mutate(tweet_id = gsub("^id:", "", tweet_id)) %>%
  select(tweet_id, code) %>%
  rename("label" = "code")

labeled_tweets

labeled_tweets %>% group_by(label) %>% summarise(n = n())

tweets <- coded_tweets %>%
  mutate(tweet_id = gsub("^id:", "", tweet_id)) %>%
  select(tweet_id, code) %>%
  left_join(tweets, ., by = "tweet_id")

tweets <- labeled_tweets %>%
  left_join(tweets, ., by = "tweet_id") %>%
  mutate(label_indicator = ifelse(!is.na(label), 1, 0))

tweets <- tweets %>%
  distinct(tweet_id, .keep_all = TRUE)

glimpse(tweets)

# ...and replies
replies <- readRDS("data/replies_data_final.rds")
glimpse(replies)

coded_replies <- read_csv("data/labeled_replies.csv",
                            na = c("", "NA", "#N/A"))
glimpse(labeled_replies)

set.seed(987654321)

labeled_replies <- coded_replies %>%
  filter(check == "labeled") %>%
  filter(!grepl("!|\\?",flag)) %>%
  # Sample 800 replies equally distributed from each code
  slice_sample(prop = 0.582) %>%
  mutate(tweet_id = gsub("^id:", "", tweet_id)) %>%
  select(tweet_id, code) %>%
  rename("label" = "code")

labeled_replies

labeled_replies %>% group_by(label) %>% summarise(n = n())

labeled_replies

replies <- coded_replies %>%
  mutate(tweet_id = gsub("^id:", "", tweet_id)) %>%
  select(tweet_id, code) %>%
  left_join(replies, ., by = "tweet_id")

replies <- labeled_replies %>%
  left_join(replies, ., by = "tweet_id") %>%
  mutate(label_indicator = ifelse(!is.na(label), 1, 0))

replies <- replies %>%
  distinct(tweet_id, .keep_all = TRUE)

glimpse(replies)

###

keep_cols <- c("tweet_id", "code", "label", "label_indicator")

tweets %>% select(all_of(keep_cols)) %>%
  saveRDS("data/readme/inputs/labeled_tweets.rds")

replies %>% select(all_of(keep_cols)) %>%
  saveRDS("data/readme/inputs/labeled_replies.rds")

### PREPROCESS #################################################################

# Clean Twitter data following Stanford NLP preprocess procedure
# (Truncate the process for transformer based embedding)

# Handle emojis (transformers)
emojis <- lexicon::hash_emojis
emojis$y <- gsub("[[:space:]]|[[:punct:]]", "-", emojis$y)
emojis$y <- gsub("$", "-emoji", emojis$y)
emojis$y <- gsub("emoji-emoji", "emoji", emojis$y)

emojis

# df <- tweets

preprocess_transformer <- function(df) {

  # Reduce run time by turning data frame to data.table and back to tibble
  dtplyr::lazy_dt(df) %>%

    # Mark hyperlinks
    mutate(text = gsub("https?(://).*?(\\s+|$)", " http://t.co ", text)) %>%

    # Uniform quotation marks, 1
    mutate(text = gsub('“|”', '"', text)) %>%

    # Uniform quotation marks, 2
    mutate(text = gsub("‘|’", "'", text)) %>%

    # mutate(text = gsub("^\\s*(@.+?(\\s+))+", " [AT MULTIPLE USERS] ", text)) %>%
    # mutate(text = gsub("^\\s*@.+?(\\s+)", " [AT USER] ", text)) %>%

    # Mark users
    # mutate(text = gsub("@.+?(\\s+|$)", " <username> ", text)) %>%
    # mutate(text = gsub("@.+?(\\s+|$)", "@username ", text)) %>%
    mutate(text = gsub("@.+?(\\s+|$)", "", text)) %>%

    # Mark ellipsis
    mutate(text = gsub("…|\\.{3}", " ... ", text)) %>%

    # Remove double spaces
    mutate(text = gsub("\\s{2,}", " ", text)) %>%

    # Remove lerading and trailing spaces
    mutate(text = trimws(text, which = "both")) %>%

    # Convert back to a tibble
    as.data.frame() %>% as_tibble()
}

preprocess_transformer_markemojis <- function(df) {

  # Reduce run time by turning data frame to data.table
  dtplyr::lazy_dt(df[1:10]) %>%

  # Mark emoticons
  mutate(text = gsub("[8:=;]+['`\\-]+[)d]+", " <smile emoticon> ", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[(d]+", " <sadface emoticon> ", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[\\|d]+", " <neutralface emoticon> ", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[pP]+", " <lolface emoticon> ", text)) %>%
    mutate(text = gsub(r"(/<3/)", " <heart emoticon> ", text)) %>%

    # Mark emojis
    mutate(text = textclean::replace_emoji(text, emoji_dt = emojis)) %>%

    # Remove double spaces
    mutate(text = gsub("\\s{2,}", " ", text)) %>%

    # Remove lerading and trailing spaces
    mutate(text = trimws(text, which = "both")) %>%

    # # Convert back to a tibble
    as.data.frame() %>% as_tibble() %>%

    # Run preprocess transformers procedure
    preprocess_transformer() %>%

    # Remove all remaining hex codes (<00><00><00>) produced by textclean...
    mutate(text = gsub("(\\s{0,}<.{,2}>)+", "", text))
}

preprocess_glove <- function(df) {

  # Reduce run time by turning data frame to data.table
  dtplyr::lazy_dt(df) %>%

    # Mark all caps
    mutate(text = gsub("([A-Z]{2,})", "<ALLCAPS> \\1", text)) %>%

    # Mark hyperlinks
    mutate(text = gsub("https?(://).*?(\\s+|$)", " <URL> ", text)) %>%

    # Mark emoticons
    mutate(text = gsub("[8:=;]+['`\\-]+[)d]+", "<SMILE>", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[(d]+", "<SADFACE>", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[\\|d]+", "<NEUTRALFACE>", text)) %>%
    mutate(text = gsub("[8:=;]+['`\\-]+[pP]+", "<LOLFACE>", text)) %>%
    mutate(text = gsub(r"(/<3/)", " <HEART> ", text)) %>%

    # Uniform quotation marks, 1
    mutate(text = gsub('“|”', '"', text)) %>%

    # Uniform quotation marks, 2
    mutate(text = gsub("‘|’", "'", text)) %>%

    # Mark repeated punctuation
    mutate(text = gsub("([[:punct:]])\\1{2,}", "\\1 <REPEAT> ", text)) %>%

    # Force splitting words appended with slashes
    mutate(text = gsub("/", " / ", text)) %>%

    # Force splitting words appended with hyphens
    mutate(text = gsub("-", " - ", text)) %>%

    # Force splitting words appended with plus signs
    mutate(text = gsub("\\+", " + ", text)) %>%

    # Mark hashtags
    mutate(text = gsub("#", "<HASHTAG> ", text)) %>%

    # Mark retweets
    mutate(text = gsub("RT\\s{1,}", "<RETWEET> ", text)) %>%

    # Mark users
    mutate(text = gsub("@.+?(\\s+|$)", " <USER> ", text)) %>%

    # Mark numbers, 1
    mutate(text = gsub(r"(/[-+]?[.\d]*[\d]+[:,.\d]*/)", " <NUMBER> ", text)) %>%

    # Mark numbers, 2
    mutate(text = gsub("\\d{1,}", " <NUMBER> ", text)) %>%

    # Mark ellipses
    mutate(text = gsub("…|\\.{3}", " … ", text)) %>%

    # Mark elongated words (e.g. "wayyyy" => "way <ELONG>")
    mutate(text = gsub("([[:alpha:]])\\1{2,}", "\\1 <ELONG>", text)) %>%

    # Split punctuation TRY WITHOUT
    mutate(text = gsub("([[:punct:]])", " \\1 ", text)) %>%

    # Split punctuation, not all, 1
    mutate(text = gsub("'\\s{1}s", "'s", text)) %>%

    mutate(text = gsub("n\\s{1}'\\s{1}t", "n't", text)) %>%

    # Split punctuation, not all, 1
    mutate(text = gsub("\\s{1}>", ">", text)) %>%

    # Split punctuation, not all, 2
    mutate(text = gsub("<\\s{1}", "<", text)) %>%

    # Split punctuation, not all, 3
    mutate(text = gsub("'\\s{1}", "'", text)) %>%

    # # Remove double spaces
    mutate(text = gsub("\\s{2,}", " ", text)) %>%

    # Remove lerading and trailing spaces
    mutate(text = trimws(text, which = "both")) %>%

    # Turn to lower case
    mutate(text = tolower(text)) %>%

    # Convert back to a tibble
    as.data.frame() %>% as_tibble()

}

# Run preprocess procedures and save cleaned datasets, tweets
tweets2transformers <- preprocess_transformer(tweets)
tweets2transformers_verbose <- preprocess_transformer_markemojis(tweets)
tweets2glove <- preprocess_glove(tweets)

# Run preprocess procedures and save cleaned datasets, replies
replies2transformer <- preprocess_transformer(replies)
replies2transformers_verbose <- preprocess_transformer_markemojis(replies)
replies2glove <- preprocess_glove(replies)
