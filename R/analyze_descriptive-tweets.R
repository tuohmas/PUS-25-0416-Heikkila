# 30 Jan, 2023
# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Descriptive analysis based on tweets and replies dataframes

# Load up packages
pacman::p_load(tidyverse,
               vroom,
               checkpoint,
               academictwitteR)

checkpoint("2023-01-30", r_version="4.2.2")


# FIXME Things to add
# 02-08: Expressions of concern! First major editorial notice to redacted_studies
# dataframe (from Retraction Watch Database)
# 02-08: Ridge plots per study showing the distribution before and after the redaction
# (Do a mock-up image here so not to forget: "Ennen submission(...).pptx")
#
# Problem for 8.2.2023 How to pair replies with
# A) Search for tweets that have replies, count > 0
# B) Match replies from data/replies_data.csv

filt <- tweet_data %>% filter(lang == "en")

replies <- filt %>% filter(!is.na(in_reply_to_user_id) &
                             !is.na(sourcetweet_author_id))

nrow(replies)

replies_pre_redaction <- replies %>% filter(tweeted_post_redaction == FALSE)
replies_post_redaction <- replies %>% filter(tweeted_post_redaction == TRUE)

nrow(replies_pre_redaction)
nrow(replies_post_redaction)

names(replies_pre_redaction)

tidy_pre_replies <-
  replies_pre_redaction %>%
  mutate(clean_text = gsub("@([a-zA-Z0-9_]{1,50})", "", text)) %>%
  mutate(clean_text = gsub("\\s+", " ", str_trim(clean_text))) %>%
  mutate(clean_source_text = gsub("@([a-zA-Z0-9_]{1,50})", "", sourcetweet_text)) %>%
  mutate(clean_source_text = gsub("\\s+", " ", str_trim(clean_source_text))) %>%
  select(conversation_id, tweet_id, sourcetweet_author_id, clean_source_text,
         user_username, clean_text, created_at, study_citation,
         tweeted_post_redaction, reply_count)

set.seed(0)

sample_pre_replies <-
  tidy_pre_replies[sample(nrow(tidy_pre_replies), 50),]

View(sample_pre_replies)

#### Clean tweets ####
tweets <-
  tweets %>%

  # Remove links, mentions <- and hashtag signs; remove stop words (first time)
  mutate(clean_text = tolower(text)) %>%
  mutate(clean_text = gsub("https:\\/\\/.*\\/[A-Za-z0-9_]+", "",
                           clean_text)) %>%
  mutate(clean_text = removeWords(clean_text, stop_words$word)) %>%
  mutate(clean_text = gsub("@([a-zA-Z0-9_]{1,50})", "", clean_text)) %>%

  # Remove some stop words with multiple spellings
  mutate(clean_text = gsub("hydrox.+?", " ", clean_text)) %>%
  mutate(clean_text = gsub("ivermec.+?", " ", clean_text)) %>%
  mutate(clean_text = gsub("covid.+?", " ", clean_text)) %>%
  mutate(clean_text = gsub("corona.+?", " ", clean_text)) %>%

  # Remove punctuation, numbers and extra whitespaces
  mutate(clean_text = gsub("[[:punct:]]", " ", clean_text)) %>% # Remove punctuation
  mutate(clean_text = gsub("[^[:alnum:]]", " ", clean_text)) %>% # ...other non-alphabetic
  mutate(clean_text = gsub("[[:digit:]]", " ", clean_text)) %>% # and numbers

  # Finally remove stop words (again) and clean up extra whitespaces
  mutate(clean_text = removeWords(clean_text, stop_words$word)) %>%
  mutate(clean_text = gsub("\\s+", " ", str_trim(clean_text)))



# FIXME

# Explore restored tweets
restored_tweets <-
  bind_tweets("data/restored_tweets", output_format = "tidy") %>%
  distinct(tweet_id, .keep_all = TRUE)

restored_tweets_raw <-
  bind_tweets("data/restored_tweets") %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(restored_tweets$tweet_id)

restored_tweets_raw <- restored_tweets_raw %>%
  unpack(cols = c(entities)) # unnest entities

glimpse(restored_tweets)
glimpse(restored_tweets_raw)

restored_tweets <- restored_tweets %>%
  mutate(sourcetweet_type = ifelse(
    !is.na(in_reply_to_user_id), "replied", sourcetweet_type),
         sourcetweet_type = ifelse(
     is.na(sourcetweet_type), "original_tweet", sourcetweet_type))

table(restored_tweets$sourcetweet_type)


# For English tweets: how many tweeters and what tweet types
filt_restored_tweets <-
  restored_tweets %>%
  filter(lang == "en")

glimpse(filt_restored_tweets)

# attach(filt_restored_tweets)
attach(restored_tweets)

unique(author_id) %>% length()
unique(sourcetweet_id) %>% length()

retweeted_authors <-
  restored_tweets %>%
  filter(sourcetweet_type == "retweeted") %>%
  select(sourcetweet_author_id) %>% table() %>%
  as.data.frame() %>% arrange(desc(Freq)) %>% as.data.frame()

retweeted_authors$sourcetweet_author_id[i]

names(retweeted_authors)

i <- 1

# start_tweets <- "2020-01-01T00:00:00Z"
start_tweets <- "2021-01-18T00:00:00Z"
end_tweets <- "2023-01-19T00:00:00Z"
# in_between_days <- as.Date(end_tweets) - as.Date(start_tweets)
in_between_days <- 731

all_tweet_counts <- data.frame(matrix(nrow = in_between_days, ncol = 0))
all_retweet_counts <- data.frame(matrix(nrow = in_between_days, ncol = 0))

for (i in 1:nrow(retweeted_authors)) {

tweet_counts <-
  count_all_tweets(
  query = paste0("from:",
                 as.character(retweeted_authors[i,"sourcetweet_author_id"])),
  start_tweets = start_tweets,
  end_tweets = end_tweets,
  n = in_between_days,
  data_path = paste0("data/tweet_counts/restored_tweets/from_author_id_",
  retweeted_authors[i,"sourcetweet_author_id"]),
  granularity = "day") %>%
  arrange(desc(start)) %>%
  mutate(author_id = as.character(
    retweeted_authors[i,"sourcetweet_author_id"])) %>%
  select(-end)

all_tweet_counts <- cbind(tweet_counts, all_tweet_counts)

View(all_tweet_counts)

library(academictwitteR)

retweet_counts <-
  count_all_tweets(
    query = paste0("retweets_of:",
                   as.character(retweeted_authors[i,"sourcetweet_author_id"])),
    start_tweets = start_tweets,
    end_tweets = end_tweets,
    n = in_between_days,
    data_path = paste0("data/tweet_counts/restored_tweets/retweets_of_author_id_",
                       retweeted_authors[i,"sourcetweet_author_id"]),
    granularity = "day") %>%
  arrange(desc(start)) %>%
  mutate(author_id = as.character(
    retweeted_authors[i,"sourcetweet_author_id"])) %>%
  select(-end)

all_retweet_counts <- cbind(retweet_counts, all_retweet_counts)

View(all_retweet_counts)

}

all_tweet_counts


# FIXME Use a) suppelemented tweet data; b) correct redaction dates
tweets_col_types <- "cccccc???c????????c????????c??c?????" # stringify id nums

tweets_col_types <- cols(tweet_id = "c",
                         user_username = "c",
                         text = "c",
                         lang = "c",
                         conversation_id = "c",
                         author_id = "c",
                         source = "c",
                         possibly_sensitive = "l",
                         created_at = "T",
                         in_reply_to_user_id = "c",
                         user_verified = "l",
                         user_location = "c",
                         user_name = "c",
                         user_description = "c",
                         user_profile_image_url = "c",
                         user_protected = "l",
                         user_created_at = "T",
                         user_url = "c",
                         user_pinned_tweet_id = "c",
                         retweet_count = "i",
                         like_count = "i",
                         quote_count = "i",
                         user_tweet_count = "i",
                         user_list_count = "i",
                         user_followers_count = "i",
                         user_following_count = "i",
                         sourcetweet_type = "c",
                         sourcetweet_id = "c",
                         sourcetweet_text = "c",
                         sourcetweet_lang = "c",
                         sourcetweet_author_id = "c",
                         study_doi = "c",
                         study_citation = "c",
                         tweeted_post_redaction = "l",
                         reply_count = "i",
                         hashtags = "c")

tweet_data <- read_delim("data/tweet_data.csv",
                      col_types = tweets_col_types)

glimpse(tweet_data)

studies <- read_csv("data/redacted_studies.csv")

studies$editorial_notice_date[1] %>% typeof()
tweet_data$created_at[1] %>% typeof()

tweet_data$study_doi

View(tweet_data)

studies$doi

tweet_data$editorial_notice_date %>% unique()

tweet_data$editorial_notice_date <- as.Date("2020-01-01")

# CONTINUE FROM HERE! DONE

# source("R/coltypes.R")
#
# tweet_data <- read_csv("data/tweet_data.csv", col_types = tweets_col_types)
# studies <- read_csv("data/redacted_studies.csv")
#
# glimpse(tweet_data)
# glimpse(studies)
#
# tweet_data <- as.data.frame(tweet_data)
# studies <- as.data.frame(studies)
#
# i <- 1
#
# for(i in 1:nrow(tweet_data)){
#
#   tweet_data$tweeted_post_redaction[i] <-
#     ifelse(studies[studies$doi == tweet_data$study_doi[i],
#               "editorial_notice_date_first"] <= tweet_data$created_at[i],
#       TRUE, FALSE)
# }
#
# View(tweet_data)
# glimpse(tweet_data)
#
# table(tweet_data$tweeted_post_redaction)
#
# write_csv(tweet_data, "data/tweet_data.csv")

# Check
tweet_data[tweet_data$tweeted_post_redaction, c("created_at", "study_doi")]

tweet_data$editorial_notice_date <- as.Date(tweet_data$editorial_notice_date)

  tweet_data$tweeted_post_redaction[i] <-
    ifelse(tweet_data$created_at >= )

}

tweet_data <-
  left_join(
    tweet_data,
    studies[,c("doi","editorial_notice_date")],
    by = c("study_doi" = "study_doi",
           "doi" = "doi"))

tweet_data

all(tweet_data$study_doi %>% unique() == studies$doi %>% unique())

glimpse(tweet_data)
nrow(tweet_data)

filt_tweets <- filter(tweet_data, lang == "en")

nrow(filt_tweets)
glimpse(filt_tweets)

# Frequencies by study by phase (pre- and post-redaction)

""
# How many users tweeted more than one study?
# ... breakdown per pre- and post-redactions

# Columns:

study_usage <- filt_tweets %>%

  mutate(study_citation = gsub(r"( \(version \d\))", "", study_citation)) %>%

  group_by(author_id, study_citation) %>%

  summarise(mentions = n()) %>%

  arrange(desc(mentions))

# How many mentions per study?
study_usage$study_citation %>% table()

study_usage_wide <-
  study_usage %>% spread(key = study_citation, value = mentions, fill = 0) %>%
  mutate(row_sum = rowSums(.!=0))

View(study_usage)

tweet_data$author_id
tweet_data$study_citation %>% table()

# Prepare: combine different versions  of the same paper (sum rows)
tweet_samples <- tweet_samples %>%
  mutate(study_citation = gsub(r"( \(version \d\))", "", study_citation),
         study_doi = gsub(r"(/v\d)", "", study_doi)) %>%

  group_by(study_citation, study_doi) %>%

  summarise(across(c("tweets_in_altmeric_explored",
                     "tweets_collected_november_2022",
                     "tweets_collected_january_2023"), sum))






# Clean the environment
# rm(list = ls())
