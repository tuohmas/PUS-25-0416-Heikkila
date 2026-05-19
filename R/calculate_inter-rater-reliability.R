# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Calculate inter-coder reliability scores between two raters based on the final
# round of test coding on 150 tweets and 150 replies

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation, restrict fractions to three decimal places
options(scipen = 999, digits = 3)

# Install and load packages
if(!require("pacman")) { install.packages("pacman") }

pacman::p_load(dplyr,
               tidyr,
               readr,
               krippendorffsalpha,
               snow,
               DescTools,
               irr,
               psych)

sessionInfo()

### INTER-CODER RELIABILITY MEASURES ###########################################

# Custom function for basic pre-processing when reading csv files
read_and_preprocess <- function(file, rater = 1) {

  label_info <- data.frame(
    code = as.factor(c("P", "N", "A", "U", 1, 0, NA)), # Recorded codes
    label = c("Positive stance",                   # Intelligible codes
                   "Negative stance",
                   "Neutral stance",
                   "Undefined stance",
                   "User correction",
                   "Other",
                   NA),
    # Numeric values for calculations
    num = c(1, -1, 0, 99, 1, 0, NA))

  tweet_ids <- read_csv("data/test_sample_ids.csv", col_types = "cc")

  df <- read_csv(file, na = c("", "NA", "#N/A")) %>%
    select(-label) %>%
    mutate(code = as.factor(code)) %>%
    left_join(label_info, by = "code") %>%
    left_join(tweet_ids, by = "id") %>%

     # Clean and anonymise data
    select(id, tweet_id, type, study, label, num) %>%
    rename_with(
      ~ paste0("rater", rater, "_", .x), ends_with(c("label", "num")))

  return(df)

}

# Original data available upon request
# rater1 <- read_and_preprocess("data/test_sample_rater1.csv", rater = 1)
# rater2 <- read_and_preprocess("data/test_sample_rater2.csv", rater = 2)

raters <- inner_join(
  rater1, rater2 %>% select(id, starts_with("rater2")), by = "id") %>%
  relocate(rater1_num, .before = rater2_num)

# raters %>% write_csv("data/anonymised_test_codes.csv")
# raters <- read_csv("data/anonymised_test_codes.csv")

#### Agreement on stance (tweets) ##############################################

# Calculate percentage agreements
raters_on_stance <- raters %>% filter(type == "tweets")

agree_on_stance <- DescTools::Agree(
  raters_on_stance[,c("rater1_num", "rater2_num")], tolerance = 0, na.rm = TRUE)

agree_on_stance

# Use krippendorffsalpha (Hughes, 2021) to calculate Krippendorff's alpha
set.seed(1234)

kalpha_on_stance <- krippendorffs.alpha(
  data = as.matrix(raters_on_stance[, c("rater1_num", "rater2_num")]),
  level = "nominal", verbose = TRUE, method = "customary",
  # Calculate conf. intervals with 5,000 bootstrap samples
  control = list(parallel = TRUE, bootit = 5000, nodes = 2))

summary(kalpha_on_stance)

# Use psych to calculate Cohen's kappa
kappa_on_stance <- psych::cohen.kappa(
  x = as.matrix(raters_on_stance[, c("rater1_num", "rater2_num")]),
  alpha = 0.05)

kappa_on_stance
kappa_on_stance$kappa
kappa_on_stance$agree

# Show units where coders disagreed
disagreements_on_stance <- raters_on_stance %>%
  filter(rater1_num != rater2_num)

disagreements_on_stance

# Collate statistics on stance (tweets)
results <- data.frame(
  type = "tweets",
  units = attr(agree_on_stance, "subjects"),
  agreement = agree_on_stance[1],
  disagree_n = nrow(disagreements_on_stance),
  kalpha = kalpha_on_stance$alpha.hat,
  kalpha_lower = confint(kalpha_on_stance, level = 0.95)[1],
  kalpha_upper = confint(kalpha_on_stance, level = 0.95)[2],
  kappa = kappa_on_stance$kappa,
  kappa_lower = kappa_on_stance$confid[2, "lower"],
  kappa_upper = kappa_on_stance$confid[2, "upper"])

#### Agreement on corrections (replies) ########################################

raters_on_corrections <- raters %>% filter(type == "replies")

# Calculate precentage agreements
agree_on_corrections <- DescTools::Agree(
  raters_on_corrections[, c("rater1_num", "rater2_num")], tolerance = 0, na.rm = TRUE)

agree_on_corrections[1]

# Use krippendorffsalpha (Hughes, 2021) to calculate Krippendorff's alpha
set.seed(1234)

kalpha_on_corrections <- krippendorffs.alpha(
  data = as.matrix(raters_on_corrections[, c("rater1_num", "rater2_num")]),
  level = "nominal", verbose = TRUE, method = "customary",
  # Calculate conf. intervals with 5,000 bootstrap samples
  control = list(parallel = TRUE, bootit = 5000, nodes = 2))

# Print summary
summary(kalpha_on_corrections)

# Use psych to calculate Cohen's kappa
kappa_on_corrections <- psych::cohen.kappa(
  x = as.matrix(raters_on_corrections[, c("rater1_num", "rater2_num")]),
  alpha = 0.05)

kappa_on_corrections
kappa_on_corrections$kappa
kappa_on_corrections$agree

# Show units where coders disagreed
disagreements_on_corrections <- raters_on_corrections %>%
  filter(rater1_num != rater2_num)

disagreements_on_corrections

# Collate statistics, bind to previous
results <- rbind(results, data.frame(
  type = "replies",
  units = attr(agree_on_corrections, "subjects"),
  agreement = agree_on_corrections[1],
  disagree_n = nrow(disagreements_on_corrections),
  kalpha = kalpha_on_corrections$alpha.hat,
  kalpha_lower = confint(kalpha_on_corrections, level = 0.95)[1],
  kalpha_upper = confint(kalpha_on_corrections, level = 0.95)[2],
  kappa = kappa_on_corrections$kappa,
  kappa_lower = kappa_on_corrections$confid[2, "lower"],
  kappa_upper = kappa_on_corrections$confid[2, "upper"])
  )

row.names(results) <- NULL

results

# Uncomment
if(!file.exists("results")) {
  dir.create(file.path(getwd(), "results")) }

results %>% write_csv("results/inter-rater-reliability-scores.csv")
