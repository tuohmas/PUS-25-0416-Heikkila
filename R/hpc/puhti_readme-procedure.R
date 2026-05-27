# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for running Automated Nonparametric Content Analysis with Readme2

## Login overriding the default MAC algorithm
# ssh -m hmac-sha2-512 tuomheik@puhti.csc.fi

## Create a folder for your R packages in /projappl
# cd /projappl/project_2010556
# mkdir project_rpackages

## Launch interactive mode with default memory and local scratch space. for example:
# sinteractive --account project_2010556 --cores 2 --time 02:00:00 --mem 8000 --tmp 100

## Go to projappl and activate r-readme
# cd /projappl/project_2010556
# module load python-data
# source /projappl/project_2010556/r-readme/bin/activate

# Start R
# module load r-env
# start-r

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# In R, add the folder you created above to the list of directories where R will look for packages:
.libPaths(c("/projappl/project_2010556/project_rpackages", .libPaths()))

# Assign libpath
libpath <- .libPaths()[1]

# Install packages
if(!require("tensorflow")) {
  remotes::install_github("rstudio/tensorflow", dependencies = TRUE,
                          upgrade = "always", lib = libpath) }

if(!require("readme")) {
  remotes::install_github("iqss-research/readme-software/readme",
                          dependencies = TRUE,
                          upgrade = "always", lib = libpath) }

# r_pkgs <- c("dplyr", "tidyr", "reticulate", "text", "pacman", "httr2",
#             "polite", "data.table")

# install.packages(r_pkgs, dependencies = TRUE, lib = libpath)

# Tee interactive
install.packages(doParallel)

# Load other packages
pacman::p_load(dplyr,
               tidyr,
               reticulate,
               tensorflow,
               text,
               readme,
               doParallel)

sessionInfo()

# Unset Environment variable RETICULATE_PYTHON, if one exists
if(nchar(Sys.getenv("RETICULATE_PYTHON")) > 0) {
  Sys.unsetenv("RETICULATE_PYTHON") }

envname <- "./r-reticulate"

# Create new virtual environment to home path, if necessary
if(!virtualenv_exists(envname)) {

  virtualenv_create(envname = envname, version = "3.10.11",
                    packages = NULL, module = "venv",
                    system_site_packages = FALSE)

  # Install tensorflow and dependencies to virtual env
  tensorflow::install_tensorflow(method = "virtualenv",
                                 envname = envname,
                                 extra_packages = "keras",
                                 restart_session = FALSE,
                                 python_version = "3.10.11")

  # Install tensorflow and dependencies to virtual env
  textrpp_install_virtualenv(envname = envname, prompt = FALSE)
}

# Activate virtual env
use_virtualenv(envname)

# If anything else needs to be installed
# virtualenv_install(envname, py_pkgs, ignore_installed = TRUE,
#                    python_version = "3.10.11")

# Testing whether TensorFlow is working
# tensorflow::tf$constant("Hello world")

# Initialize and test text package
textrpp_initialize(virtualenv = envname, refresh_settings = TRUE,
                   save_profile = FALSE, textEmbed_test = FALSE, check_env = FALSE)

### TEST

# Use parallel processing
tensorflow::use_session_with_seed(seed = 12345, disable_parallel_cpu = FALSE)

# Detect and allocate cores


### LOAD TWITTER GLOVE WORD EMBEDDINGS #########################################

# Download Standford NLP word embeddings of Twitter, if necessary

out_dir <- "/scratch/project_2010556/readme/models/glove"

if(!"glove.twitter.27B.200d.txt" %in% list.files(out_dir)) {

  pacman::p_load(polite, httr2)

  # Wrap request function inside politely (good manners)
  politely_req <- politely(httr2::request, verbose = TRUE)

  req <- politely_req("https://nlp.stanford.edu/data/wordvecs/")

  # Build query
  query <- req %>%
    req_url_path_append("glove.twitter.27B.zip") %>%
    req_progress() %>%                   # Monitor progress
    req_user_agent(paste("polite",
                         "(email:tuomas.k.heikkila@helsinki.fi)", # Replace
                         getOption("HTTPUserAgent"),
                         "bot")) %>%
    req_throttle(rate = 1 / 5) %>%  # Limit requests to 1 per 5 seconds
    req_retry(max_tries = 4, backoff = ~30) # Proceed with caution

  # Try query out
  query %>% req_dry_run()

  # Perform the request
  resp <- query %>%
    req_perform()

  if(resp_check_content_type(resp, "application/zip")) { # Maybe not right

    # Save zip file to readme install path
    # out_dir <- paste(.libPaths()[1], "readme", sep = "/")

    resp %>%
      httr2::resp_body_raw() %>%
      brio::write_file_raw(
        path = paste(out_dir, "glove.twitter.27B.zip", sep = "/"))

    # Unzip files
    unzip(paste0(out_dir, "/", "glove.twitter.27B.zip"), exdir = out_dir)

    # List files in out directory, all in order?
    list.files(out_dir)

  } else { warning("Did not return a zip file") }

}

## WORD VECTOR SUMMARIES #######################################################

# Proof of concept with Clinton data
data(clinton, package = "readme")

my_data <- clinton %>% as_tibble() %>% slice(1:10)

glimpse(my_data)

# Models OLD
# models <- c("bert" = "google-bert/bert-base-uncased",
#             "roberta" = "openai-community/roberta-large-openai-detector",
#             "gpt2" = "openai-community/gpt2-xl",
#             "nvidia-llama3" = "nvidia/Llama3-ChatQA-1.5-8B",
#             "pegasus" = "google/pegasus-xsum")

# Models NEW
models <- c("bert" = "bert-base-uncased",
            "roberta" = "roberta-base",
            "llama3 = nvidia/Llama-3.1-Nemotron-Nano-8B-v1")

set.seed(987654321)

# Generate a word vector summary for first N documents
my_dfm <- undergrad(
  documentText = my_data$TEXT,
  word_quantiles = c(0.1, 0.5, 0.9),
  numericization_method = "transformer_based",
  textEmbed_control = list(model = models["bert"],
                           layers = -1L,
                           tokenizer_parallelism = TRUE)
)

saveRDS(my_dfm, "/scratch/project_2010556/dfm-test-bert.rds")

## PROPORTION ESTIMATION #######################################################

# nProj <- 50L
#
# set.seed(987654321)
#
# # Perform estimation
# readme_results <- readme(
#   dfm = my_dfm,                       # Result from undergrad()
#   labeledIndicator = my_data$TRAININGSET,
#   categoryVec = my_data$TRUTH,
#   nBoot = 100,
#   numProjections = 50,
#   sgdIters = 750,
#   nCores = 15,
#   nCores_OnJob = 15,
#   verbose = T)
#
# saveRDS(readme_results, "readme_results.rds")
#
# res <- readme_results$point_readme
#
# ################################################################################
#
# data.frame(categories = names(res), estimates = res) %>%
#   write.table("readme_results.txt", sep = "\t", row.names = FALSE)
#
# # Load twitter word vectors: read to data.table
# tweet_wordVecs <-
#   data.table::fread(file.path(out_dir, "glove.twitter.27B.200d.txt"),
#                     sep = " ", nThread = 8L)
#
# # Coerce to matrix; name rows and columns
# tweet_wordVecs <- as.matrix(tweet_wordVecs, rownames = 1)
# colnames(tweet_wordVecs) <- NULL
# colnames(tweet_wordVecs) <- colnames(tweet_wordVecs, do.NULL = FALSE,
#                                      prefix = "V")

################################################################################
