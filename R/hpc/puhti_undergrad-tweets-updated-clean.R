# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for running Automated Nonparametric Content Analysis with Readme2
# To be run as a CSC Puhti Array job

## Login overriding the default MAC algorithm
# ssh -m hmac-sha2-512 tuomheik@puhti.csc.fi

### TESTTING IN INTERACTIVE
## Launch interactive mode with default memory and local scratch space. for example:
# cd /projappl/project_2010556
# sinteractive --account project_2010556 --cores 1 --time 01:00:00 --mem 8G --tmp 10

## Create a folder for your R packages in /projappl
# cd /projappl/project_2010556
# mkdir project_rpackages

## Go to projappl and activate r-readme
# cd /projappl/project_2010556
# module load python-data
# source /projappl/project_2010556/r-reticulate/bin/activate

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

r_pkgs <- c("dplyr", "tidyr", "reticulate", "text", "pacman", "data.table",
            "doParallel", "foreach")

if(!all(r_pkgs %in% list.files(libpath))) {
  install.packages(setdiff(r_pkgs, list.files(libpath)),
                   dependencies = TRUE, lib = libpath) }

# Load other packages
pacman::p_load(r_pkgs, character.only = TRUE)

sessionInfo()

# Activate virtual env
envname <- "./r-reticulate" # HPC ENV
use_virtualenv(envname)

# Testing whether TensorFlow is working
tensorflow::tf$constant("Hello world")

# Initialize and test text package
textrpp_initialize(virtualenv = envname, refresh_settings = FALSE,
                save_profile = FALSE, textEmbed_test = TRUE, check_env = FALSE)

print("\nDone with preparations...")

## WORD VECTOR SUMMARIES #######################################################

path <- "/scratch/project_2010556/readme" # HPC ENV

# Models
models <- c("bert" = "bert-base-cased", # google-bert/bert-base-cased
            "roberta" = "roberta-base", # FacebookAI/roberta-base
            "distilgpt2" = "distilgpt2") # "distilbert/distilgpt2

# FIXME GET BACK TO THIS
# input_types <- list.files("./data/readme")
# input_types <- c("tweets", "replies")

# Array arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])

# Which models have to be lower cased, and which can handle emojis?
# You can test with tranformers python package with given model by
# transformer.decode(transformer.encode('ToGGlEcase 😂 with 😊 emojis 🙌'))

if (names(models[i]) %in% c("bert")) {

  model_input <-
    readRDS(paste(path, "inputs", "tweets2transformers-verbose.rds", sep = "/")) %>%
    pull(text)

} else {

  model_input <-
    readRDS(paste(path, "inputs", "tweets2transformers.rds", sep = "/")) %>%
    pull(text)

}

head(model_input, n = 10)

model_path <- paste(path, "models", models[i], sep = "/") # HPC ENV
list.files(model_path)

# Quickly test model performance before proceeding; something like this
test_model <- text::textEmbed(
  model_input[30:39], model = model_path, layers = -1, max_token_to_sentence = 1)

print(test_model)
rm(test_model)

# Set up parallel processing
n_cores <- detectCores() - 1
n_cores
cl <- makeCluster(n_cores, type = "FORK") # HPC ENV

registerDoParallel(cl)
getDoParWorkers()

if(exists("my_dfm")) { rm(my_dfm) }

start_time <- Sys.time()

set.seed(987654321)

# Generate a word vector summary for first N documents
my_dfm <- readme::undergrad(
  documentText = model_input,
  word_quantiles = c(0.1, 0.5, 0.9),
  numericization_method = "transformer_based",
  textEmbed_control = list(
    model = model_path,
    max_token_to_sentence = 1,
    layers = -1L,
    tokenizer_parallelism = TRUE,
    device = "cpu"))

elapsed_time <- Sys.time() - start_time
elapsed_time

dim(my_dfm)

stopCluster(cl)

# FIXME if something else than tweets...
saveRDS(my_dfm, paste0(path, "/undergrad/tweets-", names(models[i]), ".rds"))
