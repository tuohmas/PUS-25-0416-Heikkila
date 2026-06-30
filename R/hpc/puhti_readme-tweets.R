# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script for running Automated Nonparametric Content Analysis with Readme2
# To be run as a CSC Puhti Array job

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

r_pkgs <- c("dplyr", "tidyr", "reticulate", "pacman", "data.table",
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

print("\nDone with preparations...")

### README PROCESS #############################################################

# path <- "/scratch/project_2010556/readme" # HPC ENV

# Models
models <- c("bert"       = "bert-base-cased", # google-bert/bert-base-cased
            "roberta"    = "roberta-base", # FacebookAI/roberta-base
            "distilgpt2" = "distilgpt2", # "distilbert/distilgpt2
            "glove"      = "glove")

# Array arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])

# Fetch "Undergrad" vector embedding DFMs
my_dfm <-
  readRDS(paste0(path, "/undergrad", "/tweets-", names(models[i]), ".rds"))
dim(my_dfm)

labels <-
  readRDS(paste0(path, "/inputs/tweets2transformers.rds"))

dim(my_dfm)[1] == dim(labels)[1]

my_labeled_indicator <- labels$label_indicator
my_category_vec <- labels$code

rm(labels)

# Set up parallel processing
n_cores <- detectCores() - 1
n_cores

cl <- makeCluster(n_cores, type = "FORK") # HPC ENV
# cl <- makeCluster(n_cores) # For local environment

registerDoParallel(cl)
getDoParWorkers()

# Free up space if applicable
if(exists("readme_results")){ rm(readme_results) }

# For some reason "nProj" ("numProjections") has to be set as a global variable
nProj <- 20L

set.seed(987654321)

start_time <- Sys.time()

# Perform estimation
readme_results <- readme::readme(
  dfm = my_dfm,
  labeledIndicator = my_labeled_indicator,
  categoryVec = my_category_vec,
  nBoot = 20L,
  sgdIters = 500,
  numProjections = 20L,
  nCores = detectCores(),
  nCores_OnJob = n_cores,
  verbose = TRUE
  )

elapsed_time <- Sys.time() - start_time
elapsed_time

stopCluster(cl)

# Glimpse results
readme_results$point_readme

saveRDS(readme_results,
        paste0(path, "/results/readme-tweets-", names(models[i]), ".rds"))

# Script finished
message("Script finished succesfully")
