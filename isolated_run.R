#!/usr/bin/env Rscript

# Local testing script - runs train then predict

source("train.R")
source("predict.R")

cat("Running train...\n")
train_chap("example_data/training_data.csv", "output/model.rds")

cat("Running predict...\n")
predict_chap("output/model.rds", "example_data/training_data.csv",
             "example_data/future_data.csv", "output/predictions.csv")

cat("\nDone! Check output/predictions.csv for results.\n")
