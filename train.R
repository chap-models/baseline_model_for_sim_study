#!/usr/bin/env Rscript

# Minimal train.R - model fitting is done in predict.R
# This follows the ewars_template pattern where train just passes through

train_chap <- function(train_data_path, model_path) {
  # Save minimal info - actual model fitting happens in predict.R
  saveRDS(list(trained = TRUE), file = model_path)
}

if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    train_chap(args[1], args[2])
  } else {
    cat("Usage: Rscript train.R <train_data.csv> <model.rds>\n")
    quit(status = 1)
  }
}
