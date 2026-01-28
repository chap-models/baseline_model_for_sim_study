#!/usr/bin/env Rscript

library(INLA)
source("lib.R")

predict_chap <- function(model_path, historic_data_path, future_data_path, predictions_path) {
  # Load data
  historic_df <- read.csv(historic_data_path)
  future_df <- read.csv(future_data_path)

  # Create location mapping from historic data
  location_mapping <- create_location_mapping(historic_df$location)

  # Add numeric location IDs
  historic_df <- add_location_ids(historic_df, location_mapping)
  future_df <- add_location_ids(future_df, location_mapping)

  # Handle missing disease_cases
  historic_df$disease_cases[is.na(historic_df$disease_cases)] <- 0

  # Define INLA formula
  # Poisson likelihood with log link
  # Linear effects for rainfall and mean_temperature (shared across locations)
  # IID random effect per location
  formula <- disease_cases ~ rainfall + mean_temperature + f(location_id, model = "iid")

  # Fit INLA model on historic data
  inla_result <- inla(
    formula,
    family = "poisson",
    data = historic_df,
    E = historic_df$population,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )

  # Generate posterior samples for future data
  n_samples <- 1000
  samples_matrix <- generate_poisson_samples(
    inla_result,
    future_df,
    n_samples,
    location_mapping
  )

  # Build output dataframe
  output_df <- data.frame(
    time_period = future_df$time_period,
    location = future_df$location
  )

  # Add sample columns
  for (s in seq_len(n_samples)) {
    col_name <- paste0("sample_", s - 1)
    output_df[[col_name]] <- pmax(0, samples_matrix[, s])
  }

  # Write predictions
  write.csv(output_df, predictions_path, row.names = FALSE)
}

if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 4) {
    predict_chap(args[1], args[2], args[3], args[4])
  } else {
    cat("Usage: Rscript predict.R <model.rds> <historic.csv> <future.csv> <predictions.csv>\n")
    quit(status = 1)
  }
}
