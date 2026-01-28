# Shared utility functions for INLA baseline model

#' Create location ID mapping (names to numeric IDs)
#' @param locations Character vector of location names
#' @return Named list with 'to_id' (name -> id) and 'to_name' (id -> name)
create_location_mapping <- function(locations) {
  unique_locs <- unique(locations)
  to_id <- setNames(seq_along(unique_locs), unique_locs)
  to_name <- setNames(unique_locs, seq_along(unique_locs))
  list(to_id = to_id, to_name = to_name)
}

#' Add numeric location IDs to dataframe
#' @param df Dataframe with 'location' column
#' @param mapping Location mapping from create_location_mapping()
#' @return Dataframe with added 'location_id' column
add_location_ids <- function(df, mapping) {
  df$location_id <- mapping$to_id[df$location]
  df
}

#' Generate Poisson samples from INLA posterior
#' @param inla_result INLA result object
#' @param newdata Dataframe for prediction (with location_id and covariates)
#' @param n_samples Number of posterior samples
#' @param location_mapping Location mapping for IID effects
#' @return Matrix of samples (rows = observations, cols = samples)
generate_poisson_samples <- function(inla_result, newdata, n_samples, location_mapping) {
  # Get posterior samples
  post_samples <- INLA::inla.posterior.sample(n_samples, inla_result)

  n_obs <- nrow(newdata)
  samples_matrix <- matrix(NA, nrow = n_obs, ncol = n_samples)

  for (s in seq_len(n_samples)) {
    sample <- post_samples[[s]]

    # Extract fixed effects
    latent <- sample$latent
    latent_names <- rownames(latent)

    # Get intercept
    intercept_idx <- grep("^\\(Intercept\\)", latent_names)
    intercept <- if (length(intercept_idx) > 0) latent[intercept_idx, 1] else 0

    # Get covariate effects
    rainfall_idx <- grep("^rainfall:", latent_names)
    beta_rainfall <- if (length(rainfall_idx) > 0) latent[rainfall_idx, 1] else 0

    temp_idx <- grep("^mean_temperature:", latent_names)
    beta_temp <- if (length(temp_idx) > 0) latent[temp_idx, 1] else 0

    # Get location random effects (IID)
    location_effects <- numeric(length(location_mapping$to_name))
    for (loc_id in seq_along(location_mapping$to_name)) {
      loc_pattern <- paste0("^location_id:", loc_id, "$")
      loc_idx <- grep(loc_pattern, latent_names)
      if (length(loc_idx) > 0) {
        location_effects[loc_id] <- latent[loc_idx, 1]
      }
    }

    # Compute linear predictor for each observation
    for (i in seq_len(n_obs)) {
      eta <- intercept +
        beta_rainfall * newdata$rainfall[i] +
        beta_temp * newdata$mean_temperature[i] +
        location_effects[newdata$location_id[i]]

      # Lambda = E * exp(eta) where E is population (offset)
      lambda <- newdata$population[i] * exp(eta)

      # Sample from Poisson
      samples_matrix[i, s] <- rpois(1, lambda)
    }
  }

  samples_matrix
}
