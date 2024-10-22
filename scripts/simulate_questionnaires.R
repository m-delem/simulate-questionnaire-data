source("scripts/_setup.R")


# Simulating individual questionnaire items' scores --------------------

# Function that generates item scores that sum up to a total score.

simulate_items <- function(
    score = 20,
    n_items = 16,
    min_item = 1,
    max_item = 5,
    verbose = FALSE # whether to print the item scores
){
  if (score < n_items * min_item) {
    stop("The total score is too low to be distributed among the items")
  }
  
  if (score > n_items * max_item) {
    stop("The total score is too high to be distributed among the items")
  }
  
  # Function to mimic the += operator in Python
  increment <- function (x, inc = 1) eval.parent(substitute(x <- x + inc))
  # Function to make sampling programatically easier
  resample <- function(x, n) x[sample.int(length(x), n)]
  
  all_items <- rep(0, n_items)
  
  # We fill the items iteratively until we reach the desired score
  for (point_missing in seq_along(1:score)) {
    
    # If some items are below the minimum (e.g., 0), we fill them first
    if (any(all_items < min_item)) {
      item <- resample(which(all_items < min_item), 1)
      all_items[item] |> increment(1)
    } else {
      item <- resample(which(all_items < max_item), 1)
      all_items[item] |> increment(1)
    }
  }

  if (verbose) {
    cat("Item scores are ", all_items, " with a total score of ", sum(all_items), "\n")
  }
  
  return(all_items)
}


# Simulate a questionnaire with a given distribution -----------------

simulate_questionnaires <- function(
    n_subjects = 1000,
    names = c("scale"),   # vector of names for each scale
    distrib = c("normal"),  # vector of distributions for each scale
    method = "score_means", # "score_means" or "item_means", whether to simulate the sample distribution of total scores or item means
    means = c(10),      # vector of means for each scale
    sds = c(3),          # vector of standard deviations for each scale
    skews = NULL,           # vector of skewness for each scale
    corrs = NULL,           # correlations between scales
    print_corrs = FALSE,    # whether to print the expected and simulated correlations
    n_items = c(15),        # number of items for each scale
    min_item = c(1),        # minimum value of items for each scale
    max_item = c(5),        # maximum value of items for each scale
    add_items = TRUE
) {
  
  # ----------- Sanity checks
  
  # Checking arguments
  if (length(names) != length(distrib) | 
      length(names) != length(means) | 
      length(names) != length(sds) |
      length(names) != length(n_items) |
      length(names) != length(min_item) |
      length(names) != length(max_item)
  ) {
    stop("Arguments 'names', 'distrib', 'means', 'sds', 'n_items', 'min_item' and 'max_item' must have the same length.")
  }
  
  # method must be either "score_means" or "item_means"
  if (!(method %in% c("score_means", "item_means"))) {
    stop("Argument 'method' must be either 'score_means' or 'item_means'.")
  }
  
  # If the method is score_means, the means must be between n_items * min_item and n_items * max_item, else if method is item_means, the means must be between min_item and max_item
  if (method == "score_means") {
    for (i in seq_along(names)) {
      if (means[i] < n_items[i] * min_item[i] | means[i] > n_items[i] * max_item[i]) {
        stop("The mean of scale '", names[i], "' is out of bounds for the number of items and their range.\n\nSpecify 'method = score_means' if you want to declare the sample means of the total scores or 'method = item_means' if you want to declare the sample means of the item scores.")
      }
    }
  } else {
    for (i in seq_along(names)) {
      if (means[i] < min_item[i] | means[i] > max_item[i]) {
        stop("The mean of scale '", names[i], "' is out of bounds for the items' range.\n\nSpecify 'method = score_means' if you want to declare the sample means of the total scores or 'method = item_means' if you want to declare the sample means of the item scores.")
      }
    }
  }
  
  # If a distribution is skew-normal, skews must be specified for everyone and be 0 for normal distributions
  if (any(distrib == "skew_normal") && length(names) != length(skews)) {
    stop("If a skew-normal distribution is specified, you must give skewnesses for all the scales. Simply specify 0 for normal distributions, e.g. here 'skews = c(", if_else(distrib == "normal", 0, -0.5) |> as.character() |> paste(collapse = ", "), ")'")
  }
  
  # There should be correlations for each scale with the others in a vector. So
  # scale_A/scale_B, scale_A/scale_C, scale_A/scale_D, scale_B/scale_C, scale_B/scale_D, etc.
  if (!is.null(corrs) && length(corrs) != length(names) * (length(names) - 1) / 2) {
    stop("Argument 'corrs' must have the right number of elements, in the present case ", length(names) * (length(names) - 1) / 2, ".")
  }
  
  # ----------- End of sanity checks
  
  # Create the correlation matrix if the argument is not empty, else create null matrix with 1 diagonal
  cor_mat <- if (!is.null(corrs)) {
    cor_mat <- matrix(1, nrow = length(names), ncol = length(names))
    cor_mat[lower.tri(cor_mat)] <- corrs
    cor_mat[upper.tri(cor_mat)] <- corrs
    diag(cor_mat) <- 1
    cor_mat
  } else {
    diag(1, length(names))
  }
  
  # Build the future data as a matrix to correlate the scales
  data_mat <- matrix(NA, nrow = n_subjects, ncol = length(names))
  
  # Fill the matrix with the simulated scores
  for (i in seq_along(names)) {
    if (distrib[i] == "normal") {
      data_mat[, i] <- rnorm(n_subjects, mean = means[i], sd = sds[i])
    } else {
      params <- cp2dp(c(means[i], sds[i], skews[i]), "SN")
      data_mat[, i] <- rsn(n_subjects, dp = params, "SN")
    }
  }
  
  # Correlate the scales
  data_mat <- data_mat %*% chol(cor_mat)
  
  # Add column names depending on the method chosen 
  prefix <- if (method == "score_means") "score_" else "mean_"
  colnames(data_mat) <- paste0(prefix, names)
  
  # Create the df
  df <- 
    as_tibble(data_mat) |> 
    mutate(subject = 1:n_subjects) |> 
    select(subject, everything())
  
  # Print the expected and simulated correlations
  if (print_corrs) {
    cat("Expected correlations:\n")
    print(cor_mat)
    cat("\nSimulated correlations:\n")
    print(cor(df |> select(-subject) |> as.matrix()) |> round(2))
  }
  
  # If methods is item_means, we:
  #   - Bound the means of the items between min and max item score and round
  #   - Simulate integer total scores from the number of items and bound them
  #   - Simulate individual items from the total scores
  # Else:
  #   - Just bound and round the total scores (which are already simulated)
  #   - Simulate individual items from the total scores
  #   - Compute means from the simulated individual items
  if (method == "item_means") {
    for (i in seq_along(names)) {
      df <- 
        df |>
        mutate(
          # bound the simulated means between min and max item score
          !!paste0("mean_", names[i]) := case_when(
            !!sym(paste0("mean_", names[i])) < min_item[i] ~ min_item[i], 
            !!sym(paste0("mean_", names[i])) > max_item[i] ~ max_item[i], 
            TRUE ~ !!sym(paste0("mean_", names[i]))
          ) |> round(2),
          # simulate integer total scores from the number of items
          !!paste0("score_", names[i]) := floor(!!sym(paste0("mean_", names[i])) * n_items[i])
        ) |>
        rowwise() |>
        mutate(
          item = list(
            simulate_items(
              !!sym(paste0("score_", names[i])),
              n_items[i],
              min_item[i],
              max_item[i])
          )
        ) |>
        unnest_wider(item, names_sep = "_") |>
        rename_with(~paste0(names[i], "_", .), starts_with("item_")) |> 
        select(
          subject, 
          starts_with("score_"), 
          starts_with("mean_"),
          contains("item_"), 
          everything()
        )
    }
  } else {
    for (i in seq_along(names)) {
      df <- 
        df |>
        mutate(
          !!paste0("score_", names[i]) := case_when(
            !!sym(paste0("score_", names[i])) < n_items[i] * min_item[i] ~ n_items[i] * min_item[i],
            !!sym(paste0("score_", names[i])) > n_items[i] * max_item[i] ~ n_items[i] * max_item[i],
            TRUE ~ !!sym(paste0("score_", names[i]))
          ) |> round()
        ) |>
        rowwise() |>
        mutate(
          item = list(
            simulate_items(
              !!sym(paste0("score_", names[i])),
              n_items[i],
              min_item[i],
              max_item[i])
          )
        ) |>
        unnest_wider(item, names_sep = "_") |>
        mutate(
          !!paste0("mean_", names[i]) := round(rowMeans(across(starts_with("item_"))), 2)
        ) |>
        rename_with(~paste0(names[i], "_", .), starts_with("item_")) |> 
        select(
          subject, 
          starts_with("score_"), 
          starts_with("mean_"),
          contains("item_"), 
          everything()
        )
    }
  }
  
  # Remove the individual items if unnecessary
  if (!add_items) df <- df |> select(!contains("item"))
  
  cat("Data frame:\n")
  return(df)
}


# Simulating VVIQ data ------------------------------------------------

# The VVIQ was a pain in the ass because the distribution is not normal and 
# I did not have precise statistical parameters from the literature, only 
# general visual imagery prevalence data:
# 
# According to Wright et al. (2024), the VVIQ distribution has the following
# characteristics:
# 0.9% of the population score 16 (aphantasia)
# 3.3% score between 17 and 32 (hypophantasia)
# 89.7% score between 33 and 74 (typical imagery)
# 6.1% score between 75 and 80 (hyperphantasia)

# We will solve this by creating four distributions, one for each group:
# Aphantasics will be centered on mean 16
# Hypophantasics will be centered on mean 16 + (32 - 16) / 2 = 24
# Typical imagers will be centered on mean 33 + (74 - 33) / 2 = 53.5
# Hyperphantasics will be centered on mean 75 + (80 - 75) / 2 = 77.5

simulate_vviq <- function(
    n_subjects = 500,
    add_items = FALSE
) {
  # Means for aphantasia, hypophantasia, typical and hyperphantasia groups
  group_means <- c(16, 24, 53, 77)
  # Probabilities for each group from Wright et al. (2024)
  probabilities <- c(0.009, 0.033, 0.897, 0.061)
  
  # Function to make sampling programatically easier
  resample <- function(x, n, prob) x[sample.int(length(x), n, prob = prob)]
  
  df <- 
    tibble(
      subject = 1:n_subjects,
      # We sample from each group given the probabilities
      mean_score = sample(
        group_means, 
        n_subjects, 
        replace = TRUE, 
        prob = probabilities),
      # Naming the groups
      group = case_match(
        mean_score,
        16 ~ "aph",
        24 ~ "hypo",
        53 ~ "typical",
        77 ~ "hyper"
      ) |> factor(levels = c("aph", "hypo", "typical", "hyper")),
      # We define the score ranges for each group
      score_range = case_match(
        group,
        "aph" ~ list(16),
        "hypo" ~ list(17:32),
        "typical" ~ list(33:74),
        "hyper" ~ list(75:80)
      )
    ) |> 
    rowwise() |> 
    # We define different probability distributions within each group
    mutate(
      prob = case_when(
        # aphantasics are always at floor VVIQ
        group == "aph" ~ list(c(1)), 
        # hypophantasics are skewed towards the upper end
        group == "hypo" ~ list(score_range), 
        # typical are normally distributed around the mean
        group == "typical" ~ list(
          dnorm(
            score_range, 
            mean = mean_score, 
            sd = 8)),
        # hyperphantasics are skewed towards the upper end too
        group == "hyper" ~ list(score_range),
      ),
      score_vviq = resample(score_range, 1, prob = prob),
      item = list(simulate_items(score_vviq, 16, 1, 5))
    ) |> 
    unnest_wider(item, names_sep = "_") |> 
    rename_with(~paste0("vviq_", .), starts_with("item")) |> 
    rowwise() |> 
    mutate(mean_vviq = round(rowMeans(across(starts_with("vviq_"))), 2)) |> 
    select(subject, group, score_vviq, mean_vviq, contains("item"))
  
  # We can remove the individual items if unnecessary
  if (!add_items) df <- df |> select(!contains("item"))
  
  return(df)
}