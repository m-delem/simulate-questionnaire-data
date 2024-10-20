source("scripts/_setup.R")

# Simulating individual questionnaire items' scores --------------------

# I created a function that generates item scores that sum up to a total score.
# The function is set with VVIQ values by default, but it could be used for 
# any other scale by tweaking the number of items, min and max scores.

simulate_items <- function(
    score = 20,
    n_items = 16,
    min_item_score = 1,
    max_item_score = 5,
    verbose = FALSE # whether to print the item scores
){
  if (score < n_items * min_item_score) {
    stop("The total score is too low to be distributed among the items")
  }
  
  # Function to mimic the += operator in Python
  increment <- function (x, inc = 1) eval.parent(substitute(x <- x + inc))
  # Function to make sampling programatically easier
  resample <- function(x, n) x[sample.int(length(x), n)]
  
  all_items <- rep(0, n_items)
  
  # We fill the items iteratively until we reach the desired score
  for (point_missing in seq_along(1:score)) {
    
    # If some items are below the minimum (e.g., 0), we fill them first
    if (any(all_items < min_item_score)) {
      item <- resample(which(all_items < min_item_score), 1)
      all_items[item] |> increment(1)
    } else {
      item <- resample(which(all_items < max_item_score), 1)
      all_items[item] |> increment(1)
    }
  }

  if (verbose) {
    cat("Item scores are ", all_items, " with a total score of ", sum(all_items), "\n")
  }
  
  return(all_items)
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


# Simulating OSIVQ data ---------------------------------------------------

# I could find way more material for the OSIVQ than the VVIQ, using the original 
# article from Blazhenkova and Kozhevnikov (2009).
# 
# The OSIVQ has 45 items divided into three scales with 15 items each:
# - Object scale (OSIVQ-O), M = 3.63, SD = 0.62
# - Spatial scale (OSIVQ-S), M = 2.83, SD = 0.66
# - Verbal scale (OSIVQ-V), M = 3.00, SD = 0.68
# 
# B & K also precise that the OSIVQ-O is negatively skewed with Skewness = -.392
# and SE = .098, which is a benediction. I found the way to simulate this with a
# skew-normal distribution using the "sn" package.
# 
# B & K report that O and S have a correlation of -0.03, O and V 0.12, and S and
# V -0.18. I found in my good old Msc simulation code the way to correlate the 
# three scales using an effect matrix. I knew it would come in handy one day!
# 
# We can now proceed the same way we did with the VVIQ:
# - Simulate distributions for the O/S/V scales with the given correlations
# - Rescale the scores between 15 and 75 for each scale to have total scores
# - Simulate item scores based on these total scores
# 
# (Idea for later: to merge with the VVIQ data, we could sort the OSIVQ data by 
# the Object score and bind it. This way high VVIQ = high OSIVQ-O.)

simulate_osivq <- function(
   n_subjects = 500,
   o_skew = -0.392, # Skewness for the object scale
   add_items = FALSE,
   verbose = FALSE # whether to print the correlations
){
  # Means and SDs for the three scales
  scale_means <- c(3.63, 2.83, 3.00)
  scale_sds <- c(0.62, 0.66, 0.68)
  
  # Correlation structure described by B & K (2009)
  cor_mat <- matrix(c(
        1, -0.03,  0.12,
    -0.03,     1, -0.18,
     0.12, -0.18,     1
    ), nrow = 3, byrow = TRUE)
  
  # We convert mean, sd and skewness reported for the object scale to xi, omega 
  # and alpha parameters of the skew-normal using the sn package
  params <- cp2dp(c(
    scale_means[1],
    scale_sds[1],
    o_skew),
    "SN") # we can now call "rsn()" with these parameters
  
  df <- matrix(c(
    rsn(n_subjects, dp = params, "SN"),
    rnorm(n_subjects, scale_means[2], scale_sds[2]),
    rnorm(n_subjects, scale_means[3], scale_sds[3])
  ), ncol = 3)
  
  # We correlate the three scales
  df <- df %*% chol(cor_mat)
  colnames(df) <- paste0("mean_", c("object", "spatial", "verbal"))
  df <- as_tibble(df)
  
  if (verbose) {
    cat(
      "Correlations  expected: O/S = -0.03 ; O/V = 0.12 ; S/V = -0.18\nCorrelations simulated: O/S =", round(cor(df$mean_object, df$mean_spatial), 2),
      "; O/V =", round(cor(df$mean_object, df$mean_verbal), 2),
      "; S/V =", round(cor(df$mean_spatial, df$mean_verbal), 2), "\n")
  }
  
  # We create total scores by rescaling the means between 15 and 75
  df <- 
    df |>
    mutate(
      score_object = floor(mean_object * 15),
      score_spatial = floor(mean_spatial * 15),
      score_verbal = floor(mean_verbal * 15),
      across(contains("score"), ~case_when(. < 15 ~ 15, . > 75 ~ 75, TRUE ~ .))
    )
  
  # We can use the total scores to add the individual items if necessary
  if (add_items) {
    df <- 
      df |> 
      rowwise() |> 
      mutate(
        item_o = list(simulate_items(score_object, 15, 1, 5)),
        item_s = list(simulate_items(score_spatial, 15, 1, 5)),
        item_v = list(simulate_items(score_verbal, 15, 1, 5))
      ) |> 
      unnest_wider(c(item_o, item_s, item_v), names_sep = "_") |> 
      rename_with(~paste0("osivq_", .), starts_with("item"))
  }
  
  return(df)
}

