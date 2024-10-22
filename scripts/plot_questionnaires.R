source("scripts/simulate_questionnaires.R")


# Plotting questionnaire distributions ------------------------------------

plot_questionnaires <- function(
    df, 
    var = "score", 
    questionnaire = "questionnaire"
) {
  
  if (!(var %in% c("score", "mean"))){
    stop('var must be either "score" or "mean".')
  }
  
  n_subjects <- nrow(df)
  
  if (var == "score") {
    div_breaks <- 1
  } else {
    div_breaks <- 2
  }
  
  p <- 
    df |> 
    select(contains(var)) |>
    pivot_longer(cols = everything()) |> 
    mutate(
      scale = 
        str_remove(name, paste0(var, "_")) |> 
        str_replace_all("_", " ") |> 
        str_to_title()
    ) |> 
    ggplot(aes(
      x = value, 
      colour = scale,
      fill = scale
    )
    ) +
    geom_density(aes(y = after_stat(count)), alpha = 0.1, linewidth = 0.5) +
    scale_y_continuous(expand = expansion(c(0,0.1))) +
    scale_x_continuous(
      expand = expansion(c(0,0)),
      breaks = breaks_pretty(20 / div_breaks)
    ) +
    labs(
      title = paste0("Simulated ", questionnaire, " ", var, " distribution (N = ", n_subjects, ")"),
      colour = "Scale",
      fill = "Scale",
      x = paste0(questionnaire, " ", var, "s"),
      y = "Count"
    ) +
    theme_modern() +
    theme(
      panel.grid.major.y = element_line(colour = "grey90"),
      panel.grid.minor.y = element_line(colour = "grey90"),
      axis.text.x = element_text(size = 10),
      axis.ticks.x = element_line(),
    )
  
  return(p)
}


# Plotting VVIQ distributions ---------------------------------------------

plot_vviq <- function(df, var = "score", print = FALSE) {
  
  if (!(var %in% c("score", "mean"))){
    stop('var must be either "score" or "mean".')
  }
  
  n_subjects <- nrow(df)
  
  if (var == "score") {
    bw <- 1
    div <- 1
  } else {
    bw <- 0.2
    div <- 5
  }
  
  p <- 
    df |> 
    mutate(group = case_match(
      group,
      "aph" ~ "Aphantasia",
      "hypo" ~ "Hypophantasia",  
      "typical" ~ "Typical",
      "hyper" ~ "Hyperphantasia"
      ) |> 
      factor(levels = c(
        "Aphantasia", "Hypophantasia", 
        "Typical", "Hyperphantasia"))
    ) |> 
    ggplot(aes(
      x = .data[[paste0(var, "_vviq")]], 
      colour = group,
      fill = group
      )
    ) +
    geom_histogram(binwidth = bw, alpha = 0.3) +
    geom_density(
      aes(y = after_stat(count)/div, fill = NULL),
      colour = "red",
      linewidth = 0.5,
      show.legend = FALSE
    ) + 
    scale_y_continuous(expand = expansion(c(0,0.1))) +
    scale_x_continuous(
      expand = expansion(c(0,0)),
      breaks = breaks_pretty(20)
    ) +
    labs(
      title = paste0("Simulated VVIQ distribution (N = ", n_subjects, ")"),
      colour = "Group",
      fill = "Group",
      x = paste0("VVIQ ", var),
      y = "Count"
    ) +
    theme_modern() +
    theme(
      panel.grid.major.y = element_line(colour = "grey90"),
      panel.grid.minor.y = element_line(colour = "grey90"),
      axis.text.x = element_text(size = 10),
      axis.ticks.x = element_line(),
    )
  
  if (print) print(p)
  
  return(p)
}