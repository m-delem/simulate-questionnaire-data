source("scripts/simulate_questionnaires.R")


# Plotting VVIQ distributions ---------------------------------------------

plot_vviq <- function(df, var = "score") {
  
  if (!(var %in% c("score", "mean"))){
    stop('var must be either "score" or "mean".')
  }
  
  if (var == "score") {
    bw <- 1
    div <- 1
  } else {
    bw <- 0.2
    div <- 5
  }
  
  p <- 
    df |> 
    ggplot(aes(
      x = .data[[paste0(var, "_vviq")]], 
      colour = group,
      fill = group
    )
    ) +
    geom_histogram(binwidth = bw, alpha = 0.4) +
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
      title = "Simulated VVIQ distribution",
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
  
  print(p)
  
  return(p)
}

