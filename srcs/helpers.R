
options(digits = 4, scipen = 999)
knitr::opts_chunk$set(fig.align = 'center',
                      fig.width = 9.5, 
                      fig.height = 6.5,
                      cache = T,
                      warning = F,
                      message = F,
                      #echo = F,
                      eval = T)

# Set working dictionary where this rmd file is.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Basics
if(!require(dplyr)) install.packages("dplyr")
if(!require(tibble)) install.packages("tibble")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(forcats)) install.packages("forcats")
if(!require(patchwork)) install.packages("patchwork")
if(!require(scales)) install.packages("scales")
if(!require(readr)) install.packages("readr")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(GGally)) install.packages("GGally")
if(!require(plotly)) install.packages("plotly")
if(!require(forcats)) install.packages("forcats")
if(!require(stringr)) install.packages("stringr")

if(!require(ggpubr)) install.packages("ggpubr")
if(!require(PupillometryR)) install.packages("PupillometryR")
if(!require(ggridges)) install.packages("ggridges")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(magrittr)) install.packages("magrittr") # allow %<>%

if(!require(vcd)) install.packages("vcd")
if(!require(d3r)) install.packages("d3r")
if(!require(ggalluvial)) install.packages("ggalluvial")
if(!require(parcoords)) install.packages("parcoords")

# Data Set
if(!require(Lock5withR)) install.packages("Lock5withR")
if(!require(openintro)) install.packages("openintro")

# pair plot
if(!require(gridGraphics)) install.packages("gridGraphics")
if(!require(gridExtra)) install.packages("gridExtra")

# set up ggplot2 helper variable
JACE_COLOR <- c("#FF5A5F", "#FFB400", "#007A87", 
                "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", 
                "#9CA299", "#565A5C", "#00A04B", "#E54C20")
JACE_COLOR_SCALE <- scale_color_manual(values = JACE_COLOR)
JACE_FILL_SCALE <- scale_fill_manual(values = JACE_COLOR)

only_x = theme(
  panel.grid.major.x = element_line(linetype = "dashed", color = "lightgray"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)

only_y = theme(
  panel.grid.major.y = element_line(linetype = "dashed"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

only_y_all = theme(
  panel.grid.major.y = element_line(linetype = "dashed", color = alpha("lightgray", 0.75)),
  panel.grid.minor.y = element_line(linetype = "dashed", color = alpha("lightgray", 0.75)),
)

both_xy = theme(
  panel.grid.major.y = element_line(linetype = "dashed", color = alpha("lightgray", 0.75)),
  panel.grid.major.x = element_line(linetype = "dashed", color = alpha("lightgray", 0.75))
)


no_xy = theme(
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank()
)

get_lightxy <- function(alpha = 0.65, linetype = "dotted"){
  return(theme(
    panel.grid.major.y = element_line(linetype = linetype, color = alpha("gray",alpha)),
    panel.grid.major.x = element_line(linetype = linetype, color = alpha("gray",alpha))
  ))
}

# Create my own theme: theme_jace
FONT = "Times New Roman"
theme_bw() -> themebw_help
theme(
  text = element_text(family = FONT,
                      color = "black"),
  plot.title = element_text(face="bold",
                            hjust = 0.5,
                            family = FONT,
                            colour = "black",
                            margin = margin(t = 10, r = 0, b = 10, l = 0),
                            size = 15),
  axis.text = element_text(family = FONT,
                           color = "black"),
  plot.subtitle = element_text(family = FONT,
                               hjust = 0.5,
                               size = 12),
  axis.title = element_text(size = 12),
  legend.title = element_text(size = 11,
                              face = "bold",
                              color = "black",
                              family = FONT),
  legend.text = element_text(size = 10,
                             color = "black",
                             family = FONT)) -> text_theme

theme(
  panel.background = themebw_help$panel.background,
  
  strip.background = element_rect(fill = alpha("lightgray", 0.5), inherit.blank = T, colour = NA),
  panel.border = themebw_help$panel.border,
  legend.background = themebw_help$legend.background,
  plot.background = element_rect(color = "white"),
  panel.grid.major.y = element_line(linetype = "dashed", color = "gray")) -> background_theme

theme(
  panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  panel.border = element_blank(),
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
) -> empty_theme

theme_legend = theme(
  legend.box.margin = margin(6, 6, 6, 6),
  legend.background = element_rect(color = NA),
  legend.box.background = element_blank()
)

theme_clean() + text_theme + background_theme + empty_theme + theme_legend  -> theme_jace

# create a theme for dot plots, which can be reused
theme_dotplot <- theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.7),
        panel.grid.minor.x = element_blank())


axis_unit_scaler <- function(n, digits = 0){
  addUnits_leq_0 <- function(n){
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3, digits = digits), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6, digits = digits), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9, digits = digits), 'B'), # in billions
                                          ifelse(n < 1e15, paste0(round(n/1e12, digits = digits), 'T'), # in trillions
                                                 'too big!'
                                          )))))}
  
  labels <- ifelse(n < 0, 
                   paste0("-", addUnits_leq_0(-n)),  # less than thousands
                   ifelse(n >= 0, addUnits_leq_0(n),  
                          "NA"))
  return(labels)
}

axis_unit_scaler_1 <- function(n, digits = 1){
  addUnits_leq_0 <- function(n){
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3, digits = digits), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6, digits = digits), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9, digits = digits), 'B'), # in billions
                                          ifelse(n < 1e15, paste0(round(n/1e12, digits = digits), 'T'), # in trillions
                                                 'too big!'
                                          )))))}
  
  labels <- ifelse(n < 0, 
                   paste0("-", addUnits_leq_0(-n)),  # less than thousands
                   ifelse(n >= 0, addUnits_leq_0(n),  
                          "NA"))
  return(labels)
}

plot_missing <- function(df, percent=F){
  n_row = nrow(df)
  variables = colnames(df)
  
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup() %>%
    
    arrange(-count) %>%
    mutate(missing_pattern = factor(1:n()),
           is_complete = ifelse(rowSums(across(variables))==0, TRUE, FALSE))
  
  missing_patterns %>%
    pivot_longer(-c(count, missing_pattern, is_complete),
                 names_to = "variable",
                 values_to = "is_na") -> missing_patterns_long  
  
  missing_patterns_long %>%
    group_by(variable) %>%
    filter(is_na == TRUE) %>%
    summarise(miss_rows_by_col = sum(count)) -> missing_patterns_agg
  
  missing_patterns_long %>%
    distinct(variable) %>%
    left_join(missing_patterns_agg) %>%
    mutate_at("miss_rows_by_col", ~replace_na(., 0)) %>%
    arrange(-miss_rows_by_col) %>%
    mutate(variable = fct_inorder(variable)) -> missing_patterns_agg
  
  variable_levels = levels(missing_patterns_agg$variable)
  mid_level = variable_levels[ceiling(length(variable_levels)/2)]
  
  missing_patterns_long %>%
    mutate(variable = factor(variable, levels = variable_levels)) %>%
    mutate(is_na = case_when(
      is_na ~ "na",
      !is_na & is_complete ~ "not_na_and_complete",
      !is_na & !is_complete ~ "not_na"
    )) %>%
    mutate(na_text = case_when(
      is_na == "not_na_and_complete" & variable == mid_level~"complete cases"
    )) %>% 
    
    
    # p_missing_pattern
    arrange(desc(missing_pattern)) %>% 
    ggplot(aes(x = variable, 
               y = fct_rev(missing_pattern), 
               fill = is_na)) +
    geom_tile(color = "white", show.legend = FALSE) + 
    geom_text(aes(label=na_text),
              size = 6.5,
              color = "gray25",
              na.rm = TRUE) +
    scale_fill_manual(values = c("na" = "gray",
                                 "not_na" = "#85C1E9", 
                                 "not_na_and_complete" = "#D6EAF8")) +
    theme_bw() +
    labs(x = "variable",
         y = "missing pattern",
         fill='none') -> p_na_pattern
  
  
  if(percent){
    missing_patterns %>%
      mutate(count = 100 * count /n_row) %>%
      ggplot() +
      aes(y = fct_rev(missing_pattern),
          x = count) +
      geom_col(fill = "#8caef1AA") +
      scale_x_continuous(breaks = breaks_width(25), limits = c(0, 100)) +
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(y = "",
           x = "% row") -> p_na_rowcount
    
    missing_patterns_agg %>%
      mutate(count = 100 * miss_rows_by_col / n_row) %>%
      ggplot() +
      aes(y = miss_rows_by_col,
          x = variable) +
      geom_col(fill = "#8caef1AA") +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_y_continuous(breaks = breaks_width(25), limits = c(0, 100)) +
      labs(x = "",
           title = "Missing value patterns",
           y = "% rows \n missing:") -> p_na_colomncount
    
  }else{
    missing_patterns %>%
      ggplot() +
      aes(y = fct_rev(missing_pattern),
          x = count) +
      geom_col(fill = "#8caef1AA") +
      scale_x_continuous(breaks = breaks_pretty(3)) +
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(y = "",
           x = "row count") -> p_na_rowcount
    
    missing_patterns_agg %>%
      ggplot() +
      aes(y = miss_rows_by_col,
          x = variable) +
      geom_col(fill = "#8caef1AA") +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_y_continuous(breaks = breaks_pretty(3)) +
      labs(x = "",
           title = "Missing value patterns",
           y = "num rows \n missing:") -> p_na_colomncount
  }
  
  return(p_na_colomncount + plot_spacer() + p_na_pattern + p_na_rowcount + plot_layout(ncol=2, widths = c(5,1), heights = c(1,5)))
}