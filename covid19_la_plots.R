# This script pulls daily covid cases from Emma Doughty's github page
# And produces some very basic plots.
# The data is scraped from the PHE covid tracker - there are known data quality
# Issues, so everything should be treated with caution.

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot)
library(tools)

# Grab daily data
u <- "https://raw.githubusercontent.com/emmadoughty/Daily_COVID-19/master/Data/cases_by_utla.csv"

download.file(url = u,
              destfile = "coivd19_daily_cases.csv",
              method = "curl")

d <- read.csv("coivd19_daily_cases.csv",
              header = TRUE,
              stringsAsFactors = FALSE)

d <- d %>%
  mutate(date = dmy(date),
         confirm = as.numeric(confirm)) %>%
  mutate_if(is.character, tolower) %>%
  group_by(UTLA) %>%
  mutate(new_cases = confirm - lag(confirm))

# Fix specific issue in Bury cases
d$confirm[d$date == dmy("07/03/2020") & d$UTLA == "bury"] <- 3

# Plot function
local_covid_plot <- function(la_name, data){
  # Ensure names are all lower case
  la_name <- tolower(la_name)
  
  # Filter data to single local authority
  d <- filter(data, UTLA == la_name) %>%
    mutate(new_cases = case_when(
      new_cases < 0 ~ 0,
      new_cases >= 0 ~ new_cases
    ))
  
  d$new_cases[1] <- d$confirm[1]
  
  # Make a plot
  g1 <- ggplot(data = d,
               aes(x = date,
                   y = new_cases)) +
    geom_bar(stat = "identity",
             colour = "black") +
    theme_bw() +
    labs(x = NULL,
         y = "New cases") +
    theme(plot.caption = element_text(hjust = 0))
  
  g2 <- ggplot(data = d,
               aes(x = date,
                   y = confirm)) +
    geom_bar(stat = "identity",
             colour = "black") +
    theme_bw() +
    labs(x = NULL,
         y = "Total cases") +
    theme(plot.caption = element_text(hjust = 0))
  
  g_title <- ggplot() +
    labs(title = "Confirmed cases of COVID-19",
         subtitle = paste0("Local authority: ",
                           toTitleCase(la_name))) +
    theme_minimal()
  
  g_caption <- ggplot() +
    labs(caption = "WARNING: Data is provisional and subject to data quality issues. Treat with caution.") +
    theme(plot.caption = element_text(hjust = 0)) +
    theme_minimal()
  
  g <- plot_grid(g_title, g1, g2, g_caption,
                 ncol = 1,
                 rel_heights = c(0.25, 0.5, 0.5, 0.1))
  
  ggsave(filename = paste0("covid_plot_",
                           today(),
                           ".jpg"),
         plot = g,
         device = "jpeg",
         units = "cm",
         width = 16, 
         height = 9)
  
  return(g)
}

# Initial plot for bury
local_covid_plot(data = d, la_name = "bury")

