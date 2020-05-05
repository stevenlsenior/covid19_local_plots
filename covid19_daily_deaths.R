# Load packages
require(tidyverse)
require(readxl)

# DOWNLOAD DATA
# Deaths released with 2 week delay
# URL contains week number so script should automatically grab latest sheet

# Get current week number
now <- today()
week_no <- week(now)

# URL for data set 
u <- paste0("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek",
            week_no - 2, # 2 week delay on data
            "new.xlsx")

# Download file
download.file(url = u,
              destfile = "covid19_deaths.xlsx",
              method = "curl",
              mode = "wb")

# LOAD DATA
deaths <- read_excel(path = "covid19_deaths.xlsx",
                     sheet = 6,
                     skip = 3,
                     col_names = TRUE)

# Sort out variable names: lower case, no spaces
names(deaths) <- gsub(" ", "_", names(deaths))
names(deaths) <- tolower(names(deaths))

# PLOTTING

# Generic function for plotting by cause, place, and area
covid_deaths_plot <- function(area = "Bury", cause = c("All causes", "COVID 19"), place = unique(deaths$place_of_death)){
  d <- deaths %>% filter(area_name %in% area, 
                         cause_of_death %in% cause, 
                         place_of_death %in% place)
  
  g <- ggplot(data = d,
              aes(x = week_number,
                  y = number_of_deaths,
                  group = place_of_death,
                  colour = place_of_death)) +
       geom_point() +
       geom_smooth(se = FALSE) +
       #if(length(cause) > 1){facet_grid(cols = vars(cause_of_death)) +
       theme_classic() +
       labs(x = "week number",
            y = "no. of deaths",
            colour = "place of death",
            title = paste0("Deaths from ",
                           cause,
                           " in ",
                           area),
            subtitle = "2020 by week number",
            caption = "Data source: ONS") +
       theme(plot.title = element_text(size = 11, face = "bold"),
             plot.caption = element_text(hjust = 0))
  
  return(g)
}

# Plot for Bury
covid_deaths_plot(area = "Bury")

# Table of total deaths
death_table <- deaths %>% 
  filter(cause_of_death == "COVID 19") %>%
  group_by(area_name, place_of_death) %>%
  summarise(covid_deaths = sum(number_of_deaths))

View(death_table)
  
