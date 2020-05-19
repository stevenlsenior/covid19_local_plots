# A simple app for exploring the ONS death data for COVID-19 
# across local authorities.

# Load packages
library(shiny)
require(tidyverse)
require(readxl)
library(lubridate)

#### Cases ####
source("covid19_getdata.R")

# Grab daily cases data
json <- fetch_datafile("https://coronavirus.data.gov.uk/downloads/json/coronavirus-cases_latest.json")

d1 <- get_cumulative_cases_utla(json = json) %>%
    select(-value_name, -area_type) %>%
    rename(cum_cases = number)


d2 <- get_daily_cases_utla(json = json) %>%
    select(-value_name, -area_type, -area_name) %>%
    rename(new_cases = number)

cases <- merge(d1, d2, by = c("date", "area_code"))

rm(d1, d2)

#### Deaths ####

# Get current week number
now <- today()
week_no <- week(now)

# URL for data set 
u <- paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2020/lahbtablesweek",
            week_no - 1, # 2-3 week delay on data
            ".xlsx")

# Download file
download.file(url = u,
              destfile = "covid19_deaths.xlsx",
              method = "curl",
              mode = "wb")

# Load data
deaths <- read_excel(path = "covid19_deaths.xlsx",
                     sheet = 6,
                     skip = 3,
                     col_names = TRUE)

# Sort out variable names: lower case, no spaces
names(deaths) <- gsub(" ", "_", names(deaths))
names(deaths) <- tolower(names(deaths))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 deaths explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "area",
                        label = "Select a local authority",
                        choices = sort(unique(deaths$area_name)),
                        selected = "Bury"),
            
            selectInput(inputId = "cause",
                        label = "Choose cause(s) of death to compare",
                        choices = c("All causes", "COVID 19"),
                        selected = "All causes"),
            
            checkboxGroupInput(inputId = "place",
                               label = "Choose place(s) of death to compare",
                               choices = sort(unique(deaths$place_of_death)),
                               selected = c("Care home", "Hospital", "Home")),
            
            checkboxInput(inputId = "smooth",
                          value = TRUE,
                          label = "Show smoothed trendline?")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            mainPanel(
                plotOutput("la_case_plot"),
                p(" "),
                plotOutput("la_death_plot"),
                p(" "),
                width = 12
            )
        )
    )
)

# Define server logic required to draw the plot
server <- function(input, output) {
    
    # Function for plotting
    
    output$la_case_plot <- renderPlot({
        
        # draw the plot
        g_case_1 <- ggplot(data = filter(cases, area_name == input$area),
                     aes(x = date,
                         y = new_cases)) +
            geom_bar(stat = "identity",
                     fill = "darkblue") +
            geom_smooth() +
            labs(x = NULL,
                 y = NULL,
                 title = paste0("New confirmed cases of COVID-19 in ",
                                input$area,
                                " by date")) +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
            
        
        g_case_2 <- ggplot(data = filter(cases, area_name == input$area),
                     aes(x = date,
                         y = cum_cases)) +
            geom_bar(stat = "identity",
                     fill = "darkblue") +
            geom_smooth() +
            labs(x = NULL,
                 y = NULL,
                 title = paste0("Total confirmed cases of COVID-19 in ",
                                input$area,
                                " by date")) +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
        
        g_case_title <- ggplot() +
                        labs(title = paste0("Confirmed cases of COVID-19 in ", input$area)) +
                        theme_classic() +
                        theme(plot.title = element_text(size = 14, face = "bold"))
        
        g_case_capt <- ggplot() + 
                       labs(caption = "Data source: PHE.") +
                       theme_classic() +
                       theme(plot.caption = element_text(size = 12))
        
        g_case <- plot_grid(g_case_1, g_case_2, ncol = 1, labels = "AUTO")
        
        plot_grid(g_case_title, g_case, g_case_capt, ncol = 1, rel_heights = c(0.08, 1, 0.05))
    })
    
    output$la_death_plot <- renderPlot({
        # Tabulate deaths due to selected cause in selected area
        death_table <- deaths %>% 
            filter(cause_of_death == input$cause,
                   area_name == input$area,
                   place_of_death %in% input$place) %>%
            group_by(place_of_death) %>%
            summarise(total_deaths = sum(number_of_deaths))
        
        death_totals <- deaths %>%
            filter(cause_of_death == input$cause,
                   area_name == input$area) %>%
            group_by(week_number) %>%
            summarise(total_deaths = sum(number_of_deaths))
            
        # Plot the deaths by cause
        g_death_0 <- ggplot(data = death_totals,
                     aes(x = week_number,
                         y = total_deaths)) +
              geom_point(colour = 9) +
              stat_smooth(geom = "line",
                          se = FALSE,
                          alpha = as.numeric(input$smooth),
                          colour = 9) +
              labs(x = "week number",
                   y = "no. of deaths",
                   title = paste0("Total deaths due to ",
                                  input$cause,
                                  " in ",
                                  input$area,
                                  " by week number")) +
              guides(colour = FALSE) +
              theme_classic() +
              theme(plot.title = element_text(face = "bold", size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 12),
                    legend.title = element_text(face = "bold", size = 12),
                    legend.text = element_text(size = 12))
        
        g_death_1 <- ggplot(data = filter(deaths, 
                                   area_name == input$area,
                                   cause_of_death == input$cause,
                                   place_of_death %in% input$place),
                     aes(x = week_number,
                         y = number_of_deaths,
                         group = place_of_death,
                         colour = place_of_death)) +
            geom_point() +
            stat_smooth(geom = "line",
                        se = FALSE,
                        linetype = 1,
                        alpha = as.numeric(input$smooth)) +
            theme_classic() +
            labs(x = "week number",
                 y = "no. of deaths",
                 colour = "place of death",
                 title = paste0("Deaths due to ",
                                input$cause,
                                " in ",
                                input$area,
                                " by place of death and week number")) +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))

        g_death_2 <- ggplot(data = death_table,
               aes(x = place_of_death,
                   y = total_deaths,
                   colour = place_of_death,
                   fill = place_of_death)) +
            geom_bar(stat = "identity") +
            theme_classic() +
            guides(fill = FALSE,
                   colour = FALSE) +
            labs(x = NULL,
                 y = "no. of deaths",
                 title = paste0("Deaths due to ",
                                input$cause,
                                " in ",
                                input$area,
                                " by place of death")) +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
        
        g_death_title <- ggplot() +
            labs(title = paste0("Deaths attributed to COVID-19 in ", input$area)) +
            theme_classic() +
            theme(plot.title = element_text(size = 14, face = "bold"))
        
        g_death_capt <- ggplot() + 
            labs(caption = "Data source: ONS.") +
            theme_classic() +
            theme(plot.caption = element_text(hjust = 0, size = 12))
        
        g_death <- plot_grid(g_death_0, g_death_1, g_death_2, ncol = 1, labels = "AUTO")
        
        plot_grid(g_death_title, g_death, g_death_capt, ncol = 1, rel_heights = c(0.08, 1, 0.05))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
