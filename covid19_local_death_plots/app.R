# A simple app for exploring the ONS death data for COVID-19 
# across local authorities.

# Load packages
require(shiny)
require(tidyverse)
require(readxl)
require(lubridate)
require(cowplot)
require(zoo)

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
            week_no - 2, # 2-3 week delay on data
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

# Calculate dates for week endings
year_start <- dmy("01/01/2020")
deaths$week_end <- 7*deaths$week_number + year_start

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
                        selected = "COVID 19"),
            
            checkboxGroupInput(inputId = "place",
                               label = "Choose place(s) of death to compare",
                               choices = sort(unique(deaths$place_of_death)),
                               selected = c("Care home", "Hospital"))

        ),

        # Show a plot of the generated distribution
        mainPanel(
            mainPanel(
                h4(strong("Cases")),
                br(),
                textOutput("case_summary"),
                br(),
                plotOutput("la_case_plot"),
                br(),
                br(),
                h4(strong("Deaths")),
                br(),
                textOutput("death_summary"),
                br(),
                plotOutput("la_death_plot"),
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
                     fill = "cyan4") +
            geom_line(aes(y=rollmean(new_cases, 7, na.pad=TRUE))) +
            labs(x = NULL,
                 y = NULL,
                 title = paste0("Confirmed cases of COVID-19 in ",
                                input$area,
                                " by date"),
                 subtitle = "Daily new cases (bars) plus 7-day rolling average (line)") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12, colour = "grey40"),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
            
        
        g_case_2 <- ggplot(data = filter(cases, area_name == input$area),
                     aes(x = date,
                         y = cum_cases)) +
            geom_bar(stat = "identity",
                     fill = "cyan4") +
            geom_line(aes(y=rollmean(cum_cases, 7, na.pad=TRUE))) +
            labs(x = NULL,
                 y = NULL,
                 title = paste0("Confirmed cases of COVID-19 in ",
                                input$area,
                                " by date"),
                 subtitle = "Cumulative cases by day (bars) and 7-day rolling average (line)") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12, colour = "grey40"),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
        
        g_case_capt <- ggplot() + 
                       labs(caption = "Data source: PHE.") +
                       theme_classic() +
                       theme(plot.caption = element_text(size = 12))
        
        g_case <- plot_grid(g_case_1, g_case_2, ncol = 1, labels = "AUTO")
        
        plot_grid(g_case, g_case_capt, ncol = 1, rel_heights = c(1, 0.05))
    })
    
    output$la_death_plot <- renderPlot(
        height = 600,
        {
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
            group_by(week_end) %>%
            summarise(total_deaths = sum(number_of_deaths))
            
        # Plot the deaths by cause
        g_death_0 <- ggplot(data = death_totals,
                     aes(x = week_end,
                         y = total_deaths)) +
              geom_point(colour = 9) +
              geom_line(colour = 9) +
              labs(x = NULL,
                   y = "no. of deaths",
                   title = paste0("Total weekly deaths due to ",
                                  input$cause,
                                  " in ",
                                  input$area)) +
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
                     aes(x = week_end,
                         y = number_of_deaths,
                         group = place_of_death,
                         colour = place_of_death)) +
            geom_point() +
            geom_line() +
            theme_classic() +
            labs(x = NULL,
                 y = "no. of deaths",
                 colour = "place of death",
                 title = paste0("Weekly deaths due to ",
                                input$cause,
                                " in ",
                                input$area,
                                " by place of death")) +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12),
                  legend.position = "bottom")

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
        
        g_death_capt <- ggplot() + 
            labs(caption = "Data source: ONS.") +
            theme_classic() +
            theme(plot.caption = element_text(hjust = 0, size = 12))
        
        g_death <- plot_grid(g_death_0, g_death_1, g_death_2, ncol = 1, labels = "AUTO")
        
        plot_grid(g_death, g_death_capt, ncol = 1, rel_heights = c(1, 0.05))
    })
    
    output$case_summary <- renderText({
        # get most recent date
        date <- max(cases$date)
        
        # get new cases in last day
        last_day <- cases %>%
                       filter(area_name == input$area) %>%
                       filter(date == max(date)) %>%
                       pull(new_cases)
        
        # get new cases in last 7 days
        last_week <- cases %>%
                     filter(area_name == input$area) %>%
                     filter(date > max(date) - 7) %>%
                     pull(new_cases) %>%
                     sum()
        
        # get total cases
        total_cases <- cases %>% 
                       filter(area_name == input$area) %>%
                       filter(date == max(date)) %>%
                       pull(cum_cases)
        
        # create text output
        paste0("As of ",
              date,
              " there were ",
              total_cases,
              " cases of COVID-19 in ",
              input$area,
              ". This was an increase of ",
              last_day,
              " cases on the previous day and ",
              last_week,
              " cases over the previous week (an average of ",
              round(last_week/7, digits = 1),
              " new cases per day).")
                    
    })
    
    output$death_summary <- renderText({
        # get total deaths
        total_deaths <- deaths %>% 
                        filter(area_name == input$area,
                               cause_of_death == input$cause) %>%
                        pull(number_of_deaths) %>%
                        sum()
        
        # get care home deaths
        care_home_deaths <- deaths %>% 
            filter(area_name == input$area,
                   cause_of_death == input$cause,
                   place_of_death == "Care home") %>%
            pull(number_of_deaths) %>%
            sum()
        
        # get hospital deaths
        hospital_deaths <- deaths %>% 
            filter(area_name == input$area,
                   cause_of_death == input$cause,
                   place_of_death == "Care home") %>%
            pull(number_of_deaths) %>%
            sum()
        
        # get deaths in the most recent week
        last_week_deaths <- deaths %>%
            filter(area_name == input$area,
                   cause_of_death == input$cause,
                   week_number == max(week_number)) %>%
            pull(number_of_deaths) %>%
            sum()
        
        # Calculate date for end of most recent week
        year_start <- dmy("01/01/2020")
        week_end <- year_start + max(deaths$week_number)*7
        
        # create text output
        paste0("As of the week ending ",
               week_end,
               " there were ",
               total_deaths,
               " deaths due to ",
               input$cause,
               " in ",
               input$area,
               ". This included ",
               care_home_deaths,
               " in care homes and ",
               hospital_deaths,
               " in hospitals. This was an increase in ",
               last_week_deaths,
               " total deaths in the last week.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
