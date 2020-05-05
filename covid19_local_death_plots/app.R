# A simple app for exploring the ONS death data for COVID-19 
# across local authorities.

# Load packages
library(shiny)
require(tidyverse)
require(readxl)
library(lubridate)

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
                               selected = c("Care home", "Hospital", "Home"))

        ),

        # Show a plot of the generated distribution
        mainPanel(
            mainPanel(
                plotOutput("la_death_plot_2"),
                plotOutput("la_death_plot_1"),
                width = 12
            )
        )
    )
)

# Define server logic required to draw the plot
server <- function(input, output) {
    
    output$la_death_plot_1 <- renderPlot({
        # draw the plot
        ggplot(data = filter(deaths, 
                             area_name == input$area,
                             cause_of_death == input$cause,
                             place_of_death %in% input$place),
               aes(x = week_number,
                   y = number_of_deaths,
                   group = place_of_death,
                   colour = place_of_death)) +
            geom_point() +
            geom_smooth(se = FALSE,
                        linetype = 1) +
            theme_classic() +
            labs(x = "week number",
                 y = "no. of deaths",
                 colour = "place of death",
                 title = paste0("Deaths due to ",
                                input$cause,
                                " in ",
                                input$area,
                                " by week number"),
                 caption = "Data source: ONS.") +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  plot.caption = element_text(hjust = 0, size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
    })
    
    output$la_death_plot_2 <- renderPlot({
        # Tabulate deaths due to selected cause in selected area
        death_table <- deaths %>% 
            filter(cause_of_death == input$cause,
                   area_name == input$area,
                   place_of_death %in% input$place) %>%
            group_by(place_of_death) %>%
            summarise(total_deaths = sum(number_of_deaths))
        
        # Plot the deaths by cause
        ggplot(data = death_table,
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
                                input$area),
                 caption = "Data source: ONS.") +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  plot.caption = element_text(hjust = 0, size = 12),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 12))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
