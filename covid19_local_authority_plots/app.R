# load packages
library(shiny); 
library(tidyverse)
library(lubridate)
library(cowplot)
library(tools)

# Source functions for getting data
source("covid19_getdata.R")

# Grab daily data
json <- fetch_datafile()
d1 <- get_cumulative_cases_utla(json = json) %>%
      select(-value_name, -area_type) %>%
      rename(cum_cases = number)


d2 <- get_daily_cases_utla(json = json) %>%
      select(-value_name, -area_type, -area_name) %>%
      rename(new_cases = number)

d <- merge(d1, d2, by = c("date", "area_code"))

rm(d1, d2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Local Authority Confirmed COVID-19 Cases"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("la",
                        "Select a local authority",
                        choices = sort(unique(d$area_name)),
                        selected = "Bury"),
            
            h4("Instructions:"),
            
            p("Select a local authority (a local government area in England). 
			  The app will produce plots of daily new cases and total cases
              of COVID-19."),
            
            h4("About:"),
            
            p("This app produces simple plots of the numbers of confirmed cases of
              COVID-19, as reported on the PHE COVID-19 tracker. The data used is
              drawn from a the PHE data using a script written by Oli Hawkins."),
            
            h4("Caveats:"),
            
            p("There are known data quality issues with the data source.
              The numbers of confirmed cases are a function of the numbers of people
              that are tested. Testing availability may vary between local authorities.
              Testing availability has also changed over time.
              This means that the results should be treated with caution. 
              The plots produced here should be used for illustrative purposes only.
              You have been warned."),

            a(href = "https://github.com/stevenlsenior/covid19_local_plots",
              "Link to source code on GitHub"),
            
            p(),
            
            a(href = "https://gist.github.com/olihawkins/6962c6df563e3dbea8917dbf6fd4ab01",
              "Link to Oli Hawkins's excellent script on GitHub")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("la_case_plot"),
           textOutput("summary")
        )

    )
)

# Define server logic
server <- function(input, output) {
    
    # Function for plotting
    local_covid_plot <- function(la_name, data){

        # Filter data to single local authority
        d <- filter(data, area_name == la_name) 
        
        # Make a plot
        g1 <- ggplot(data = d,
                     aes(x = date,
                         y = new_cases)) +
            geom_bar(stat = "identity",
                     fill = "darkblue") +
            theme_minimal() +
            labs(x = NULL,
                 y = "New cases") +
            theme(plot.caption = element_text(hjust = 0)) +
            scale_y_continuous(breaks  = 1:max(d$new_cases)) +
            theme(panel.ontop = TRUE,
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(size = 0.5, colour = "white"),
                  panel.grid.minor.y = element_blank())
        
        g2 <- ggplot(data = d,
                     aes(x = date,
                         y = cum_cases)) +
            geom_bar(stat = "identity",
                     fill = "darkblue") +
            theme_minimal() +
            labs(x = NULL,
                 y = "Total cases") +
            theme(plot.caption = element_text(hjust = 0)) +
            theme(panel.ontop = TRUE,
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(size = 0.5, colour = "white"),
                  panel.grid.minor.y = element_line(size = 0.5, colour = "white"))
        
        g <- plot_grid(g1, g2,
                       ncol = 1,
                       rel_heights = c(0.5, 0.5))
        
        return(g)
    }
    
    local_daily_covid <- function(la_name, data){
        
        # Ensure names are all lower case
        la_name <- tolower(la_name)
        
        # Filter data to single local authority
        d <- filter(data, area_name == la_name) 
    
        # Get total cases, new cases,
        total <- d$cum_cases[d$date == max(d$date)]
        new <- d$new_cases[d$date == max(d$date)]
        date <- max(d$date)
        
        # Return text summary
        return(paste0("As of ",
                      date,
                      " there were ",
                      total,
                      " confirmed cases of COVID-19 in ",
                      input$la,
                      ". This was an increase of ",
                      new,
                      " cases on the previous day."))
        
    }

    output$la_case_plot <- renderPlot({
        # draw the plot
        local_covid_plot(data = d, la_name = input$la)
    })
    
    output$summary <- renderText({
        # get the summary
        local_daily_covid(data = d, 
                          la_name = input$la)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
