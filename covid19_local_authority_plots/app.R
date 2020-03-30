# load packages
library(shiny); 
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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Local Authority Confirmed COVID-19 Cases"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("la",
                        "Select a local authority",
                        choices = sort(unique(d$UTLA)),
                        selected = "bury"),
            
            numericInput("numdays",
                         "Select number of days' data to use to estimate doubling rate",
                         value = as.numeric(today() - dmy("16/03/2020")),
                         min = 10,
                         step = 1),
            
            h4("Instructions:"),
            
            p("Select a local authority (a local government area in England). 
			  The app will produce plots of daily new cases and total cases
              of COVID-19."),
            
            h4("About:"),
            
            p("This app produces simple plots of the numbers of confirmed cases of
              COVID-19, as reported on the PHE COVID-19 tracker. The data used is
              drawn from a GitHub repository by Emma Doughty, who has been collating
              the daily cases by local authority."),
            
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
            
            a(href = "https://github.com/emmadoughty/Daily_COVID-19",
              "Link to Emma Doughty's excellent repository on GitHub")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("la_case_plot"),
           textOutput("summary"),
           textOutput("doubletime")
        )

    )
)

# Define server logic
server <- function(input, output) {
    
    # Function for plotting
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
                         y = confirm)) +
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
        d <- filter(data, UTLA == la_name) %>%
             mutate(new_cases = case_when(
                    new_cases < 0 ~ 0,
                    new_cases >= 0 ~ new_cases
            ))
    
        # Get total cases, new cases,
        total <- d$confirm[d$date == max(d$date)]
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
    
    # Function to find doubling time - poisson model
    # Estimate proportion increase in rate of new cases daily
    # Then calculate doubling time 
    
    local_doubling_time <- function(data = d, 
                                    la_name = "bury",
                                    n_days = 10)
    {
        
        # Filter data to single local authority
        d <- filter(data, UTLA == la_name) %>%
            mutate(new_cases = case_when(
                new_cases < 0 ~ 0,
                new_cases >= 0 ~ new_cases
            ))
        
        d$new_cases[1] <- d$confirm[1]
        
        # Fit a poisson model
        m <- glm(new_cases ~ date,
                 data = filter(d, date > today() - n_days),
                 family = poisson)
        
        # Calculate doubling time
        t_dbl <- log(2)/coef(m)[2]
        
        return(paste0("Based on data from the last ",
                      n_days,
                      " the number of new confirmed cases is doubling approximately every ",
                      round(t_dbl, digits = 1),
                      " days."))
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
    
    output$doubletime <- renderText({
        local_doubling_time(data = d, 
                            la_name = input$la,
                            n_days = input$numdays)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
