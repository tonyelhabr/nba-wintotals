
remove(list = ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

dir_data <- "C:/Users/aelhabr/Dropbox/personal/data/"

# getwd()
setwd(dir_data)
d <- read_csv(paste0(dir_data, "nba_picks-win_totals_processed.csv"))

d <- d %>%
  select(season_year_start,
         team,
         num_wins_actual,
         win_diff,
         result_calc,
         person, pick,
         pick_result_calc)

ui <- fluidPage(
   
  titlePanel("NBA Picks"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("person_input", "Person",
                 choices = c("tony", "andrew"),
                 selected = "tony"),
      sliderInput("num_wins_input",
                  "Filter for number of wins:",
                  min = 0,
                  max = 82,
                  value = c(9, 72)),
      uiOutput("team_output")
    ),
    
    mainPanel(
      plotOutput("plot_output"),
      br(), br(),
      dataTableOutput("table_output")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$team_output <- renderUI({
    selectInput("team_input", "Team",
                sort(unique(d$team)),
                selected = "SAS")
    
  })
  
  d_filtered <- reactive({
    if (is.null(input$team_input)) {
      return(NULL)
    }

    d %>%
    filter(num_wins_actual >= input$num_wins_input[1],
           num_wins_actual <= input$num_wins_input[2],
           team == input$team_input,
           person == input$person_input)
    })
  
  
  output$plot_output <- renderPlot({
    if (is.null(d_filtered())) {
      return()
    }
    ggplot(d_filtered(), aes(x = season_year_start,
                           y = num_wins_actual)) +
      geom_line()
  })
  
  output$table_output <- renderDataTable({
    d_filtered()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

