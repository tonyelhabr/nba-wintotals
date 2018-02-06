
remove(list = ls())
library(tidyverse)
library(shiny)
library(DT)

dir_nba <- "C:/Users/aelhabr/Dropbox/data_science/projects/nba/"
setwd(dir_nba)
v_raw <- read.csv(paste0(dir_nba, "v_rs_win_totals.csv"))

v <- v_raw %>%
  select(yr_start,
         conf_name, team,
         w,
         w_diff,
         result_calc,
         person,
         pick,
         pick_result_calc,
         # `points-per-game`,
         rank)

# yrs <- seq(2012, 2016)
yrs <- seq.int(2012, 2016)

theme_format <-
  theme(
    axis.text = element_text(size = 14), 
    axis.title = element_blank(), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    # strip.text = element_text(size = 14), 
    legend.position = "none" # legend.position = "bottom",
    # axis.text.x = element_blank()# , 
    # legend.title = element_blank()
  )

ui <- fluidPage(
   
  titlePanel("NBA Picks"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("person_input", "Person",
                 choices = c("Tony", "Andrew"),
                 selected = "Tony"),
      sliderInput("seasons_input",
                  "Filter for seasons:",
                  # This generates resolution weirdness with slider labels.
                  # min = yrs[1],
                  # max = yrs[length(yrs)],
                  # value = c(yrs[1], yrs[length(yrs)]),
                  min = 2012,
                  max = 2016,
                  value = c(2012, 2016),
                  sep = ""),
      uiOutput("conf_output"),
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

  
  output$conf_output <- renderUI({
    selectInput("conf_input", "Conference",
                sort(unique(v$conf_name)),
                selected = "West")
  })
    
  output$team_output <- renderUI({
    selectInput("team_input", "Team",
                sort(unique(v$team)),
                selected = "SA")
    
  })

  v_filt <- reactive({
    if (is.null(input$team_input)) {
      return(NULL)
    }

    v %>%
    filter(yr_start >= input$seasons_input[1],
           yr_start <= input$seasons_input[2],
           team == input$team_input,
           person == input$person_input)
    })
  
  
  output$plot_output <- renderPlot({
    if (is.null(v_filt())) {
      return()
    }
    ggplot(v_filt(),
           aes(x = yr_start,
               # y = w)) +
               # y = `points-per-game` #) +
               y = rank,
               fill = pick_result_calc)) +
      geom_col() +
      ggtitle("Picks For Chosen Season(s)") +
      theme_format
  })
  
  output$table_output <- renderDataTable({
    v_filt()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

