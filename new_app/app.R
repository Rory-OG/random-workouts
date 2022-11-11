library(gridlayout)
library(import)
library(shiny)
library(ggplot2)
library(googlesheets4)
library(dplyr)

# Chick weights investigated over three panels
ui <- navbarPage(
  title = "Random Workouts",
  collapsible = FALSE,
  theme = bslib::bs_theme(),
  tabsetPanel(
    id="tabs",
    tabPanel(title = "Exercises",DT::dataTableOutput("exercise_table")),
    tabPanel(title = "Randomizer",
             actionButton("rw","Randomize"),
             actionButton("dw","Do this workout"),
             DT::dataTableOutput("random_workout")),
    tabPanel(title = "Activity Log",
             DT::dataTableOutput("chosen_workout")
             )
))
# Define server logic
server <- function(input, output, session) {
  # Display all possible exercises
  exercises = googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1_2tUyBnXQ3zsnnSoyy9gzK-hOpmnWqgd9fsM9v6sUW8/edit#gid=0",
    sheet = "Exercise DB"
  )
  output$exercise_table = DT::renderDT(exercises)
  
  # Generates a random workout the user can review to pick a workout
  observe({
    random_workout = exercises %>% group_by(`Body part`) %>% slice_sample(n = 1) %>% ungroup()
    rand_work <<- random_workout %>% select(-`Body part`) %>% mutate(sets = "",reps="")
    output$random_workout = DT::renderDT(random_workout)
  }) %>%
    bindEvent(input$rw) 
    
  
# Copies the workout to activity log and gives input columns for resistance, sets and reps
  observe({
    output$chosen_workout = DT::renderDT(rand_work,editable = list(target = 'column',disable = list(columns = 1)))
    updateTabsetPanel(session = session,"tabs", selected = "Activity Log")
  }) %>%
    bindEvent(input$dw)
  }

shinyApp(ui, server)
