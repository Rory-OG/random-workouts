library(gridlayout)
library(shiny)
library(ggplot2)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(DataEditR)

# Randomized workouts generated from exercise DB and recording in an activity log that appends to google sheets
ui <- navbarPage(
  title = "Random Workouts",
  collapsible = FALSE,
  theme = bslib::bs_theme(),
  id = "tabs",
  tabPanel(
    title = "Randomizer",
    actionButton("rw", "Randomize"),
    actionButton("dw", "Do this workout"),
    DT::dataTableOutput("random_workout")),
  tabPanel(title = "Activity Log",
           dataEditUI("edit-1"),
           actionButton("record","Save to Activity Log")),
  tabPanel(title = "Exercises", DT::dataTableOutput("exercise_table"))
)
# Define server logic
server <- function(input, output, session) {
  # Display all possible exercises
  # gs4_auth(cache = ".secrets", email = TRUE)
  url <- "https://docs.google.com/spreadsheets/d/1_2tUyBnXQ3zsnnSoyy9gzK-hOpmnWqgd9fsM9v6sUW8/edit#gid=0"
  exercises = googlesheets4::read_sheet(url,sheet = "Exercise DB")
  output$exercise_table = DT::renderDT(exercises)
  
  # Generates a random workout the user can review to pick a workout
  observe({
    random_workout = exercises %>% group_by(`Body part`) %>% slice_sample(n = 1) %>% ungroup()
    rand_work <<- random_workout %>%
      select(`Body part`, Exercise) %>%
      mutate(Date = Sys.Date(),
             resistance = "",
             sets = "",
             reps = "") %>%
      relocate(Date) %>% 
      reactiveValues(data = .)
    output$random_workout = DT::renderDT(random_workout)
  }) %>%
    bindEvent(input$rw) 
  
  data_edit = dataEditServer("edit-1", data = rand_work$data)
  # Copies the workout to activity log and gives input columns for resistance, sets and reps
  observe({
    updateTabsetPanel(session = session, "tabs", selected = "Activity Log")
    rand_work$data = data_edit()
    }) %>%
    bindEvent(input$dw)
  # handles output and save the data to an object 
  dataOutputServer("output-1",rand_work$data)
  
  # writes the data to the google sheet
  observe({
    ask_confirmation(inputId = "confirm1",
                     title = "Finished with your workout? Confirm to save activity")
  }) %>% bindEvent(input$record)  
  observe({
    if (isTRUE(input$confirm1)) {
      sheet_append(ss = url,
                   data = rand_work$data,
                   sheet = "Activity Log")
    } else{
      F
    }
  }) %>%
    bindEvent(input$confirm1)
  }
# writes the data to the 
shinyApp(ui, server)