library(gridlayout)
library(shiny)
library(ggplot2)
library(googlesheets4)

# Chick weights investigated over three panels
ui <- navbarPage(
  title = "Random Workouts",
  collapsible = FALSE,
  theme = bslib::bs_theme(),
  tabPanel(title = "Exercises"),
  tabPanel(title = "Randomizer"),
  tabPanel(title = "Activity Log")
)

# Define server logic
server <- function(input, output) {
  exercises = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_2tUyBnXQ3zsnnSoyy9gzK-hOpmnWqgd9fsM9v6sUW8/edit#gid=0",sheet = "Exercise DB")
  render
}

shinyApp(ui, server)
