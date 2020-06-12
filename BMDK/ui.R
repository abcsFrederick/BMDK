#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("BioMarker Discovery Kit"),
  
  # Sidebar with a slider input with two file inputs, one for training data set and another for test data set 
  sidebarLayout(
    sidebarPanel(
       fileInput("file1", "Choose Training CSV File",
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values, text/plain",
                   ".csv"),
                 placeholder = "No file selected"
                 ),
       tags$hr(),
       checkboxInput("header", "Header", TRUE),  # Will display header if the box is checked
       
       
       fileInput("file2", "Choose Test CSV File",
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values, text/plain",
                   ".csv"),
                 placeholder = "No file selected"
                 ),
       tags$hr(),
       checkboxInput("header", "Header", TRUE)
       
       
    ),
    
    # Show a table of the selected dataset named "contents"
    ###### only showing one for right now
    mainPanel(
       tableOutput("contents")
    )
  )
))
