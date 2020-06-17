#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that displays two tables
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("BioMarker Discovery Kit"),
  
  # Sidebar with two file inputs, one for training data set and another for test data set
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1",
        "Choose Training TXT File",
        accept = c("text/txt",
                   "text/tab-delimited-text, text/plain",
                   ".txt"),
        placeholder = "No file selected"
      ),
      
      
      
      fileInput(
        "file2",
        "Choose Test TXT File",
        accept = c("text/txt",
                   "text/tab-delimited-text, text/plain",
                   ".txt"),
        placeholder = "No file selected"
      )
    ),
    
    # Show a table of the selected data set named "contents"
    mainPanel(tableOutput("contents"),
              tableOutput("contents2"))
  )
))
