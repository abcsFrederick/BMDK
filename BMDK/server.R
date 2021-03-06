#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
#library(BMDK)
source('../R/read_bmdk.R')

# Define server logic required to define a table

shinyServer(function(input, output) {
  # Rendering a table for "contents"
  output$contents <- renderTable({
    # Naming a variable that selects the file input from "file1"
    inFile <- input$file1
    
    
    # Input$file1 will be NULL initially
    # User will select file with 'name', 'type', and 'datapath' columns
    # This datapath column contains filenames where the data can be retrieved
    if (is.null(inFile))
      return(NULL)
    
    # Reading in the csv file
    
    read_bmdk(inFile$datapath) %>%
      head()
    
    
    
  })
  # Repeating the same code for the second dataset
  output$contents2 <- renderTable({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    
    read_bmdk(inFile2$datapath) %>%
      head()
    
  })
  
})
