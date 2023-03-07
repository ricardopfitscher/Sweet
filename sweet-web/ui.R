#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SWEET - Soil unit WEight Estimator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      downloadButton("downloadData", "Download Example", style = "background-color: #005266 ; border: 1px solid #005266 ; color: white"),
      
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      ,
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Comma'),
      tags$hr(),
      p("Feed forms to add data directly:"),

      textInputRow(inputId="G", label="G", value = 0.0),
      textInputRow(inputId="qt", label="qt", value = 0.0),
      textInputRow(inputId="fs", label="fs", value = 0.0),
      textInputRow(inputId="u", label="u ", value = 0.0),
      textInputRow(inputId="soil.name", label="Soil name", value = "Example"),
      actionButton("submit", "Add")
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput(outputId = 'table.output')
      
    )
  )
)