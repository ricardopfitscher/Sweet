#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(neuralnet)
library(dplyr)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("example", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("data/example.csv", file)
    },
    contentType = "text/csv"
  )
  

  values <- reactiveValues(inData = NA)
  database <- NA
  model_log <- readRDS("data/model_log.rds")
  model_nn <- readRDS("data/model_nn_train_data.rds")
  
  observeEvent(input$submit,{
      values$inData <- data.frame(input$soil.name,as.numeric(input$G),as.numeric(input$qt),as.numeric(input$fs),as.numeric(input$u))
      names(values$inData) <- c("soil.name","G","qt","fs","u")
  
  })
  

  
  output$table.output <- renderTable({

    inFile <- input$file1
   
    if (is.null(inFile)){
      tbl <- read.csv("data/example.csv", sep = ",", header = T, dec = ".", stringsAsFactors=TRUE)
    }
    else{
      tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec = ".",stringsAsFactors=TRUE)
      database <- as.data.frame(tbl)
    }
    
    if(is.null(nrow(database))){
      database <- as.data.frame(tbl)
    }

    if(!is.null(values$inData) && input$submit>0) {
        database<-rbind(database,values$inData)
    }
    database$logFs <- log(database$fs+1)
    database$logqt <- log(database$qt+1)
    database$UTransf <- log(database$u+1)
    database$yhat_lm_log <- predict(model_log, database)
    database_model <- select(database, c(G,qt,fs,u,yhat_lm_log))
    max_data <- data.frame(G = c(4.34210),
                           qt = c(48123.74150),
                           fs = c(611.40524),
                           u = c(2898.91627),
                           yhat_lm_log = c(24.72895)
    )
    min_data <- data.frame(G = c(1.572000),
                           qt = c(30.372072),
                           fs = c(0.000000),
                           u = c(0.000000),
                           yhat_lm_log = c(9.976484)
    )

    scaled <- scale(database_model,center = min_data, scale = max_data - min_data)
    pr.all <- neuralnet::compute(model_nn,scaled)
    
    database$p.gamma <- pr.all$net.result*(29.89422-9.640000)+9.640000
    
    database_print <- select(database, c(soil.name,G,qt,fs,u,yhat_lm_log,p.gamma))
    database <<- select(database,c(soil.name,G,qt,fs,u))
    names(database_print) <- c("Soil name","G","qt","fs","u","Predicted Gamma (log model)","Predicted Gamma (NN model)")
    return(database_print)
  })
  
}
