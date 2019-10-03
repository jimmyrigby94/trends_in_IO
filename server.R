# Helper ----------------------------------------------------------------------
library(shiny)
library(shinyBS)
library(tidyverse)
library(tidytext)
library(plotly)
library(knitr)
library(vroom)

## Importing Data
master <- rbind(vroom("MasterFinal_060219_1.csv", col_names = T),
                vroom("MasterFinal_060219_2.csv", col_names = T),
                vroom("MasterFinal_060219_3.csv", col_names = T))

# Importing Dependencies
source("tidy_trend_plot.R")
source("cite_pred.R")
source("download_prep.R")
source("table_prep.R")
source("cite_plot.R")

server <- function(input, output, session) {
  
  
  # Parsing the user query --------------------------------------------------
  inp.unigram <- reactive({
    as.vector(str_split(input$oneword, pattern = ", ", simplify = TRUE))
  })
  
  
  # Querying data base with search term -------------------------------------------------
  datasetInput <-
    reactive({
      table_prep(
        data = master[master$`Source title` %in% input$journal,],
        unigram = input$oneword,
        threshhold = input$cutoff,
        upper_year = input$yearrange[2],
        lower_year = input$yearrange[1]
      )
    })
  
  
  # Preping the results for download ----------------------------------------
  downloadinput <-
    reactive({
      download_prep(
        data = master[master$`Source title` %in% input$journal,],
        unigram = input$oneword,
        threshhold = input$cutoff,
        upper_year = input$yearrange[2],
        lower_year = input$yearrange[1]
      )
    })
  
  
  # Runing Citation Rate Models ---------------------------------------------
  citedinput <-
    reactive({
      cite_pred(
        data = master[master$`Source title` %in% input$journal,],
        date = Year,
        group = `Source title`,
        unigram = input$oneword,
        threshhold = input$cutoff
      )
    })
  
  
  # Plotting publication trends ---------------------------------------------
  output$plot1 <-
    renderPlotly({
      tidy_trend_plot(
        data = master[master$`Source title` %in% input$journal,],
        date = Year,
        group = `Source title`,
        unigram = input$oneword,
        threshhold = input$cutoff,
        prop = input$prop,
        byjourn = input$journ,
        upper_year = input$yearrange[2],
        lower_year = input$yearrange[1]
      )
    })
  
  
  # Ploting citation Rates --------------------------------------------------
  output$plot2 <-
    renderPlotly({
      cite_plot(
        data = master[master$`Source title` %in% input$journal,],
        date = Year,
        group = `Source title`,
        unigram = input$oneword,
        threshhold = input$cutoff
      )
    })
  
  
  # Outputing Tabular data --------------------------------------------------
  output$table <-
    renderDataTable(datasetInput(), options = list(pageLength = 50))
  
  output$coverage <-
    renderDataTable(master[master$`Source title` %in% input$journal,] %>% count(Year, `Source title`) %>% arrange(Year),
                    options = list(pageLength = 50))
  
  output$citetest <- renderDataTable(citedinput())
  
  
  # Handler for select all and deselect all ---------------------------------
  # One observe context for deselect
  i<-0
  observe({
    if (input$deselectall > i) {
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = c(NULL))
      
      i<-i+1
    }
  })
  
  # One observe context for select
  j<-0
  observe({
    if(input$selectall > j){
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = names(j_names))
      j<-j+1
    }
  })
  
  
  # Outputting downloadable csv ---------------------------------------------
  output$my_trends <-
    downloadHandler(
      filename = function() {
        paste("trends", Sys.Date(), ".csv", sep = "")
      },
      content = function(fname) {
        write.csv(downloadinput(), fname, row.names = FALSE)
      }
    )
}