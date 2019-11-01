server <- function(input, output, session) {
  
  # parses the user-specified query
  inp.unigram <- reactive({
    as.vector(str_split(input$oneword, pattern = ", ", simplify = TRUE))
  })
  
  # queries database with user-specified query
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
  
  # preps user-specified search results for download
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
  
  # estimates citation rate models
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
  
  # plots publication trends
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
  
  # plots citation rates
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
  
  # outputs tabular data
  output$table <-
    DT::renderDT(datasetInput(), options = list(pageLength = 50))
  
  output$coverage <-
    DT::renderDT(master[master$`Source title` %in% input$journal,] %>% count(Year, `Source title`) %>% arrange(Year),
                    options = list(pageLength = 50))
  
  output$citetest <- DT::renderDT(citedinput())
  
  # handler for select all and deselect all
  # one observe context for deselect
  i <- 0
  observe( {
    if (input$deselectall > i) {
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = c(NULL))
      
      i <- i+1
    }
  })
  
  # one observe context for select
  j <- 0
  observe( {
    if(input$selectall > j) {
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = names(j_names))
      j <- j + 1
    }
  })
  
  # outputs downloadable .csv file
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