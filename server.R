server <- function(input, output, session) {
  

# Preporatory Reactive Environments ---------------------------------------
  
  # parses the user-specified query
  prepped_unigram <- reactive({
    prep_unigram(input$oneword)
  })
  
  

  search_data<-  eventReactive(input$plot, {
    search_abstract(data = master[master$`Source title` %in% input$journal,],
                    unigram = prepped_unigram(),
                    threshhold = input$cutoff,
                    date = input$yearrange)
  }, 
  ignoreNULL = FALSE
  )
  
  table_prep<-reactive({
    search_data()%>%
      filter(Present == "Present")%>%
      arrange(desc(`Matching Terms`))%>%
      select(Authors:DOI, `Matching Terms`)
  })
  
  
  # estimates citation rate models
  citedinput <-
    reactive({
      cite_pred(
        data = search_data()
      )
    })
  

# Plots -------------------------------------------------------------------

  # plots publication trends
  output$plot1 <-
    renderPlotly({
      tidy_trend_plot(
        data = search_data(),
        prop = input$prop,
        byjourn = input$journ,
        date = input$yearrange,
        journals = input$journal
      )
    })
  
  # plots citation rates
  output$plot2 <-
    renderPlotly({
      cite_plot(
        data = search_data()
      )
    })
  

# Tables ------------------------------------------------------------------

  # outputs tabular data
  output$table <-
    DT::renderDT(table_prep(), options = list(pageLength = 50))
  
  output$coverage <-
    DT::renderDT(search_data() %>% 
                   count(Year, `Source title`) %>% 
                   arrange(Year),
                    options = list(pageLength = 50))
  
  output$citetest <- DT::renderDT(citedinput())
  

# Event Handlers ----------------------------------------------------------
  
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
        write.csv(table_prep(), fname, row.names = FALSE)
      }
    )
}