server <- function(input, output, session) {

# Including the Modal 
modal_output<-modalDialog(
  tags$h2("Welcome to the Trends in IO Psychology Application"),
  tags$p("As scientists search for new insights into the world of work, topics inevitably fall in and out of vogue. Perhaps the most volatile and well recognized swings in research interests have been observed in personality research. Walter Mischel’s 1968 book Personality and Assessment came to the gloomy conclusion that “[w]ith the possible exception of intelligence, highly generalized behavioral consistencies have not been demonstrated, and the concept of personality traits as broad response predispositions is thus untenable.” After this book was published, personality research for selection and assessment slowed considerably until about a decade later. Identifying these trends (and slumps) can help researchers and practitioners see where the field is headed, where it has been, and identify ideas and topics that should be brought to light once again."),
  tags$p("This app was designed to help researchers and practitioners identify current trends in industrial-organizational psychology. It uses user-defined queries to search article abstracts in I-O psychology’s top scientific journals. Search results are plotted over time, and users have the option to focus on specific (or all) journals, and download a .csv file of their generated data."),
                          easyClose = TRUE,
                          size = "l",
                          fade = TRUE,
                          footer = tagList(
                            tags$a(tags$i(class = "fab fa-github"), "Github", class="btn3", style = "margin-right: 5px;", href = "https://github.com/jimmyrigby94/trends_in_IO", target="_blank"),
                            actionButton("tutorial",
                                         "Begin the Tutorial",
                                         icon = icon("graduation-cap"),
                                         class = "btn3"),
                            actionButton("close",
                                         "Close Window",
                                         icon = icon("times"),
                                         class = "btn3")
                          ))

showModal(modal_output)

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
  
  observeEvent(input$close,
               {
                 removeModal()
               })
  
  observeEvent(input$tutorial,{
    introjs(session, options = list("nextLabel" = "Continue",
                                    "prevLabel" = "Previous",
                                    "doneLabel" = "Alright. Let's go"))
    removeModal()
  }
  )
  
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