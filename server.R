server <- function(input, output, session) {

# Including the Modal 
modal_output<-modalDialog(
  tags$h2("Welcome to the Trends in IO Psychology Application"),
  tags$hr(),
  tags$p(" As scientists search for new insights into the world of work, topics fall in and out of vogue. Perhaps, the most well-recognized swings in research interests occurred in personality research. In 1968, Walter Mischel concluded that “[w]ith the possible exception of intelligence, highly generalized behavioral consistencies have not been demonstrated, and the concept of personality traits as broad response predispositions is thus untenable.” Personality research slowed after Mischel's critique. Only a few decades later, however, personality research was back in style. Studies exploring personality and its outcomes become commonplace at the turn of the century. These investigations fueled meta-analyses that contended Mischel's claim. Identifying these trends (and slumps) can help researchers and practitioners see where the field has been, where it's going, and where it can grow."),
  tags$p("This app was designed to help researchers and practitioners identify current trends in industrial-organizational psychology. It uses user-defined queries to search article abstracts in I-O psychology’s top scientific journals. Search results are plotted over time, and users have the option to focus on specific (or all) journals, and download a .csv file of their results."),
  tags$p("To see the code, press the Github button below. To take a short tutorial on how to use the app, press the \"Begin Tutorial\" button. Close this window to skip the tutorial and start using the app."),
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
    introjs(session, options = list("nextLabel" = "Next",
                                    "prevLabel" = "Previous",
                                    "doneLabel" = "I'm Done",
                                    showStepNumbers = "false"))
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