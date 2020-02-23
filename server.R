server <- function(input, output, session) {

# Including the Modal 
modal_output<-modalDialog(
  tags$h2("Welcome to the Trends in IO Psychology Application"),
  tags$hr(),
  tags$p("As scientists search for new insights into the world of work, topics fall in and out of vogue. Perhaps, the most well-recognized swings in research interests occurred in personality research. In 1968, Walter Mischel concluded that “[w]ith the possible exception of intelligence, highly generalized behavioral consistencies have not been demonstrated, and the concept of personality traits as broad response predispositions is thus untenable.” Personality research slowed after Mischel's critique. Only a few decades later, however, personality research was back in style. Studies exploring personality and its outcomes become commonplace at the turn of the century. These investigations fueled meta-analyses that contended Mischel's claim. Identifying these trends (and slumps) can help researchers and practitioners see where the field has been, where it's going, and where it can grow."),
  tags$p("This app was designed to help researchers and practitioners identify current trends in industrial-organizational psychology. It uses user-defined queries to search article abstracts in I-O psychology’s top scientific journals. Search results are plotted over time, and users have the option to focus on specific (or all) journals, and download a .csv file of their results."),
  tags$p("To see the code, press the Github button below. To take a short tutorial on how to use the app, press the \"Begin Tutorial\" button. Close this window to skip the tutorial and start using the app."),
                          easyClose = TRUE,
                          size = "l",
                          fade = TRUE,
                          footer = tagList(
                            tags$a(tags$i(class = "fab fa-github"), "Github", class="btn3", style = "margin-right: 1%;", href = "https://github.com/jimmyrigby94/trends_in_IO", target="_blank"),
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
    
    # Handle new loading
    test_journals<-!input$journal %in% init_selected
    
    if(any(test_journals)){
      new_journals<-input$journal[test_journals]
      
      master<-bind_rows(master, map_dfr(new_journals, ~read_rds(paste0("data/", ., ".rds"))))
      
      init_selected<- c(init_selected, new_journals)
    }
    
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
    renderPlotly(search_data() %>% 
                   count(Year, `Source title`)%>%
                   ggplot(aes(x = Year, y = n, color = `Source title`))+
                   geom_line()+
                   labs(y = "Article Count")+
                   theme(panel.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line())+
                   guides(color = guide_legend(title = "")))
  
  output$citetest <- DT::renderDT(citedinput())


# Event Handlers ----------------------------------------------------------
  # Handles button to remove initial pop-up window
  observeEvent(input$tutorial|input$close,
               {
                 validate(need(input$tutorial > 0|input$close > 0, ''))
                 removeModal()
               }
               )


# launches js.intro
observeEvent(c(input$tutorial, input$launchhelp, input$launchhelp2),{
  validate(need(input$tutorial>0| input$launchhelp>0|input$launchhelp2>0, ''))
  introjs(session, options = list("nextLabel" = "Next",
                                  "prevLabel" = "Previous",
                                  "doneLabel" = "I'm Done",
                                  showStepNumbers = "false"),
          events = list("onchange" = I(
             "if (this._currentStep==0) {
        $('a[data-value!=\"plts_and_analytics\"]').removeClass('active');
        $('a[data-value=\"plts_and_analytics\"]').addClass('active');
        $('a[data-value=\"plts_and_analytics\"]').trigger('click');
  }")
          ))

})
  
  # handler for select all and deselect all
  observeEvent(input$deselectall, {
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = c(NULL))
    })
  
  observeEvent(input$selectall, {
      updateCheckboxGroupInput(session=session, 
                               inputId="journal",
                               choices = j_names,
                               selected = names(j_names))
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