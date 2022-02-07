server <- function(input, output, session) {

# Including the Modal 
modal_output<-modalDialog(includeHTML("modal-window.html"),
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
    
    if(!is.null(input$author)){
      author_index<-str_detect(master$Authors, input$author)
    }else{
      author_index<-rep(TRUE, times = nrow(master))
    }
    
    journal_index<-master$`Source title` %in% input$journal
    
    search_abstract(data = master[author_index & journal_index,],
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
  
  update_plot1<-eventReactive(
    input$plot,
    tidy_trend_plot(
      data = search_data(),
      prop = input$prop,
      byjourn = input$journ,
      date = input$yearrange,
      journals = input$journal
    ),
    ignoreNULL = FALSE
  )
  

# Plots -------------------------------------------------------------------

  # plots publication trends
  output$plot1 <- renderPlotly({
    update_plot1()
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
  
  output$citetest <- DT::renderDT(citedinput(), 
                                  rownames = FALSE,
                                  options = list(
                                    columnDefs = list(
                                      list(className = 'dt-center', targets = 1:6),
                                      list(className = 'dt-left', targets = 0)
                                      ),
                                    dom = 't'
                                  ))


# Event Handlers ----------------------------------------------------------
  # Handles button to remove initial pop-up window
  observeEvent(input$tutorial|input$close,
               {
                 validate(need(input$tutorial > 0|input$close > 0, ''))
                 removeModal()
               }
               )
  
  observeEvent(input$`relaunch-modal`,
               {
                 validate(need(input$`relaunch-modal` > 0, ''))
                 showModal(modal_output)
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