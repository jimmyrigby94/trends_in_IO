##HELPER-------------------------------

library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(knitr)

master<-read_csv("Master.csv")

source("tidy_trend_plot.R")
source("cite_pred.R")
source("download_prep.R")
source("table_prep.R")










#App-----------------------------------------------------------------------------

ui <- fluidPage(titlePanel("Trends in Industrial-Organziational Psychology Journals"), 
                
                sidebarLayout(
                  sidebarPanel(sliderInput(inputId = "yearrange", 
                                           label = "Published After",
                                           min = 1930,
                                           max = 2018,
                                           value = 1930,
                                           sep = ""),
                              textInput(inputId = "oneword", 
                                         label = "Search (multiple words can be separated by \", \").", 
                                         value = "personality, trait, openness, conscientousness, extraversion, agreeableness, emotional stability"),
                               br(),
                               numericInput(inputId = "cutoff", 
                                            label = "Minimum Match Count", 
                                            min = 1, 
                                            max = 30, 
                                            step = 1, 
                                            value = 1),
                               br(),
                               radioButtons(inputId = "prop",
                                            label = "Plot Proportion of Articles Published",
                                            selected =  FALSE,
                                            choices = c("Yes" = TRUE,
                                                        "No" = FALSE)),
                               br(),
                               radioButtons(inputId = "journ",
                                            label = "Plot by Journal", 
                                            selected = FALSE,
                                            choices = c("Yes" = TRUE,
                                                        "No" = FALSE)),
                               br(),
                               downloadButton('trends.csv', 'Download'),
                               h5("To save data include \".csv\" in the file name.")
                               ),
                  mainPanel(
                    tabsetPanel(
                      type = "tabs", 
                      tabPanel("Plot", plotlyOutput("plot1"),conditionalPanel("input.prop =='TRUE'", h4("Proportions are based on SCOPUS database coverage. Weak coverage will result in inaccurate proportions. Earlier dates have noticibly weaker coverage."))),
                      tabPanel("Dataset", dataTableOutput("table")),
                      tabPanel("Citation Rate", dataTableOutput("citetest"), 
                               h4("Utilizes Poisson regression to explore the relationship between search terms and citation rates. Predicted values are reported in the columns titled \"Estimate Without\" and \"Estimate With\". The raw coefficient is presented in the \"Effect\" column. Separate regressions were conducted for each decade. While this is not a perfect method and cut points are arbitrarly defined it allows for estimates of nonlinear changes in slopes across time (which happen freqently when moratoriums are called). An article is identified as present so long as the number of matching terms is equal to or exceeds the user specified threshhold.")),
                      tabPanel("About", h3("Author: James Rigby M.A., University of Houston"),
                               br(),
                               h3("Overview"),
                               h4("This app plots the trends of word usage in abstacts ascross major IO and OB journals. 
                                  It is intended to assist researchers and practitioners in identifing emerging trends across 
                                  major journal outlets. The app searches a database of abstracts collected from SCOPUS and counts 
                                  the number of articles that match the terms entered by the user. This app is
                                  currently in Beta, meaning that there will still be qwerks to work out. If you encounter
                                  any errors or have suggestions to improve the app, please contact me at jrigby@uh.edu."), 
                               
                               h3("Using the App"),
                               h4("Input"),
                               h4("Text Input: Enter in a set of words or terms separated by \", \" that you would like to track over time."),
                               
                               h4("Minimum Match Count: No textmining procedure is perfect and Type I errors will occur. One way to reduce the number of false matches is to increase the number of times an article must have used the search terms
                                  to be included in the figures, data, and analysis. Changing the threshold paramter will reduce the number of articles that match your query."),
                               
                               h4("Plot by Journal: This option allows you to produce a plot that depicts trends in each journal separatley or plots the a general trend line for all journals."),
                               br(),
                               h4("Plot"),
                               h4("The results for your search term are ploted in an interactive chart. Hovering over a particular datapoint will
                                  show the number of articles published in the outlet for a particular year. Clicking on a journal outlet in the key
                                  will hide that journal from the plot. Click and drag your cursor on the plot to rescale the plot. You can always return
                                  to the original scale by clicking on \"reset axes\" in the tool bar."),
                               
                               h4("Datset"),
                               h4("This tab provides you with the references for the articles that exceed your threshold. The dataset is searchable and sortable. It also can be downloaded, although you need to specify the file type in file name (.csv)."),
                               h4("Database Coverage"),
                               dataTableOutput(outputId = "coverage")
                               
                               )
                      )
                    )
                  )
)


server <- function(input, output) {
  inp.unigram<-reactive({
    as.vector(str_split(input$oneword, pattern = ", ", simplify = TRUE))
  })
  
  datasetInput<-reactive({
    table_prep(data = master, 
               unigram = input$oneword, 
               threshhold = input$cutoff,
               since = input$yearrange)
  })
 
  downloadinput<-reactive({
    download_prep(data = master, 
               unigram = input$oneword, 
               threshhold = input$cutoff)
  })
  
  citedinput<- reactive({
    cite_pred(data = master,
             date =  Year,group =  `Source title`,
             unigram = input$oneword,
             threshhold = input$cutoff)
  })
  
  
  output$plot1<- renderPlotly({
    tidy_trend_plot(data = master,date =  Year,group =  `Source title`,
              unigram = input$oneword,
              threshhold = input$cutoff,
              prop = input$prop,
              byjourn = input$journ,
              since = input$yearrange)
  })
  
  output$table<- renderDataTable(datasetInput(), 
                                 options = list(pageLength=50
  ))
  
  output$coverage<-renderDataTable(master%>%
                                     count(Year, `Source title`)%>%
                                     arrange(Year), options = list(pageLength = 50))
  
  output$citetest<- renderDataTable(citedinput())
  
  output$trends.csv<- downloadHandler(
    filename =  function(){
      paste("trends", Sys.Date(), ".csv", sep = "")
    },
    content =  function(fname){
      write_csv(downloadinput(),
                fname)},
    contentType = "text/csv"
  )
}

shinyApp(ui = ui, server = server)

