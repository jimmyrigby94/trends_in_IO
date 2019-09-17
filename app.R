# Helper ----------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(knitr)
library(vroom)

master <- rbind(vroom("MasterFinal_060219_1.csv", col_names = T),
                vroom("MasterFinal_060219_2.csv", col_names = T),
                vroom("MasterFinal_060219_3.csv", col_names = T))

source("tidy_trend_plot.R")
source("cite_pred.R")
source("download_prep.R")
source("table_prep.R")
source("cite_plot.R")

# App -------------------------------------------------------------------------
ui <- fluidPage(
  

  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error: before { visibility: hidden; }",
    "h2 { text-align: center; }",
    "    .multicol {

      -webkit-column-count: 3; /* Chrome, Safari, Opera */

        -moz-column-count: 3; /* Firefox */

        column-count: 3;

    }"
  ),
  
  titlePanel(
    "Trends in Industrial-Organizational Psychology and Related Journals"
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "yearrange",
        label = "Publication Range",
        min = 1930,
        max = 2019,
        value = 1930,
        sep = ""
      ),
      textInput(
        inputId = "oneword",
        label = "Search Terms/Phrases (Use comma \", \" to separate)",
        value = "personality, general mental ability, training"
      ),
      numericInput(
        inputId = "cutoff",
        label = "Minimum Match Count",
        min = 1,
        max = 30,
        step = 1,
        value = 1
      ),
      radioButtons(
        inputId = "prop",
        label = "Plot Proportion of Articles Published",
        selected = FALSE,
        choices = c("Yes" = TRUE, "No" = FALSE)
      ),
      radioButtons(
        inputId = "journ",
        label = "Plot by Journal",
        selected = FALSE,
        choices = c("Yes" = TRUE, "No" = FALSE)
      ),
      downloadButton('my_trends', 'Download'),
      h5(
        "Note. Must leave \".csv\" in the filename in order to successfully download the data."
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        # overview tab
        tabPanel(
          "Overview",
          br(),
          h4(
            "The purpose of this application is to help quickly identify emerging
             trends among major I-O journals (and related literatures) by counting and
             plotting the frequency of word/phrase usage in the abstracts of scholarly
             publications from 85 peer-reviewed journals between 1950 and May, 2019."
          ),
          br(),
          h4(
            "Data were collected from SCOPUS, and word/phrase usage reflects the frequency
             with which abstracts include the user-specifed words/phrases."
          ),
          br(),
          h4(
            "Please note that this app is currently in Beta, and there are still a few bugs
             that need to be resolved.  If you encounter any errors or have suggestions, please
             feel free to contact the author below."
          ),
          br(),
          hr(),
          h4(
            span("Created by "),
            em(
              a("James Rigby, M.A., University of Houston",
                href = "mailto:jrigby@uh.edu"),
              span("and"),
              em(a("Zach Traylor, M.S., Texas A & M",
                   href = "mailto:zktraylor@gmail.com"))
            ),
            br(),
            span("Last updated: June 2019")
          )
        ),
        
        # instructions tab
        tabPanel(
          "Instructions",
          br(),
          h3("Search Terms/Phrases Text Input"),
          h4(
            "Individual terms and/or phrases may be used to search the database.  For searches
             using more than one word and/or phrase, a comma (i.e., \",\") must be placed
             between them."
          ),
          br(),
          h3("Minimum Match Count"),
          h4(
            "By increasing the frequency with which search terms/phrases must occur in each
             abstract in order to be included in the results of one's query, one can mitigate
             the extent of false positives (Type I errors) present in database generated in
             accordance with the specified search terms/phrases."
          ),
          br(),
          h3("Plotting the Search Terms/Phrases"),
          h4(
            "Plots can be generated using (1) all journals, or (2) each individual
             journal that is presently included in the database.  Hovering the cursor
             over any given datapoint reveals the number of articles published in that year
             (aggregated across journals or by individual journal).  Journals can also be
             hidden/removed from the plot by clicking on the name of the journal in the
             legend to the immediate right of the plot.  Axes can be rescaled by clicking
             and making a rectangular selection inside of the plot, and the \"reset axes\"
             button in the toolbar will revert the plot back to its original scale."
          ),
          br(),
          h3("Dataset"),
          h4(
            "Articles that meet the search criteria are searchable, sortable, and
             downloadable.  Please note that filenames *must* include a file extension
             to successfully download the datafile (e.g., my_data.csv, *not* my_data)."
          )
        ),
        
        # plot tab
        tabPanel(
          "Plot",
          br(),
          plotlyOutput("plot1"),
          conditionalPanel(
            "input.prop =='TRUE'",
            h3(
              "Proportions are based on SCOPUS database coverage.  Weak coverage will
                              result in inaccurate proportions, and earlier dates have noticeably weaker
                              coverage."
            )
          )
        ),
        
        # dataset  tab
        tabPanel("Dataset",
                 br(),
                 dataTableOutput("table")),
        
        # citation rates tab
        tabPanel(
          "Citation Rates",
          br(),
          plotlyOutput("plot2"),
          br(),
          dataTableOutput("citetest"),
          h4(
            "The relationship between user-specified words/phrases and citation rates is estimated
               using Poisson regression.  Separate regressions are estimated for each decade.
               Although this is not a perfect method (e.g., cut points are arbitrary by nature),
               doing so permits the estimation of nonlinear changes in slopes over time (which
               frequently occurs when moratoriums are called [e.g., Mischel (1968) and the
               subsequent lack of personality/individual differences research])."
          ),
          br(),
          h4(
            "Estimates are reported under the \"Estimate Without\" and \"Estimate With\" columns,
               and raw coefficients are reported underneath the \"Effect\" column.  Articles that meet
               or exceed the user's specified Minimum Match Count threshold are retained for the analyses."
          )
        ),
        # Data base Coverage tab
        tabPanel(
          "Database Coverage",
          h3(
            "The table below contains information about the number of articles stored in the application
                    database for each year. Users can employ this table as a diagnostic tool to verify the reliability
                    of the data presented."
          ),
          dataTableOutput(outputId = "coverage")
        ),
        # Advanced Options tab
        tabPanel(
          "Journal Selection",
          h3("Journals to Include in Analysis:"),
          tags$div(class = "multicol",
          checkboxGroupInput(
            "journal",
            label = NULL,
            c(
              "Academy of Management Executive" = "Academy of Management Executive",
              "Academy of Management Journal" = "Academy of Management Journal",
              "Academy of Management Perspectives" = "Academy of Management Perspectives",
              "Academy of Management Review" = "Academy of Management Review",
              "Administrative Science Quarterly" = "Administrative Science Quarterly",
              "American Psychologist" = "American Psychologist",
              "Annual Review of Psychology" = "Annual Review of Psychology",
              "Applied Psychological Measurement" = "Applied Psychological Measurement",
              "Applied Psychology" = "Applied Psychology",
              "Assessment" = "Assessment",
              "Basic and Applied Social Psychology" = "Basic and Applied Social Psychology",
              "Behavior Research Methods & Instrumentation" = "Behavior Research Methods & Instrumentation",
              "Behavior Research Methods" = "Behavior Research Methods",
              "Behavior Research Methods, Instruments, & Computers" = "Behavior Research Methods, Instruments, & Computers",
              "Computers in Human Behavior" = "Computers in Human Behavior",
              "Current Directions in Psychological Science" = "Current Directions in Psychological Science",
              "Educational and Psychological Measurement" = "Educational and Psychological Measurement",
              "European Journal of Psychological Assessment" = "European Journal of Psychological Assessment",
              "European Journal of Work and Organizational Psychology" = "European Journal of Work and Organizational Psychology",
              "Foundations and Trends in Human-Computer Interaction" = "Foundations and Trends in Human-Computer Interaction",
              "Group & Organization Management" = "Group & Organization Management",
              "Group Dynamics" = "Group Dynamics",
              "Handbook of Employee Selection" = "Handbook of Employee Selection",
              "Handbook of Employee Selection, Second Edition" = "Handbook of Employee Selection, Second Edition",
              "Harvard Business Review" = "Harvard Business Review",
              "Historical Perspectives in Industrial and Organizational Psychology" = "Historical Perspectives in Industrial and Organizational Psychology",
              "Human Factors" = "Human Factors",
              "Human Performance" = "Human Performance",
              "Human Relations" = "Human Relations",
              "Human Resource Management Journal" = "Human Resource Management Journal",
              "Human Resource Management Review" = "Human Resource Management Review",
              "Human Resource Management" = "Human Resource Management",
              "Human-Computer Interaction" = "Human-Computer Interaction",
              "Industrial and Organizational Psychology" = "Industrial and Organizational Psychology",
              "International Journal of Human Resource Management" = "International Journal of Human Resource Management",
              "International Journal of Human-Computer Interaction" = "International Journal of Human-Computer Interaction",
              "International Journal of Human-Computer Studies" = "International Journal of Human-Computer Studies",
              "International Journal of Selection and Assessment" = "International Journal of Selection and Assessment",
              "International Journal of Stress Management" = "International Journal of Stress Management",
              "International Journal of Training and Development" = "International Journal of Training and Development",
              "International Review of Industrial and Organizational Psychology" = "International Review of Industrial and Organizational Psychology",
              "Journal of Applied Psychology" = "Journal of Applied Psychology",
              "Journal of Applied Social Psychology" = "Journal of Applied Social Psychology",
              "Journal of Behavioral Decision Making" = "Journal of Behavioral Decision Making",
              "Journal of Business and Psychology" = "Journal of Business and Psychology",
              "Journal of Educational Measurement" = "Journal of Educational Measurement",
              "Journal of Experimental Psychology: General" = "Journal of Experimental Psychology: General",
              "Journal of Individual Differences" = "Journal of Individual Differences",
              "Journal of Leadership and Organizational Studies" = "Journal of Leadership and Organizational Studies",
              "Journal of Management Studies" = "Journal of Management Studies",
              "Journal of Management" = "Journal of Management",
              "Journal of Managerial Psychology" = "Journal of Managerial Psychology",
              "Journal of Occupational and Organizational Psychology" = "Journal of Occupational and Organizational Psychology",
              "Journal of Occupational Health Psychology" = "Journal of Occupational Health Psychology",
              "Journal of Organizational Behavior Management" = "Journal of Organizational Behavior Management",
              "Journal of Organizational Behavior" = "Journal of Organizational Behavior",
              "Journal of Personality and Social Psychology" = "Journal of Personality and Social Psychology",
              "Journal of Personality" = "Journal of Personality",
              "Journal of Research in Personality" = "Journal of Research in Personality",
              "Journal of Vocational Behavior" = "Journal of Vocational Behavior",
              "Judgment and Decision Making" = "Judgment and Decision Making",
              "Leadership Quarterly" = "Leadership Quarterly",
              "Military Psychology" = "Military Psychology",
              "Organization Science" = "Organization Science",
              "Organizational Behavior and Human Decision Processes" = "Organizational Behavior and Human Decision Processes",
              "Organizational Dynamics" = "Organizational Dynamics",
              "Organizational Psychology Review" = "Organizational Psychology Review",
              "Organizational Research Methods" = "Organizational Research Methods",
              "Personality and Individual Differences" = "Personality and Individual Differences",
              "Personnel Psychology" = "Personnel Psychology",
              "Personnel Review" = "Personnel Review",
              "Perspectives on Psychological Science" = "Perspectives on Psychological Science",
              "Psychological Bulletin" = "Psychological Bulletin",
              "Psychological Methods" = "Psychological Methods",
              "Psychological Review" = "Psychological Review",
              "Psychological Science" = "Psychological Science",
              "Psychometrika" = "Psychometrika",
              "Public Personnel Management" = "Public Personnel Management",
              "Research in Organizational Behavior" = "Research in Organizational Behavior",
              "Research in Personnel and Human Resources Management" = "Research in Personnel and Human Resources Management",
              "Small Group Research" = "Small Group Research",
              "Strategic Management Journal" = "Strategic Management Journal",
              "The International Journal of Human Resource Management" = "The International Journal of Human Resource Management",
              "The Personnel Administrator" = "The Personnel Administrator",
              "Work and Stress" = "Work and Stress"
            ), 
            selected = c("Journal of Applied Psychology",
                         "Personnel Psychology",
                         "Academy of Management Journal",
                         "Journal of Management",
                         "Journal of Occupational and Organizational Psychology",
                         "International Journal of Selection and Assessment",
                         "Organizational Behavior and Human Decision Processes",
                         "Journal of Vocational Behavior",
                         "Academy of Management Review",
                         "Psychological Bulletin",
                         "Human Performance",
                         "American Psychologist",
                         "Journal of Business and Psychology",
                         "Leadership Quarterly",
                         "Journal of Applied Social Psychology",
                         "Journal of Occupational Health Psychology",
                         "Applied Psychology"
                         )
            )

          )
        )
      )
    )
  )
)

server <- function(input, output) {
  inp.unigram <- reactive({
    as.vector(str_split(input$oneword, pattern = ", ", simplify = TRUE))
  })
  
  datasetInput <-
    reactive({
      table_prep(
        data = master[master$`Source title` %in% input$journal,],
        unigram = input$oneword,
        threshhold = input$cutoff,
        since = input$yearrange
      )
    })
  
  downloadinput <-
    reactive({
      download_prep(
        data = master[master$`Source title` %in% input$journal,],
        unigram = input$oneword,
        threshhold = input$cutoff,
        since = input$yearrange
      )
    })
  
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
        since = input$yearrange
      )
    })
  
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
  
  output$table <-
    renderDataTable(datasetInput(), options = list(pageLength = 50))
  
  output$coverage <-
    renderDataTable(master[master$`Source title` %in% input$journal,] %>% count(Year, `Source title`) %>% arrange(Year),
                    options = list(pageLength = 50))
  
  output$citetest <- renderDataTable(citedinput())
  
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

shinyApp(ui = ui, server = server)