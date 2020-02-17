# header --------------------------------------------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = "Trends in I-O Psychology", 
                          titleWidth  = "350px"
                                        )

# sidebar ----------------------------------------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  introBox(
  sidebarMenu(
 tags$div(
   tags$p("Search Options", style = "font-family = 'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 1.5em; font-weight: 700;"),
    # user-defined query input
   introBox(
    textInput(
      inputId = "oneword",
      label = NULL,
      value = "personality, general mental ability"
    ),
   data.step=2,
   data.intro = "Enter your search terms or phrases here. <br><br> Separate multiple terms with a comma. For example, if you wanted to search for abstracts that contain personality <em>or</em> general mental ability type: \"personality, general mental ability\".<br> <br>Power users can also use regular expressions. For example, if you were interested in searching for networks or networking search \"network[[s]|[ing]]\""),
   tags$br(),
  introBox(
   menuItem(
     "Journal Selection",
     tabName = "journal_selection",
     icon = icon("check-square"),
     selected = FALSE
   ),
   data.step = 3,
   data.intro = "Click here to select which journals to include in your search. <br><br> Clicking this button will open up a separate window with a list of journal titles. Journals with a check next to them will be included in your search."
   ),
  tags$br(),
  introBox(
   numericInput(
     inputId = "cutoff",
     label = "Minimum Match Count",
     min = 1,
     max = 30,
     step = 1,
     value = 1
   ),
   data.step = 4,
   data.intro = "Sometimes an article may match your search by chance. You can increase the number of matching terms required to be included in your search results. This can reduce the number of false pasitives (Type I errors)"),
   tags$br(),
  introBox(
   sliderInput(
     inputId = "yearrange",
     label = "Published After",
     min = 1930,
     max = 2019,
     value = c(1930, 2019),
     sep = ""
   ),
   data.step = 5,
   data.intro = "Use the slider to restrict your search to a certain date range."),
  tags$br(),
   introBox(
   actionButton("plot", 
                "Search",
                icon = icon("search"),
                style = "color: black; margin-left: 15px; margin-bottom: 5px;"
   ),
   data.step = 6,
   data.intro = "Click here to refresh the results after changing your search parameters."),
  
   tags$br(),
   tags$hr(style = "color:white; width:90%; padding-left: 0;"),
   tags$p("Plot Options", style = "font-family = 'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 1.5em; font-weight: 700;"),
  
  introBox(
   radioButtons(
     inputId = "prop",
     label = "Plot Proportion of Published Articles",
     selected = TRUE,
     choices = c("Yes" = TRUE, "No" = FALSE)
   ),
   
   data.step = 7,
   data.intro = "Select \"yes\" if you are interested in plotting the proportion of articles matching your search on the y-axis. Select \"no\" if you want to plot the raw frequencies. <br><br> Note that proportions are based on SCOPUS database coverage.  Weak coverage will invariably result in 
                                  inaccurate proportion estimates, and earlier dates have notably weaker coverage."),
   
  introBox(
   radioButtons(
     inputId = "journ",
     label = "Plot by Journal",
     selected = FALSE,
     choices = c("Yes" = TRUE, "No" = FALSE)
   ),
   data.step = 8,
   data.intro = "If you want to plot journals separately select \"Yes\" otherwise select \"No\"."),


  menuItem(
      "",
      tabName = "plts_and_analytics",
      selected = TRUE
    ),
    style = "font-size: 1.5em; padding-left: 2.5%;")
  ),
 
  # download button
 introBox(
  downloadButton('my_trends', 
                 'Download .CSV', 
                 icon = icon("download"),
                 style = "color: black; margin-left: 15px; margin-bottom: 5px;"),
  data.step = 9,
  data.intro = "Click here to download your search results as a CSV. Make sure that you include .csv in your file name!")
,
 data.step = 1,
 data.intro = "The sidebar contains the controls for changing what terms are searched for, where it's searched for, and how its visualized.<br><br> Let's see what we can do!"
),
width = "350px"
)

# Body ------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
  shinyjs::useShinyjs(),
  introjsUI(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "trends_in_IO_style.css")
  ),
  tags$div(

    tags$div(
    tags$a(tags$i(class = "fa fa-chart-line"), "Dashboard", class = "btn2", href = "#shiny-tab-plts_and_analytics", `data-value` = "plts_and_analytics", `data-toggle`="tab"),
    
    tags$a(tags$i(class = "fa fa-table"), "Search Results", class = "btn2", href = "#shiny-tab-searchresults", `data-value` = "searchresults", `data-toggle`="tab"),
    
    tags$a(tags$i(class = "fa fa-signal"), "Database Coverage", class = "btn2", href = "#shiny-tab-database_coverage", `data-value` = "database_coverage", `data-toggle`="tab"),
    
    tags$a(tags$i(class = "fa fa-question"), "About", class = "btn2", href = "#shiny-tab-about", `data-value` = "about", `data-toggle`="tab"),
    class = "centerdiv"),
  style = "margin-bottom: 15px; width: 100%;"),
  tabItems(
    tabItem(tabName = "plts_and_analytics",
           fluidRow(
              box(title = "Publication Trends for User-Specified Query",
                  width = 12,
                  plotlyOutput("plot1"))
            ),
            fluidRow(
              box(title = "Citation Trends for User-Specified Query",
                  width = 6,
                  plotlyOutput("plot2")),
              box(
                title = "Do People Cite the User-Specified Query More Than Other Articles?",
                width = 6,
                DT::DTOutput("citetest")
              )
            )),
    tabItem(
      tabName =  "searchresults",
      box(
        title = "Search Results",
        width = "100%",
        height = "100%",
        DT::DTOutput("table")
      )
    ),
    tabItem(
      tabName =  "database_coverage",
      box(
        title = "Database Coverage",
        width = "100%",
        height = "100%",
        DT::DTOutput(outputId = "coverage")
      )
    ),
    tabItem(tabName = "journal_selection",
            box(
              title = "Journal Selection",
              width = 12,
              tags$div(
                class = "multicol",
                checkboxGroupInput(
                  "journal",
                  label = NULL,
                  j_names,
                  selected = c(
                    "Journal of Applied Psychology",
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
                ),
                br(),
                actionButton("selectall", label = "Select All"),
                actionButton("deselectall", label = "Deselect All")
              )
            )
    ),
    tabItem(tabName = "about",
            box(
                h1("Trends in Industrial-Organizational Psychology"),

                h3(span("By"),
                   em(a("James Rigby, M.A., University of Houston", href = "mailto:jrigby@uh.edu")),
                   span("and"),
                   em(a("Zach Traylor, M.S., Texas A&M University", href = "mailto:zktraylor@gmail.com"))),

                p("The purpose of this application is to help quickly identify emerging
                  trends among major I-O journals (and related literatures) by counting and
                  plotting the frequency of term/phrase usage contained in the abstracts of scholarly
                  publications from 85 peer-reviewed journals between 1950 and May, 2019.  
                  Users impute search term(s) and/or phrase(s), and the app then uses the query to identify relevant articles.  
                  The raw search results are located in the \"Tables\" Menu under the \"Search Results\" tab.  
                  Additional analytics are reported under the \"Dashboard\" tab, which includes publication frequency visualizations, 
                  citation rate visualizations, and citation rate analytics."),
                
                
                p("Data were collected from SCOPUS (www.scopus.com).  The only requirement for inclusion in the operational database was that 
                  articles must have been published in one of the journals listed under the \"Journal Selection\" tab.  
                  After data were downloaded from SCOPUS, article metadata was verified and standardized within journal 
                  because journal names and formatting have changed over the last 90 years.  
                  In total, the operational database contains over 155,000 articles from 85 academic journals."),
                
                p("This application relies on four core functions to query the database, and each of which are heavily dependent on the tidyverse."),

                p(class = "textlist", "(1) table_prep() returns the raw search results."),
                p(class = "textlist","(2) tidy_trend_plot() plots the publication trends for articles matching the user-specified query."),
                p(class = "textlist","(3) cite_plot() returns a plot comparing the citation rates of articles matching the user-specified query."),
                p(class = "textlist","(4) cite_pred() returns the resultant statistical estimates of several poisson regression models predicting citation rates using a 
                  binary indicator variable that stipulates whether the article matches the user-specified query.  The models are
                  estimated separately by decade in order to better explore whether and the extent to which the user-specified query became more or less popular over time."),

                p("Please note that this app is currently in Beta, and there are still a few bugs
                  that need to be resolved.  If you encounter any errors or have suggestions, please
                  feel free to contact either author using the above emails."),
                
                width = 12)
            )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")