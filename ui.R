# header
header <- dashboardHeader(title = "Trends in I-O Psychology", 
                          titleWidth  = "350px", 
                          
                          # Places Search and Plot options as Dropdowns in Header
                          dropdownMenu(headerText = "Refine Search",
                                       icon = "Refine Search",
                                       menuItem(
                                       sliderInput(
                                         inputId = "yearrange",
                                         label = "Published After",
                                         min = 1930,
                                         max = 2019,
                                         value = c(1930, 2019),
                                         sep = ""
                                       )),
                                       menuItem(  tippy::tippy(
                                         "&#9432;",
                                         "<div class = \"largerfont\"> Use slider to limit search to a specific year range. </div>"
                                       )),
                                       menuItem(
                                         numericInput(
                                           inputId = "cutoff",
                                           label = "Minimum Match Count",
                                           min = 1,
                                           max = 30,
                                           step = 1,
                                           value = 1
                                         )),
                                       # hover text
                                       menuItem(tippy::tippy(
                                         "&#9432;",
                                         "<div class = \"largerfont\"> By increasing the frequency with which queries must occur in a given abstract 
                                                for inclusion in one's search results, one can mitigate the number of false 
                                                positives (Type I errors). </div>"
                                       ))),
                          dropdownMenu(headerText = "Plot Options",
                                         icon =  "Plot Options",
                                       menuItem(
                                       # plot by proportion input
                                       radioButtons(
                                         inputId = "prop",
                                         label = "Plot Proportion of Published Articles",
                                         selected = FALSE,
                                         choices = c("Yes" = TRUE, "No" = FALSE)
                                       )),
                                       # hover text
                                       menuItem(
                                       tippy::tippy(
                                         "&#9432;",
                                         "<div class = \"largerfont\"> Select \"Yes\" to plot the proportion of published articles on the Y axis (instead of raw frequencies).  
                                  Note that proportions are based on SCOPUS database coverage.  Weak coverage will invariably result in 
                                  inaccurate proportion estimates, and earlier dates have notably weaker coverage. </div>"
                                       )),
                                       
                                       # plot by journal input
                                      menuItem(radioButtons(
                                         inputId = "journ",
                                         label = "Plot by Journal",
                                         selected = FALSE,
                                         choices = c("Yes" = TRUE, "No" = FALSE)
                                       )),
                                       # hover text
                                      menuItem(
                                       tippy::tippy(
                                         "&#9432;<br/>",
                                         "<div class = \"largerfont\"> Select \"Yes\" to plot search results for each journal separately. </div>"
                                       ))
                          )
                                        )

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    h4("Search"),
    
    # user-defined query input
    textInput(
      inputId = "oneword",
      label = NULL,
      value = "personality, general mental ability, training"
    ),
    # hover text
    tippy::tippy(
      "&#9432;",
      "<div class = \"largerfont\"> Individual terms and/or phrases may be used to search the database.  
                                  For searches with more than one word and/or phrase, a comma *MUST* 
                                  be placed between them (i.e., search phrase 1, search prase 2).  
                                  Users may also use regular expressions within their queries. </div>"
    ),
    
    h4("Navigation"),
    menuItem(
      "Dashboard",
      tabName = "plts_and_analytics",
      icon = icon("th"),
      selected = TRUE
    ),
    menuItem(
      "Journal Selection",
      tabName = "journal_selection",
      icon = icon("check-square"),
      selected = FALSE
    ),
    menuItem(
      "Tables",
      tabName = "tables",
      icon = icon("table"),
      selected = FALSE,
      menuSubItem(      "Search Results",
                        tabName = "searchresults",
                        icon = icon("table"),
                        selected = FALSE),
      menuItem(
        "Database Coverage",
        tabName = "database_coverage",
        icon = icon("signal"),
        selected = FALSE
      )
    ),

    menuItem(
      "About",
      tabName = "about",
      icon = icon("question"),
      selected = FALSE
    )
  ),

  # download button
  downloadButton('my_trends', 'Download'),

  # hover text
  tippy::tippy(
    "<br/>&#9432;",
    "<div class = \"largerfont\"> Filename *MUST* include \".csv\" so that the file can be opened 
                                  by your machine, and its associated spreadsheet software. </div>"
  ),
  width = "350px"
)

# Defining CSS script
body <- dashboardBody(
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error: before { visibility: hidden; }",
    "h2 { text-align: left; }",
    "    .multicol {

      -webkit-column-count: 3; /* Chrome, Safari, Opera */

        -moz-column-count: 3; /* Firefox */

        column-count: 3;

    }",
    ".form-group {
    margin-bottom: 1px;
}",
    ".largerfont {
    font-size: 130%;
    }",
    "p {
    text-indent:12px; 
    font-size: 18px;
    }",
    ".label-primary { visibility: hidden;}",
    ".menu {max-height: none;}",
    ".tippy-popper {max-width: 250px;}",
    ".textlist {text-indent: -1.3em;
                margin-left: 40px;}"),
  
  tags$head(tags$style(HTML('
  .navbar-nav>.messages-menu>.dropdown-menu>li .menu {
  max-height:none;
  }
  '))),
  
  
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