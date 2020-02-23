# header --------------------------------------------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = "Trends in I-O Psychology",
                          titleWidth  = "350px",
                          tags$li(
                            introBox(
                                  introBox(
                                    tags$div(
                                    tags$a(
                                      tags$i(class = "fa fa-chart-line"),
                                      "Dashboard",
                                      class = "btn2",
                                      onclick = "openTab('plts_and_analytics')",
                                      href = "#shiny-tab-plts_and_analytics",
                                      `data-value` = "plts_and_analytics",
                                      `data-toggle` = "tab"
                                    ),
                                    class = "desktop_nav"
                                    ),
                                    tags$div(
                                      tags$a(
                                        tags$i(class = "fa fa-chart-line"),
                                        class = "btn2",
                                        onclick = "openTab('plts_and_analytics')",
                                        href = "#shiny-tab-plts_and_analytics",
                                        `data-value` = "plts_and_analytics",
                                        `data-toggle` = "tab"
                                      ),
                                      class = "mobile_nav"
                                    ),
                                    data.step = 11,
                                    data.intro = "Click here to navigate to the main dashboard."
                                  ),
                                  introBox(
                                    tags$div(
                                    tags$a(
                                      tags$i(class = "fa fa-table"),
                                      "Results",
                                      class = "btn2",
                                      onclick = "openTab('searchresults')",
                                      href = "#shiny-tab-searchresults",
                                      `data-value` = "searchresults",
                                      `data-toggle` = "tab"
                                    ),
                                    class = "desktop_nav"),
                                    tags$div(
                                      tags$a(
                                        tags$i(class = "fa fa-table"),
                                        class = "btn2",
                                        onclick = "openTab('searchresults')",
                                        href = "#shiny-tab-searchresults",
                                        `data-value` = "searchresults",
                                        `data-toggle` = "tab"
                                      ),
                                      class = "mobile_nav"),
                                    data.step = 12,
                                    data.intro = "Click here to see a table of the search results. The table includes the article title, meta-information such as the doi, and the number of terms in the article that match the user's search."
                                  ),

                                  introBox(
                                    tags$div(
                                    tags$a(
                                      tags$i(class = "fa fa-signal"),
                                      "Database",
                                      class = "btn2",
                                      onclick = "openTab('database_coverage')",
                                      href = "#shiny-tab-database_coverage",
                                      `data-value` = "database_coverage",
                                      `data-toggle` = "tab"
                                    ),
                                    class = "desktop_nav"),
                                    tags$div(
                                      tags$a(
                                        tags$i(class = "fa fa-signal"),
                                        class = "btn2",
                                        onclick = "openTab('database_coverage')",
                                        href = "#shiny-tab-database_coverage",
                                        `data-value` = "database_coverage",
                                        `data-toggle` = "tab"
                                      ),
                                      class = "mobile_nav"),
                                    data.step = 13,
                                    data.intro = "Click here to explore the number of articles for each journal in our data base."
                                  ),
                                  introBox(
                                    tags$div(
                                    tags$a(
                                      tags$i(class = "fas fa-info-circle"),
                                      "About",
                                      class = "btn2",
                                      onclick = "openTab('about')",
                                      href = "#shiny-tab-about",
                                      `data-value` = "about",
                                      `data-toggle` = "tab"
                                    ),
                                    class = "desktop_nav"),
                                    tags$div(
                                      tags$a(
                                        tags$i(class = "fas fa-info-circle"),
                                        class = "btn2",
                                        onclick = "openTab('about')",
                                        href = "#shiny-tab-about",
                                        `data-value` = "about",
                                        `data-toggle` = "tab"
                                      ),
                                      class = "mobile_nav"),
                                    data.step = 14,
                                    data.intro = "Click here to learn more about the app and authors."
                                  ),
                              data.step = 10,
                              data.intro = "You can navigate to different portions of the app using these buttons."),
                            tags$div(
                              actionButton("launchhelp", "Help", icon("question"), class = "btn2"),
                              class = "desktop_nav"
                            ),
                            tags$div(
                              actionButton("launchhelp2", "", icon("question"), class = "btn2", style = "position:absolute; top:0;"),
                              class = "mobile_nav"
                            ),
                            class = "dropdown",
                            style = "width:100%; margin-top: 10px;height:100%;"
                          )
                          )

# sidebar ----------------------------------------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  introBox(
    sidebarMenu(
      tags$div(
        tags$p("Search Options", style = "font-family = 'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif; font-weight: 700;"),
        # user-defined query input
        introBox(
          textInput(
            inputId = "oneword",
            label = NULL,
            value = "personality, general mental ability"
          ),
          data.step = 2,
          data.intro = "Enter your search terms or phrases here. <br><br> Separate multiple terms with a comma. For example, if you wanted to search for abstracts that contain personality <em>or</em> general mental ability type: \"personality, general mental ability\".<br> <br>Power users can also use regular expressions. For example, if you were interested in searching for networks or networking search \"network[[s]|[ing]]\""
        ),
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
          data.intro = "Sometimes an article may match your search by chance. You can increase the number of matching terms required to be included in your search results. This can reduce the number of false positives (Type I errors)."
        ),
        tags$br(),
        introBox(
          sliderInput(
            inputId = "yearrange",
            label = "Published Betweeen",
            min = 1930,
            max = 2019,
            value = c(1930, 2019),
            sep = ""
          ),
          data.step = 5,
          data.intro = "Use the slider to restrict your search to a certain date range. End years are inclusive."
        ),
        tags$br(),
        introBox(
          actionButton("plot",
                       "Search",
                       icon = icon("search"),
                       style = "color: black; margin-left: 15px; margin-bottom: 5px;"),
          data.step = 6,
          data.intro = "Click here to refresh the results after changing your search parameters."
        ),
        
        tags$br(),
        tags$hr(style = "color:white; width:90%; padding-left: 0;"),
        tags$p("Plot Options",  style = "font-family = 'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif; font-weight: 700;"),
        
        introBox(
          radioButtons(
            inputId = "prop",
            label = "Plot Proportion of Published Articles",
            selected = TRUE,
            choices = c("Yes" = TRUE, "No" = FALSE)
          ),
          
          data.step = 7,
          data.intro = "Select \"Yes\" if you are interested in plotting the proportion of articles matching your search on the y-axis. Select \"No\" if you want to plot the raw frequencies. <br><br> Note that proportions are based on SCOPUS database coverage.  Weak coverage will invariably result in
          inaccurate proportion estimates, and earlier dates have notably weaker coverage."
        ),
        
        introBox(
          radioButtons(
            inputId = "journ",
            label = "Plot by Journal",
            selected = FALSE,
            choices = c("Yes" = TRUE, "No" = FALSE)
          ),
          data.step = 8,
          data.intro = "If you want to plot journals separately select \"Yes\" otherwise select \"No\"."
        ),
        # download button
        introBox(
          downloadButton(
            'my_trends',
            'Download .CSV',
            icon = icon("download"),
            style = "color: black; margin-left: 15px; margin-bottom: 5px;"
          ),
          data.step = 9,
          data.intro = "Click here to download your search results as a CSV."
        ),
        
        menuItem("",
                 tabName = "plts_and_analytics",
                 selected = TRUE),
        menuItem("",
                 tabName = "about",
                 selected = FALSE),
        menuItem("",
                 tabName = "searchresults",
                 selected = FALSE),
        menuItem("",
                 tabName = "database_coverage",
                 selected = FALSE),
        style = "font-size: 1.5em; margin-left: 2.5%;"
        )
    ),
    data.step = 1,
    data.intro = "The sidebar contains the controls for changing what terms are searched for, where it's searched for, and how its visualized.<br><br> Let's see what we can do!"
),
width = "350px"
)

# Body ------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
  
  # Defining JS helpers
  shinyjs::useShinyjs(),
  introjsUI(),
  # Including JS to set tab to open when clicking navbar buttons
  tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
  # loading CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "trends_in_IO_style.css")
    
  ),

  tabItems(
    tabItem(tabName = "plts_and_analytics",
            fluidRow(
              introBox(
              box(title = "Publication Trends for User-Specified Query",
                  width = 12,
                  plotlyOutput("plot1")%>%
                    withSpinner(color="#a8605c")),
              data.step = 15,
              data.intro = "This box displays a plot of the proportion of articles that match the user's query. <br><br> This plot, along with all others in this app, are interactive. Hovering over the plot provides more information about the data point. Clicking on the camera button in the top right of the plot exports the image as a .png. If you zoom in or pan, you can return to the default view by double clicking on the plot.")
            ),
            fluidRow(
              introBox(
              box(title = "Citation Trends for User-Specified Query",
                  width = 6,
                  plotlyOutput("plot2")%>%
                    withSpinner(color="#a8605c")),
              data.step = 16,
              data.intro = "This box displays the citation trends for articles matching the user's search (black) and those that don't match the user's search (red)."
              ),
              introBox(
              box(title = "Do People Cite the User-Specified Query More Than Other Articles?",
                  width = 6,
                  DT::DTOutput("citetest")%>%
                    withSpinner(color="#a8605c")),
              data.step = 17,
              data.intro = "This box tests for mean differences in citation rates between articles that match the user's query and those that do not. Models are run by decade to account for potential non-linear interactions between time and the topic. <br><br> \"Estimate With\" reports the predicted number of citations for an article that contains the search term. In contrast, \"Estimate Without\" contains the predicted number of citations for an article that does not match the user search. <br><br> \"Estimate\" and \"SE\" report the estimated regression coefficient for a dummy-coded variable indicating a match. <br><br> Finally, the columns t and p report the hypotheses test testing the null that the regression coefficient differs from 0.",
              data.position = "right")
            )
          ),
    tabItem(
      tabName =  "searchresults",
      box(
        title = "Search Results",
        width = "100%",
        height = "100%",
        DT::DTOutput("table")%>%
          withSpinner(color="#a8605c")
      )
    ),
    tabItem(
      tabName =  "database_coverage",
      box(
        title = "Database Coverage",
        width = "100%",
        height = "100%",
        plotlyOutput(outputId = "coverage")%>%
          withSpinner(color="#a8605c")
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
                  selected = init_selected
                ),
                br(),
                actionButton("selectall", label = "Select All"),
                actionButton("deselectall", label = "Deselect All")
              )
            )),
    tabItem(tabName = "about",
            box(
              h1("Trends in Industrial-Organizational Psychology"),
              
              h3(span("By"),
                 em(
                   a("James Rigby, M.A., University of Houston", href = "mailto:jrigby@uh.edu")
                 ),
                 span("and"),
                 em(
                   a("Zach Traylor, M.S., Texas A&M University", href = "mailto:zktraylor@gmail.com")
                 )),
              
              p(
                "The purpose of this application is to help quickly identify emerging
                trends among major I-O journals (and related literatures) by counting and
                plotting the frequency of term/phrase usage contained in the abstracts of scholarly
                publications from 85 peer-reviewed journals between 1950 and May, 2019.
                Users impute search term(s) and/or phrase(s), and the app then uses the query to identify relevant articles.
                The raw search results are located in the \"Tables\" Menu under the \"Search Results\" tab.
                Additional analytics are reported under the \"Dashboard\" tab, which includes publication frequency visualizations,
                citation rate visualizations, and citation rate analytics."
              ),
              
              
              p(
                "Data were collected from SCOPUS (www.scopus.com).  The only requirement for inclusion in the operational database was that
                articles must have been published in one of the journals listed under the \"Journal Selection\" tab.
                After data were downloaded from SCOPUS, article metadata was verified and standardized within journal
                because journal names and formatting have changed over the last 90 years.
                In total, the operational database contains over 155,000 articles from 85 academic journals."
              ),
              
              p(
                "This application relies on four core functions to query the database, and each of which are heavily dependent on the tidyverse."
              ),
              
              p(class = "textlist", "(1) table_prep() returns the raw search results."),
              p(
                class = "textlist",
                "(2) tidy_trend_plot() plots the publication trends for articles matching the user-specified query."
              ),
              p(
                class = "textlist",
                "(3) cite_plot() returns a plot comparing the citation rates of articles matching the user-specified query."
              ),
              p(
                class = "textlist",
                "(4) cite_pred() returns the resultant statistical estimates of several poisson regression models predicting citation rates using a
                binary indicator variable that stipulates whether the article matches the user-specified query.  The models are
                estimated separately by decade in order to better explore whether and the extent to which the user-specified query became more or less popular over time."
              ),
              
              p(
                "Please note that this app is currently in Beta, and there are still a few bugs
                that need to be resolved.  If you encounter any errors or have suggestions, please
                feel free to contact either author using the above emails."
              ),
              
              width = 12
              ))
              )
            )

ui <- dashboardPage(header, sidebar, body, skin = "purple")