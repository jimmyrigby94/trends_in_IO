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
                                    data.intro = "Click here to navigate to the main dashboard. This is where you are now."
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
                                    data.intro = "Click here to see a table of the search results.  The table includes the title, metadata such as the DOI, and the number of terms in the abstract of the articles that match the user's search."
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
                                    data.intro = "Click here to explore the number of articles by each journal in our database."
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
                                    data.intro = "Click here to learn more about the app and its authors."
                                  ),
                              data.step = 10,
                              data.intro = "Navigate to different parts of the app using these buttons."),
                            introBox(
                            tags$div(
                              actionButton("launchhelp", "Help", icon("question"), class = "btn2"),
                              class = "desktop_nav"
                            ),
                            tags$div(
                              actionButton("launchhelp2", "", icon("question"), class = "btn2", style = "position:absolute; top:0;"),
                              class = "mobile_nav"
                            ),
                            data.step = 18,
                            data.intro="You can relaunch this tutorial at any time by navigating to the "About" tab at the top of the window."
                            ),
                            class = "dropdown",
                            style = "width:100%; height:100%;"
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
          data.intro = "Enter your search terms or phrases here. <br><br> Separate multiple terms with a comma.  For example, if you wanted to search for abstracts that contain personality <em>OR</em> general mental ability, you would type: \"personality, general mental ability\". <br><br> Power users can also use regular expressions.  For example, if you were interested in searching for networks or networking, you would search \"network[[s]|[ing]]\"".
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
          data.intro = "Click here to select your preference of journals that are searched.  Journals with a check next to them will be included in your search."
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
          data.intro = "Sometimes an article may match your search by chance.  You can increase how often a search term must be present in a given abstract in order to be considered a match and included in your results, which can reduce the number of false positives (Type I errors)."
        ),
        tags$br(),
        introBox(
          sliderInput(
            inputId = "yearrange",
            label = "Published Betweeen",
            min = 1930,
            max = 2020,
            value = c(1930, 2020),
            sep = ""
          ),
          data.step = 5,
          data.intro = "Use the slider to restrict your search to a specified range of dates (inclusive)."
        ),
        tags$br(),
        introBox(
          actionButton("plot",
                       "Search",
                       icon = icon("search"),
                       style = "color: black; margin-left: 15px; margin-bottom: 5px;"),
          data.step = 6,
          data.intro = "Click here to begin your search or to refresh your results after changing your search parameters."
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
          data.intro = "This toggle allows you to select whether you want to plot the proportion of articles that match your search or the raw frequencies. <br><br> Select \"Yes\" if you are interested in plotting the proportion of articles matching your search on the y-axis.  Select \"No\" if you want to plot the raw frequencies. <br><br> Please note that proportions are based on SCOPUS database coverage.  Weak coverage will invariably result in inaccurate proportion estimates (earlier dates have notably weaker coverage)."
        ),
        
        introBox(
          radioButtons(
            inputId = "journ",
            label = "Plot by Journal",
            selected = FALSE,
            choices = c("Yes" = TRUE, "No" = FALSE)
          ),
          data.step = 8,
          data.intro = "If you would like to plot individual journals, select \"Yes\".  Selecting \"No\" will plot frequencies that reflect an aggregate of all of the journals selected to be included in your search."
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
          data.intro = "Click here to download your search results as a .CSV file."
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
    data.intro = "The sidebar contains the controls for specifying the terms that are to be searched, journals wherein the terms are searched, and how they are visualized. <br><br> Let's see what we can do!"
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
              data.intro = "This box displays a plot of the proportion of articles that match the user's query. <br><br> This plot, along with all others in this app, are interactive.  Hovering over the plot provides more information about the data point.  Clicking on the camera button in the top right of the plot exports the image as a .png.  If you zoom in or pan, you can return to the default view by double clicking on the plot.")
            ),
            fluidRow(
              introBox(
              box(title = "Citation Trends for User-Specified Query",
                  width = 6,
                  plotlyOutput("plot2")%>%
                    withSpinner(color="#a8605c")),
              data.step = 16,
              data.intro = "This box displays the citation trends for articles matching (black) and not matching (red) the user's search."
              ),
              introBox(
              box(title = "Do People Cite the User-Specified Query More Than Other Articles?",
                  width = 6,
                  DT::DTOutput("citetest")%>%
                    withSpinner(color="#a8605c")),
              data.step = 17,
              data.intro = "This box tests for mean differences in citation rates between articles that do and do not match the user's query.  Models are estimated by decade to account for potential non-linear interactions between time and topic. <br><br>  \"Estimate With\" reports the predicted number of citations for an article that contains the search term.  In contrast, \"Estimate Without\" contains the predicted number of citations for an article that does not match the user's search. <br><br>  \"Effect\" and \"Standard Error\" reflect the estimated regression coefficient for a dummy-coded variable indicating a match. <br><br>  Finally, the columns \"<em>t</em>\" and \"<em>p</em>\" contain the test statistics and corresponding <em>p</em>-values for the test of the hypothesis that the regression coefficient statistically differs from 0, respectively.",
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
            fluidRow(
              box(
                tags$h2(icon("pen-alt"), "Cite Our Work"),
                tags$hr(),
                tags$div(
                  tags$a(tags$p("Rigby, J., & Traylor, Z. (in press). Capturing trends in industrial-organizational psychology: A shiny web application.", 
                                tags$em("Human Performance."),
                                class = "citation"), href = "#", target = "_blank"))
                
              ),
              box(
                tags$h2(icon("github"),"Explore Our Code"),
                tags$hr(),
                tags$div(
                  tags$p(
                    tags$a("Repository", target = "_blank", href = "https://github.com/jimmyrigby94/trends_in_IO")
                  ),
                  tags$p(
                    tags$a("Open an Issue", target = "_blank", href = "https://github.com/jimmyrigby94/trends_in_IO/issues")
                  ),
                  style = "font-size: 300%;"
                ))
            ), 
            fluidRow(
              box(
                tags$h2(icon("redo"), "Introduction and Tutorial Window"),
                actionButton("relaunch-modal", "Click Here to Reopen", class = "btn2", style = "width:100%;")
              ),
              box(
                tags$h2(icon("envelope"), "Contact the Authors"),
                tags$hr(),
                tags$div(
                  tags$p(a("James Rigby, M.A., University of Houston", href = "mailto:jrigby@uh.edu")),
                  tags$p(a("Zach Traylor, M.S., Texas A&M University", href = "mailto:zktraylor@gmail.com")),
                  style = "font-size:300%;"
                )
              )
            )
    )
  ))


ui <- dashboardPage(header, sidebar, body, skin = "purple")