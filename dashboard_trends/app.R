library(shiny)

# defines UI for application that plots a histogram
ui <- fluidPage(

    # application title
    titlePanel("Trends in Industrial-Organizational Psychology"),

    # sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of Bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # displays plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# defines server logic required to plot a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generates bins based on input$bins from ui.R
        x <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # plots histogram with specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# runs application
shinyApp(ui = ui, server = server)