library(shiny)
library(tidyverse)


ui <- fluidPage(
    
    # avoid greying out plot while recalculating
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}"
    ),
    
    # Application title
    titlePanel("NFL Draft Sentiment"),

    fluidRow(
        column(12,
               plotOutput("sentPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$sentPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        comment_data <- as_tibble(fileReaderData())
        x    <- comment_data$length
        
        # draw the histogram with the specified number of bins
        p <- ggplot(comment_data, aes(x=length)) +
            geom_histogram(binwidth = 10) +
            facet_wrap(~team, scales = "free")
        
        print(p)
    })
    
    fileReaderData <- reactiveFileReader(1500, session,
                                         "D:/repositories/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE)
    
    # output$fileReaderText <- renderText({
    #     dat <- as_tibble(fileReaderData())
    #     print(dat[nrow(dat),]$body)
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
