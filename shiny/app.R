library(shiny)
library(tidyverse)
library(teamcolors)

### GLOBAL VARIABLES
nfl_colors <- teamcolors %>% filter(league == "nfl") %>% dplyr::select(mascot,primary)


ui <- fluidPage(
    
    # avoid greying out plot while recalculating
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}"
    ),
    
    # Application title
    titlePanel("NFL Draft Sentiment"),

    fluidRow(
        column(12,
               plotOutput("sentPlot"),
               verbatimTextOutput("fileReaderText")
        )
    )
)



server <- function(input, output, session) {

    output$sentPlot <- renderPlot({
        
        # load in reactive data
        comment_data <- as_tibble(fileReaderData()) %>%
            group_by(team) %>%
            summarise(length = mean(length))
        
        
        
        # create plot
        p <- ggplot(comment_data, aes(x=length,y=team)) +
            geom_col(aes(fill = team)) +
            theme_minimal() +
            labs(
                title = "Average /r/NFL comment length by fanbase",
                caption = "@CaioBrighenti2",
                x = "mean comment length",
                y = ""
            )
        
        print(p)
    })
    
    fileReaderData <- reactiveFileReader(1500, session,
                                         "D:/repositories/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE)
    
    output$fileReaderText <- renderText({
        dat <- as_tibble(fileReaderData())
        paste("number of observations:", nrow(dat))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
