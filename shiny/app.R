library(shiny)
library(tidyverse)
library(teamcolors)

### GLOBAL VARIABLES
nfl_colors <- teamcolors %>% filter(league == "nfl") %>% dplyr::select(mascot,primary)
init_time <- Sys.time()
n <- 0


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
    ),
    fluidRow(
        column(12,
               plotOutput("maPlot"),
        )
    )
)



server <- function(input, output, session) {
    
    # reactive vals
    obj <- reactiveValues()
    obj$n <- 0
    obj$last_exec <- Sys.time()
    
    observe({
        # redo every 5 seconds
        invalidateLater(5000)
        if (Sys.time() - obj$last_exec >= 5){
            # load in reactive data
            comment_data <- as_tibble(isolate(fileReaderData())) 
            
            # only in window
            comment_data <- comment_data %>%
                filter(as.numeric(Sys.time()) - timestamp <= 120)
            
            # get moving average
            var_time <- as.character(5 * obj$n)
            mean_sent <- comment_data %>%
                group_by(team) %>%
                summarise(sentiment = mean(sentiment))
            mean_sent$time <- var_time
            
            # put in table
            if (obj$n == 0){
                obj$ma_table <- mean_sent
                obj$n <- 1
            } else {
                obj$ma_table <- rbind(obj$ma_table,mean_sent)
                obj$n <- obj$n + 1
            }
            obj$last_exec <- Sys.time()
            
            # put in plot table
            obj$plot_table <- filter(obj$ma_table, time >= (obj$n * 5) - 600) %>% mutate(time = as.numeric(time))
        }
        
    })
    
    output$maPlot <- renderPlot({
        # plot
        p <- ggplot(obj$plot_table,aes(x=time,y=sentiment,group=team)) +
            geom_line(aes(color=sentiment), size=1) +
            scale_color_gradient(low = "red", high = "green", limits=c(0,4)) +
            scale_x_continuous(limits = c(0,600), breaks = seq(0,600,by=100)) +
            geom_hline(yintercept = 2, linetype="longdash") +
            #scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
            facet_wrap(~team) +
            labs(
                title = "Rolling average of /r/NFL comment sentiment by fanbase",
                caption = "@CaioBrighenti2",
                x = "mean sentiment (prior 2 mins)",
                y = ""
            )
        print(p)
    })

    output$sentPlot <- renderPlot({
        
        # load in reactive data
        comment_data <- as_tibble(fileReaderData()) %>%
            filter(as.numeric(Sys.time()) - timestamp <= 120) %>%
            drop_na() %>%
            filter(team != "") %>%
            group_by(team) %>%
            summarise(sentiment = mean(sentiment) - 2, n = n()) %>%
            mutate(team = fct_reorder(team, sentiment))
        
        
        
        # create plot
        # p <- ggplot(comment_data, aes(x=length,y=team)) +
        #     geom_col(aes(fill = n)) +
        #     theme_minimal() +
        #     labs(
        #         title = "Average /r/NFL comment length by fanbase",
        #         caption = "@CaioBrighenti2",
        #         x = "mean comment length",
        #         y = ""
        #     )
        
        p <- ggplot(comment_data, aes(x=sentiment, y=team)) +
            geom_col(aes(fill = sentiment)) +
            scale_fill_gradient(low = "red", high = "green", limits=c(-2,2)) +
            xlim(-2,2) + 
            theme_minimal() +
            labs(
                title = "Average /r/NFL comment length by fanbase (prior 2 mins)",
                caption = "@CaioBrighenti2",
                x = "mean sentiment",
                y = ""
            )
        
        print(p)
    })
    
    output$sentHist <- renderPlot({
        comment_data <- as_tibble(fileReaderData()) %>%
            drop_na()
        
        p <- ggplot(comment_data, aes(sentiment)) +
            geom_histogram(binwidth =  .5, aes(fill=sentiment))
        print(p)
    })
    
    fileReaderData <- reactiveFileReader(5000, session,
                                         "D:/repositories/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE, quote="")
    
    output$fileReaderText <- renderText({
        dat <- as_tibble(fileReaderData())
        paste("number of observations:", nrow(dat))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
