library(shiny)
library(tidyverse)
library(teamcolors)
library(ggimage)
library(ggrepel)
library(magick)
library(grid)
library(ggpmisc)
library(extrafont)
library(lubridate)
library(hms)
loadfonts(device = "win", quiet = TRUE)

### GLOBAL VARIABLES
lee_logos <- read.csv("D:/repositories/nfl-draft-sentiment/lee_logos.csv")
nfl_teams <- teamcolors %>% filter(league == "nfl") %>% dplyr::select(mascot,division,primary) %>% rename(team = mascot) %>%
    left_join(lee_logos) %>% mutate(team_logo = as.character(team_logo))
logo_grobs <- image_read(nfl_teams$team_logo)
# create grob for each logo
nfl_grobs <- list()
for (i in 1:nrow(nfl_teams)) {
    nfl_grobs[[i]] <- rasterGrob(image = logo_grobs[i])
}
nfl_teams$logo_grob <- nfl_grobs



ui <- fluidPage(
    
    # avoid greying out plot while recalculating
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}"
    ),
    
    titlePanel("NFL Draft sentiment tracker - @CaioBrighenti"),
    
    sidebarLayout(
        
        # Sidebar with a slider input
        sidebarPanel(width = 3,
                     htmlOutput("recentComments")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(12,
                       plotOutput("maPlot", height="500px"),
                       #verbatimTextOutput("fileReaderText")
                )
            ),
            fluidRow(
                column(12,
                       plotOutput("sentPlot"),
                )
            )
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
            
            ### X = SHARE OF POSITIVE COMMENTS, Y =  TOTAL COMMENTS
             obj$scatter_table <- left_join(
                dplyr::select(nfl_teams,team),
                filter(comment_data, as.numeric(Sys.time()) - timestamp <= 3600) %>%
                mutate(sentiment = case_when(
                    sentiment >= 3 ~ "Positive",
                    sentiment == 2 ~ "Neutral",
                    sentiment <= 1 ~ "Negative"
                )) %>%
                group_by(team) %>%
                add_count() %>%
                filter(sentiment != "Neutral") %>%
                group_by(team, n, sentiment) %>%
                summarise(n2 = n()) %>%
                mutate(pos_freq = n2 / sum(n2)) %>%
                filter(sentiment == "Positive") %>%
                dplyr::select(team, n, pos_freq)) %>%
                mutate(
                    n = replace_na(n,0),
                    pos_freq = replace_na(pos_freq,0)
                )
                
            
            
            # only in window
            comment_data <- comment_data %>%
                filter(as.numeric(Sys.time()) - timestamp <= 180)
            
            # get moving average
            var_time <- as.character(5 * obj$n)
            mean_sent <- left_join(
                dplyr::select(nfl_teams,team),
                (comment_data %>%
                group_by(team) %>%
                summarise(sentiment = mean(sentiment)))
            ) %>%
                mutate(sentiment = replace_na(sentiment,2),
                       time = var_time)
            
            # put in tables
            if (obj$n == 0){
                # moving average table
                obj$ma_table <- mean_sent

                # update n
                obj$n <- 1
            } else {
                # moving average table
                obj$ma_table <- rbind(obj$ma_table,mean_sent)
                
                # update n
                obj$n <- obj$n + 1
            }
            obj$last_exec <- Sys.time()
            
            # put in plot table
            curr_time <- max(as.numeric(obj$ma_table$time))
            obj$plot_table <- filter(obj$ma_table, as.numeric(time) >= (obj$n * 5) - 600) %>% mutate(time = as.numeric(time) - curr_time) %>%
                left_join(nfl_teams)
            
        }
        
    })
    
    output$scatterPlot <- renderPlot({
        req(obj$scatter_table)
        
        #plot
        p <- obj$scatter_table %>%
            left_join(nfl_teams) %>%
            ggplot(aes(x=pos_freq,y=n)) +
            geom_grob(aes(label = logo_grob, pos_freq, n), vp.height = 0.1, vp.width = 0.1) +
            geom_vline(xintercept = mean(obj$scatter_table$pos_freq), linetype="longdash", alpha=0.5) +
            geom_hline(yintercept = mean(obj$scatter_table$n), linetype="longdash", alpha=0.5) +
            scale_color_identity() +
            scale_x_continuous(limits=c(0,1)) + 
            theme_minimal() + 
            labs(
                title = "Fanbase comment frequency and comment positive over last 30 mins",
                caption = "@CaioBrighenti2",
                y = "number of comments (last 30 mins)",
                x = "share of positive comments (last 30 mins)"
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
    
        print(p)        
    })
    
    output$fullMaPlot <- renderPlot({
        req(obj$ma_table)
        curr_time <- max(as.numeric(obj$ma_table$time))
        # plot
        p <- obj$ma_table %>% 
            mutate(time = as.numeric(time) - curr_time) %>%
            left_join(nfl_teams) %>%
            ggplot(aes(x=time,y=sentiment,group=team)) +
            geom_line(aes(color=primary),size=1) +
            #geom_image(data=filter(obj$plot_table,time==0),aes(image=logo), hjust=5, asp = 5) +
            #geom_text_repel(data=filter(obj$plot_table,time==0),aes(label=team),direction = "y", hjust = 0) +
            geom_grob(data=filter(obj$plot_table,time==0),aes(label = logo_grob, time, sentiment)) +
            scale_color_identity() +
            scale_y_continuous(limits = c(0,4)) + 
            geom_hline(yintercept = 2, linetype="longdash") +
            #scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
            facet_wrap(~team, ncol=4) +
            labs(
                title = "Recent rolling average of /r/NFL comment sentiment by division",
                caption = "@CaioBrighenti2",
                y = "mean sentiment (prior 2 mins)",
                x = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        print(p)
    })
    
    output$maPlot <- renderPlot({
        req(obj$plot_table)
        # plot
        p <- ggplot(obj$plot_table,aes(x=time,y=sentiment,group=team)) +
            geom_line(aes(color=primary),size=1) +
            #geom_image(data=filter(obj$plot_table,time==0),aes(image=logo), hjust=5, asp = 5) +
            #geom_text_repel(data=filter(obj$plot_table,time==0),aes(label=team),direction = "y", hjust = 0) +
            geom_grob(data=filter(obj$plot_table,time==0),aes(label = logo_grob, time, sentiment), vp.height=.25,vp.width=.25) +
            scale_color_identity() +
            scale_x_continuous(limits = c(-600,100), breaks = seq(-600,0,by=180), labels = c("10m ago","7m ago","4m ago","1m ago")) +
            scale_y_continuous(limits = c(0,4)) + 
            geom_hline(yintercept = 2, linetype="longdash") +
            #scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
            facet_wrap(~division, nrow = 2) +
            labs(
                title = "Rolling average of /r/NFL comment sentiment by fanbase",
                caption = "@CaioBrighenti2",
                y = "mean sentiment (prior 2 mins)",
                x = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        print(p)
    })

    output$sentPlot <- renderPlot({
        
        # load in reactive data
        comment_data <- as_tibble(fileReaderData()) %>%
            filter(as.numeric(Sys.time()) - timestamp <= 180) %>%
            drop_na() %>%
            filter(team != "") %>%
            group_by(team) %>%
            summarise(sentiment = mean(sentiment), n = n()) %>% 
            right_join(dplyr::select(nfl_teams,team,primary)) %>%
            mutate(sentiment = replace_na(sentiment,2)) %>%
            left_join(nfl_teams) %>%
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
            geom_col(aes(fill = primary)) +
            geom_grob(aes(label = logo_grob, sentiment, team), vp.height=.1,vp.width=.1) +
            scale_fill_identity() +
            geom_vline(xintercept = 2, linetype="longdash", alpha=0.5) +
            xlim(0,4) + 
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45,hjust = 1) 
            ) +
            labs(
                title = "Average /r/NFL comment sentiment by fanbase (prior 2 mins)",
                caption = "@CaioBrighenti2",
                x = "mean sentiment",
                y = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            ) +
            coord_flip()
        
        print(p)
    })
    
    output$sentHist <- renderPlot({
        comment_data <- as_tibble(fileReaderData()) %>%
            drop_na()
        
        p <- ggplot(comment_data, aes(sentiment)) +
            geom_histogram(binwidth =  .5, aes(fill=sentiment))
        print(p)
    })
    
    fileReaderData <- reactiveFileReader(500, session,
                                         "D:/repositories/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE, quote="")
    
    output$fileReaderText <- renderText({
        dat <- as_tibble(fileReaderData())
        paste("number of observations:", nrow(dat))
    })
    
    output$recentComments <- renderText({
        dat <- as_tibble(fileReaderData()) %>% 
            left_join(lee_logos) %>%
            tail(15) %>%
            mutate(time = as.character(as_datetime(timestamp-14400))) %>%
            separate(time, into = c("ymd","hms"), sep=" ") %>%
            mutate(img_html = paste("<img src='",team_logo,"' width='20'>",sep=""),
                   text = paste(substr(as_hms(hms),1,5), img_html, body)) %>%
            arrange(-timestamp)
        HTML(paste(dat$text, "<br/> <br/>"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
