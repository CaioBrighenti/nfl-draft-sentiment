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
###### CREATE GROB FOR EACH LOGO
nfl_grobs <- list()
for (i in 1:nrow(nfl_teams)) {
    nfl_grobs[[i]] <- rasterGrob(image = logo_grobs[i])
}
nfl_teams$logo_grob <- nfl_grobs

# SETUP GAUGE PLOT FUNCTION
gg.gauge <- function(pos, breaks = c(0, 33, 66, 100), determinent, team_name) {
    get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
        th.start <- pi * (1 - a / 100)
        th.end   <- pi * (1 - b / 100)
        th       <- seq(th.start, th.end, length = 500)
        x        <- r1 * cos(th)
        xend     <- r2 * cos(th)
        y        <- r1 * sin(th)
        yend     <- r2 * sin(th)
        data.frame(x, y, xend, yend)
    }
    
    ggplot() + 
        geom_segment(data = get.poly(breaks[1],breaks[4]), 
                     aes(x = x, y = y, xend = xend, yend = yend, color = xend),size=2) +
        scale_color_gradientn(colors = c("#821624", "white", "#16823f")) +
        geom_segment(data = get.poly(pos - 1, pos + 1, 0.1), aes(x = x, y  =y, xend = xend, yend = yend)) +
        geom_text(data=as.data.frame(breaks), size = 5, fontface = "bold", vjust = 0,
                  aes(x = 0.8 * cos(pi * (1 - breaks / 100)),  y = -0.1), label = c('BOO', '', '', "CHEER")) +
        annotate("text", x  = 0, y = 0,label=determinent,vjust=0,size=8,fontface="bold")+
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              legend.position = "none")+
        labs(
            title = paste("Average comment sentiment \n for", team_name,"fans")
        )
}


### SETUP PICK ORDER


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
        mainPanel(width=9,
             

                column(6,
                    fluidRow(plotOutput("maPlot", height="400px"),
                             actionButton("pickback","", icon = icon("arrow-left",lib="glyphicon")),
                             actionButton("picknext","", icon = icon("arrow-right",lib="glyphicon")),
                             actionButton("pickrefresh","", icon = icon("refresh",lib="glyphicon"))),
                    fluidRow(column(6, htmlOutput("gaugeInfo")),column(6, plotOutput("gaugePlot", height="250px"))),
                    hr(),
                    fluidRow(column(6, htmlOutput("gaugeInfo2")),column(6, plotOutput("gaugePlot2", height="250px")))
                ),
                column(4,
                    fluidRow(plotOutput("sentPlot", width="400px", height="475px"), style='padding:0px; margin:0px'),
                    fluidRow(plotOutput("nPlot", width="400px", height="475px"), style='padding:0px; margin:0px'),
                ),
                column(2,
                       htmlOutput("pickOrder")
                )
            
            
            
            
        )
        
        )
    )

        



server <- function(input, output, session) {
    
    # reactive vals
    obj <- reactiveValues()
    obj$n <- 0
    obj$last_exec <- Sys.time()
    
    ## SETUP PICK ORDER
    obj$curr_round <- 1
    obj$curr_pick <- 1
    
    observe({
        # redo every 5 seconds
        invalidateLater(5000)
        if (Sys.time() - obj$last_exec >= 5){
            # load in reactive data
            comment_data <- as_tibble(isolate(fileReaderData())) 
    
            
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
    
    # output$scatterPlot <- renderPlot({
    #     req(obj$scatter_table)
    #     
    #     #plot
    #     p <- obj$scatter_table %>%
    #         left_join(nfl_teams) %>%
    #         ggplot(aes(x=pos_freq,y=n)) +
    #         geom_grob(aes(label = logo_grob, pos_freq, n), vp.height = 0.1, vp.width = 0.1) +
    #         geom_vline(xintercept = mean(obj$scatter_table$pos_freq), linetype="longdash", alpha=0.5) +
    #         geom_hline(yintercept = mean(obj$scatter_table$n), linetype="longdash", alpha=0.5) +
    #         scale_color_identity() +
    #         scale_x_continuous(limits=c(0,1)) + 
    #         theme_minimal() + 
    #         labs(
    #             title = "Fanbase comment frequency and comment positive over last 30 mins",
    #             caption = "@CaioBrighenti2",
    #             y = "number of comments (last 30 mins)",
    #             x = "share of positive comments (last 30 mins)"
    #         ) +
    #         theme(
    #             legend.position = "none",
    #             text = element_text(family="Roboto")
    #         )
    # 
    #     print(p)        
    # })
    
    ######################################
    ####### LINE CHART BY DIVISION #######
    ######################################
    output$maPlot <- renderPlot({
        req(obj$plot_table)
        # plot
        p <- ggplot(obj$plot_table,aes(x=time,y=sentiment,group=team)) +
            geom_line(aes(color=primary),size=1) +
            #geom_image(data=filter(obj$plot_table,time==0),aes(image=logo), hjust=5, asp = 5) +
            #geom_text_repel(data=filter(obj$plot_table,time==0),aes(label=team),direction = "y", hjust = 0) +
            geom_grob(data=filter(obj$plot_table,time==0),aes(label = logo_grob, time, sentiment), vp.height=.3,vp.width=.3) +
            scale_color_identity() +
            scale_x_continuous(limits = c(-600,100), breaks = seq(-600,0,by=180), labels = c("10m","7m","4m","1m")) +
            scale_y_continuous(limits = c(0,4)) + 
            geom_hline(yintercept = 2, linetype="longdash") +
            #scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
            facet_wrap(~division, nrow = 2) +
            labs(
                title = "Rolling average of /r/NFL comment sentiment by fanbase",
                caption = "@CaioBrighenti2",
                y = "mean sentiment (prior 2 mins)",
                x = "minutes since"
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        print(p)
    })

    
    ######################################
    ########## BAR PLOT BY TEAM ##########
    ######################################
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
        
        p <- ggplot(comment_data, aes(x=sentiment, y=team)) +
            geom_col(aes(fill = primary)) +
            geom_grob(aes(label = logo_grob, sentiment, team), vp.height=.1,vp.width=.1) +
            scale_fill_identity() +
            geom_vline(xintercept = 2, linetype="longdash", alpha=0.5) +
            scale_x_continuous(limits = c(0,4), breaks = c(0,2,4), labels = c("negative", "neutral", "positive")) +
            scale_y_discrete(expand = c(0,1)) + 
            theme_minimal() +
            labs(
                title = "Average comment sentiment",
                subtitle = "Pior 2 mins",
                caption = "@CaioBrighenti2",
                x = "mean sentiment",
                y = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        
        print(p)
    })
    
    ######################################
    ####### COMMENT COUNT BY TEAM ########
    ######################################
    output$nPlot <- renderPlot({
        
        # load in reactive data
        comment_data <- as_tibble(fileReaderData()) %>%
            filter(as.numeric(Sys.time()) - timestamp <= 180) %>%
            drop_na() %>%
            filter(team != "") %>%
            group_by(team) %>%
            summarise(n = n()) %>% 
            right_join(dplyr::select(nfl_teams,team,primary,logo_grob)) %>%
            mutate(n = as.numeric(replace_na(n,0))) %>%
            mutate(team = fct_reorder(team, n)) 
        
        p <- ggplot(comment_data, aes(x=n, y=team)) +
            geom_col(aes(fill = primary)) +
            geom_grob(aes(label = logo_grob, n, team), vp.height=.1,vp.width=.1) +
            scale_fill_identity() +
            scale_y_discrete(expand = c(0,1)) + 
            geom_vline(xintercept = mean(comment_data$n), linetype="longdash", alpha=0.5) +
            theme_minimal() +
            labs(
                title = "Number of comments in prior 2 mins",
                x = "number of comments",
                y = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        
        print(p)
    })
    
    ######################################
    ############# GAUGE PLOT #############
    ######################################
    # credit to stack overflow: https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r
    output$gaugePlot <- renderPlot({
        # temporary
        curr_team <- filter(obj$dat_picks, pick == obj$curr_pick)$team
        
        # get data
        sent <- as_tibble(fileReaderData()) %>%
            filter(as.numeric(Sys.time()) - timestamp <= 180) %>%
            drop_na() %>%
            filter(team == curr_team) %>%
            summarize(sent = mean(sentiment)) %>%
            pull(sent)
        
        # avoid div by 0
        if (is.na(sent)){sent = 2}
        
        # scale sent to 0-100 scale
        sent_scale <- (sent / 4) * 100
        
        
        # plot
        p <- gg.gauge(pos=sent_scale,determinent = as.character(round(sent,1)), team_name = curr_team)
        print(p)
    })
    
    output$gaugePlot2 <- renderPlot({
        req(obj$curr_pick > 1)
        # temporary
        prev_team <- filter(obj$dat_picks, pick == obj$curr_pick-1)$team
        
        # get data
        sent <- as_tibble(fileReaderData()) %>%
            filter(as.numeric(Sys.time()) - timestamp <= 180) %>%
            drop_na() %>%
            filter(team == prev_team) %>%
            summarize(sent = mean(sentiment)) %>%
            pull(sent)
        
        # avoid div by 0
        if (is.na(sent)){sent = 2}
        
        # scale sent to 0-100 scale
        sent_scale <- (sent / 4) * 100
        
        
        # plot
        p <- gg.gauge(pos=sent_scale,determinent = as.character(round(sent,1)), team_name = prev_team)
        print(p)
    })
    
        
    ######################################
    ############ READ IN FILE ############
    ######################################
    fileReaderData <- reactiveFileReader(500, session,
                                         "D:/repositories/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE, quote="")
    
    output$fileReaderText <- renderText({
        dat <- as_tibble(fileReaderData())
        paste("number of observations:", nrow(dat))
    })
    
    
    ######################################
    ####### STREAM IN NEW COMMENTS #######
    ######################################
    output$recentComments <- renderText({
        dat <- as_tibble(fileReaderData()) %>% 
            left_join(lee_logos) %>%
            tail(14) %>%
            mutate(time = as.character(as_datetime(timestamp-14400))) %>%
            separate(time, into = c("ymd","hms"), sep=" ") %>%
            mutate(img_html = paste("<img src='",team_logo,"' width='20'>",sep=""),
                   body = str_trunc(body, 140, side="right"),
                   text = paste(substr(as_hms(hms),1,5), img_html, body)) %>%
            arrange(-timestamp)
        HTML(paste("<h4><b>Recent Comments</b></h4><hr>\n",paste(dat$text, "<br/> <br/>", collapse="")))
    })
    
    ######################################
    ######### INFO ON NEXT PICK ##########
    ######################################
    output$gaugeInfo <- renderText({
        # get team
        curr_team <- filter(obj$dat_picks, pick == obj$curr_pick)$team
        team_name <- filter(teamcolors, league == "nfl", mascot == curr_team) %>% pull(name)
        
        # get pick number
        pick_round <- obj$curr_round
        pick_number <- obj$curr_pick
        
        # get time
        t<-format(Sys.time())
        t_str <- strsplit(t, " ")[[1]][2] %>% substr(1,5)
        t_html <- paste("<center><h1>",t_str,"</h1></center>")
        
        # get team logo
        team_logo <- filter(nfl_teams, team == curr_team) %>% pull(team_logo)
        img_html = paste("<img src='",team_logo,"' width='20'>",sep="")
        
        
        # get latest comment
        # comment <- as_tibble(fileReaderData()) %>% 
        #     left_join(lee_logos) %>%
        #     filter(team == curr_team) %>%
        #     tail(1) %>%
        #     mutate(time = as.character(as_datetime(timestamp-14400))) %>%
        #     separate(time, into = c("ymd","hms"), sep=" ") %>%
        #     mutate(img_html = paste("<img src='",team_logo,"' width='20'>",sep=""),
        #            body = str_trunc(body, 140, side="right"),
        #            text = paste(substr(as_hms(hms),1,5), img_html, body))
        
        # setup header
        team1_header <- paste("</br><i>Coming up</i> <h3>Round ", pick_round, 
                              ", Pick ", pick_number, "</h3> \n <h4>",
                        team_name, " ", img_html, "</h4>",
                        sep="")
        
        HTML(team1_header)
        #HTML(paste(team1_header,"</br>",comment))
    })
    
    output$gaugeInfo2 <- renderText({
        # wait till at least pick #2
        req(obj$curr_pick > 1)
        
        # get team
        prev_team <- filter(obj$dat_picks, pick == obj$curr_pick-1)$team
        prev_team_name <- filter(teamcolors, league == "nfl", mascot == prev_team) %>% pull(name)
        
        # get pick number
        pick_round <- obj$curr_round
        pick_number <- obj$curr_pick - 1
        
        # get time
        t<-format(Sys.time())
        t_str <- strsplit(t, " ")[[1]][2] %>% substr(1,5)
        t_html <- paste("<center><h1>",t_str,"</h1></center>")
        
        # get team logo
        prev_team_logo <- filter(nfl_teams, team == prev_team) %>% pull(team_logo)
        prev_img_html = paste("<img src='",prev_team_logo,"' width='20'>",sep="")
        
        # get player selected
        player <- filter(obj$dat_picks, pick == obj$curr_pick-1)$player
        
        # setup header
        team2_header <- paste("<i>Previous pick</i> <h3>Round ", pick_round, 
                              ", Pick ", pick_number, "</h3> \n <h4>",
                              prev_team_name, " ", prev_img_html, "</h4>",
                              "</br> <h4>Selected: </h4> <b>", player, "</b>",
                              sep="")
        
        HTML(team2_header)
    })
    
    
    ######################################
    ########## PICK ORDER TABLE ##########
    ######################################
    output$pickOrder <- renderText({
        # setup image strings
        dat <- obj$dat_picks %>%
            left_join(lee_logos) %>%
            mutate(pick_html = paste("<h5><b>Pick ", pick, ":</b> ", team, " <img src='",team_logo,"' width='20'> </br></h5>",sep=""))
        HTML(paste("<h4><i>Draft Order</i></h4>",paste(dat$pick_html, collapse="")))
    })
    
    observeEvent(input$pickrefresh,{
        obj$dat_picks <- read.csv("D:/repositories/nfl-draft-sentiment/data/draft_order.csv")
    })
    
    ######################################
    ############ PICK BUTTONS ############
    ######################################
    observeEvent(input$pickback, {
        req(obj$curr_pick > 1)
        
        # go back one pick
        obj$curr_pick <- obj$curr_pick - 1
    })
    
    observeEvent(input$picknext, {
        req(obj$curr_pick < 32)
        
        # go back one pick
        obj$curr_pick <- obj$curr_pick + 1
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
