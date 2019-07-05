
################################
##### INSTALLING PACKAGES ######
################################



library(tidyverse)
library(shiny)
library(DT)
library(shinythemes)
library(scales)
library(ggthemes)
library(plotly)
library(readr)
library(foreign)



################################
######### DATA CLEANING ########
################################


##### loading data  ######


france <- read.csv("france_clean.csv")
colnames(france) <- c("Date", "Season", "Home Team", "Away Team", "Scoreline", "Goals For",
                      "Goals Against", "Total Goals", "Goal Difference", "Result")
france$Season <- as.factor(france$Season)

####### data cleaning #######


# stats home games 
france_stats_home <- france %>% 
  group_by(Season, `Home Team`) %>% 
  summarise(`Games Played` = n(), 
            Wins = sum(`Goal Difference` > 0), 
            Draws = sum(`Goal Difference` == 0), 
            Losses = sum(`Goal Difference` < 0), 
            `Win Percentage` = (Wins/`Games Played`)*100,
            `Loss Percentage` = (Losses/`Games Played`)*100,
            `Total Goals For` = sum(`Goals For`),
            `Total Goals Against` = sum(`Goals Against`), 
            `Total Goal Difference` = sum(`Goal Difference`), 
            `Goals For per Game` = `Total Goals For`/`Games Played`, 
            `Goals Against per Game` = `Total Goals Against`/`Games Played`, 
            Points = (Wins*3) + (Draws)
  )

france_stats_home$Venue <- "Home"
france_stats_home <- france_stats_home[c(1, 2, 15, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
colnames(france_stats_home)[2] <- "Team"  

# stats away games 
france_mirror <- france
colnames(france_mirror)[6] <- "GA"
colnames(france_mirror)[7] <- "Goals For"
colnames(france_mirror)[6] <- "Goals Against"
france_mirror$`Goal Difference` <- -france_mirror$`Goal Difference`


france_mirror$Result[france_mirror$`Goal Difference` < 0] <- "Loss"
france_mirror$Result[france_mirror$`Goal Difference` == 0] <- "Draw"
france_mirror$Result[france_mirror$`Goal Difference` > 0] <- "Win"


france_stats_away <- france_mirror %>% 
  group_by(Season, `Away Team`) %>% 
  summarise(`Games Played` = n(), 
            Wins = sum(`Goal Difference` > 0), 
            Draws = sum(`Goal Difference` == 0), 
            Losses = sum(`Goal Difference` < 0), 
            `Win Percentage` = (Wins/`Games Played`)*100,
            `Loss Percentage` = (Losses/`Games Played`)*100,
            `Total Goals For` = sum(`Goals For`),
            `Total Goals Against` = sum(`Goals Against`), 
            `Total Goal Difference` = sum(`Goal Difference`), 
            `Goals For per Game` = `Total Goals For`/`Games Played`, 
            `Goals Against per Game` = `Total Goals Against`/`Games Played`, 
            Points = (Wins*3) + (Draws)
  )

france_stats_away$Venue <- "Away"
france_stats_away <- france_stats_away[c(1, 2, 15, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
colnames(france_stats_away)[2] <- "Team"    

# row binding together home and away stats 
france_stats <- rbind(france_stats_home, france_stats_away)
france_stats$Venue <- as.factor(france_stats$Venue)

# stats table without distinction between home and away games
france_home_all <- france[, -5]
colnames(france_home_all)[3] <- "Team"
colnames(france_home_all)[4] <- "Opponent"
france_home_all$Venue <- "Home"

france_mirror_all <- france_mirror[, -5]
colnames(france_mirror_all)[4] <- "Team"
colnames(france_mirror_all)[3] <- "Opponent"
france_mirror_all <- france_mirror_all[, c(1, 2, 4, 3, 5, 6, 7, 8, 9)]
france_mirror_all$Venue <- "Away"

france_all <- rbind(france_home_all, france_mirror_all)
france_all$Venue <- as.factor(france_all$Venue)

france_stats_all <- france_all %>% 
  group_by(Season, Team) %>% 
  summarise(`Games Played` = n(), 
            Wins = sum(`Goal Difference` > 0), 
            Draws = sum(`Goal Difference` == 0), 
            Losses = sum(`Goal Difference` < 0), 
            `Win Percentage` = (Wins/`Games Played`)*100,
            `Loss Percentage` = (Losses/`Games Played`)*100,
            `Total Goals For` = sum(`Goals For`),
            `Total Goals Against` = sum(`Goals Against`), 
            `Total Goal Difference` = sum(`Goal Difference`), 
            `Goals For per Game` = `Total Goals For`/`Games Played`, 
            `Goals Against per Game` = `Total Goals Against`/`Games Played`, 
            Points = (Wins*3) + (Draws)
  )

france_stats_all$Season <- as.numeric(as.character(france_stats_all$Season))

# formatting integers
france_stats_all$`Win Percentage` <- round(france_stats_all$`Win Percentage`, 2)
france_stats_all$`Loss Percentage` <- round(france_stats_all$`Loss Percentage`, 2)
france_stats_all$`Goals For per Game` <- round(france_stats_all$`Goals For per Game`, 2)
france_stats_all$`Goals Against per Game` <- round(france_stats_all$`Goals Against per Game`, 2)


# season stats with home/away distinction dataset: 
france_stats_all_venue <- france_all %>% 
  group_by(Season, Team, Venue) %>% 
  summarise(`Games Played` = n(), 
            Wins = sum(`Goal Difference` > 0), 
            Draws = sum(`Goal Difference` == 0), 
            Losses = sum(`Goal Difference` < 0), 
            `Win Percentage` = (Wins/`Games Played`)*100,
            `Loss Percentage` = (Losses/`Games Played`)*100, 
            `Total Goals For` = sum(`Goals For`),
            `Total Goals Against` = sum(`Goals Against`), 
            `Total Goal Difference` = sum(`Goal Difference`), 
            `Goals For per Game` = `Total Goals For`/`Games Played`, 
            `Goals Against per Game` = `Total Goals Against`/`Games Played`, 
            Points = (Wins*3) + (Draws)
  )

france_stats_all_venue$`Win Percentage` <- round(france_stats_all_venue$`Win Percentage`, 2)
france_stats_all_venue$`Loss Percentage` <- round(france_stats_all_venue$`Loss Percentage`, 2)
france_stats_all_venue$`Goals For per Game` <- round(france_stats_all_venue$`Goals For per Game`, 2)
france_stats_all_venue$`Goals Against per Game` <- round(france_stats_all_venue$`Goals Against per Game`, 2)

france_stats_all_venue$Season <- as.numeric(as.character(france_stats_all_venue$Season))


france_all$Season <- as.factor(france_all$Season)

# fixing colnames for different input ids: 
colnames(france_all)[3] <- "team"  
colnames(france_stats_all_venue)[3] <- "venue"


####################
# champions dataset: 

france_champions <- france_stats_all %>% 
  group_by(Season) %>% 
  filter(Points == max(Points))

# some teams have the same number of points in one season, so we must look at the goal difference to sort out the champions from the 2nd place winners 
# years when champions have the same number of points as 2nd place winners: 1932, 1945, 1955, 1961, 1978, 1997
# I manually removed those 6 cases - could have written an if statement instead 

france_champions <- france_champions[-c(1, 8, 20, 27, 45, 63),]
france_champions$Team <- droplevels(france_champions$Team)
colnames(france_champions)[1] <- c("season")
france_champions$season <- as.factor(france_champions$season)
france_champions$Points <- as.integer(france_champions$Points)


# adding number of league title column to france_stat_all - manually 

france_stats_all$`Number of League Titles` <- 0

france_stats_all$`Number of League Titles`[france_stats_all$Team == "AJ Auxerre"] <- 1
france_stats_all$`Number of League Titles`[france_stats_all$Team == "FC Sete"] <- 1
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Montpellier HSC"] <- 1
france_stats_all$`Number of League Titles`[france_stats_all$Team == "RC Lens"] <- 1
france_stats_all$`Number of League Titles`[france_stats_all$Team == "RC Roubaix"] <- 1

france_stats_all$`Number of League Titles`[france_stats_all$Team == "FC Sochaux"] <- 2
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Lille OSC"] <- 2
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Racing Club de France"] <- 2


france_stats_all$`Number of League Titles`[france_stats_all$Team == "Stade Reims"] <- 5

france_stats_all$`Number of League Titles`[france_stats_all$Team == "Paris Saint-Germain"] <- 6

france_stats_all$`Number of League Titles`[france_stats_all$Team == "FC Nantes"] <- 7
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Girondins Bordeaux"] <- 7
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Olympique Lyon"] <- 7

france_stats_all$`Number of League Titles`[france_stats_all$Team == "AS Monaco"] <- 8

france_stats_all$`Number of League Titles`[france_stats_all$Team == "AS Saint-Etienne"] <- 11
france_stats_all$`Number of League Titles`[france_stats_all$Team == "Olympique Marseille"] <- 11

france_stats_all$`Number of League Titles` <- as.factor(france_stats_all$`Number of League Titles`)
france_stats_all$`Number of League Titles` <- factor(france_stats_all$`Number of League Titles`, 
                                                     ordered = TRUE, levels = c(0, 1, 2, 5, 6, 7, 8, 11))

is.ordered(france_stats_all$`Number of League Titles`)

# for plotly scatterplot legend - france_stats_all

france_stats_all_plotly <- france_stats_all
france_stats_all_plotly[nrow(france_stats_all_plotly)+1,] <- NA
france_stats_all_plotly[nrow(france_stats_all_plotly)+1,] <- NA
france_stats_all_plotly[nrow(france_stats_all_plotly)+1,] <- NA

france_stats_all_plotly[1479, 1] <- c(1932)
france_stats_all_plotly[1479, 15] <- c(6)
france_stats_all_plotly[1480, 1] <- c(1932)
france_stats_all_plotly[1480, 15] <- c(7)
france_stats_all_plotly[1481, 1] <- c(1932)
france_stats_all_plotly[1481, 15] <- c(8)

france_stats_all_plotly <- france_stats_all_plotly[c(1479:1481, 1:1478), ]

# france_raw for stats table tabset

france_raw <- read.csv("france_raw.csv")




##############################
####### User Interface #######
##############################




# Define UI for application
ui <- shiny::shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Exploring French League (Ligue 1) Data 1932-2016"),
  
# sidebar layout
  sidebarLayout(
    
    # sidebar panel 
    
    sidebarPanel(
      width = 4, 
      img(src = "ligue1.png", height = 275, width = 200),
      br(), br(), 
      
        # conditional panel 1 
      
      conditionalPanel(
        'input.dataset === "Table Games per Season"', 
        
        
        selectInput(inputId = "team", 
                    label = "Team:", 
                    choices = levels(france_all$team), 
                    selected = "AS Monaco"), 
        
        
        selectInput(inputId = "opponent", 
                    label = "Opponent: ", 
                    choices = c("All", levels(france_all$Opponent)), 
                    selected = "All"),

        
        selectInput(inputId = "Season", 
                    label = "Season:", 
                    choices = c(levels(france_all$Season), "All"),
                    selected = "2016"),
         
        
        radioButtons(inputId = "Venue", 
                     label = "Venue:", 
                     choices = c(levels(france_all$Venue), "Both"), 
                     selected = "Home"), 
        
        br(),
        
        helpText("Explore any team's games in any season since 1932 in tabular format."), 
        
        br(), 
        
        helpText(h4("Author: Marine de Franciosi")), 
        
        helpText(p("My Shiny app code can be found", 
                   a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                   ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                   a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), ".")), 
        
        helpText(p("The data used to create this Shiny app comes from Professor Curley's github repository", 
                   tags$a(href = "https://github.com/jalapic/engsoccerdata", "engsoccerdata", target = "_blank"), "."))
      
      ), 
      
      
      
      # conditional panel 2
      
      conditionalPanel(
        'input.dataset === "Season Stats with Barplots"', 
        
        
        selectInput(inputId = "Team", 
                    label = "Team:", 
                    choices = levels(france_stats_all_venue$Team), 
                    selected = "Paris Saint-Germain"), 
        
        br(), 
        
        selectInput(inputId = "yvariable", 
                    label = "Y Variable: ", 
                    choices = c("Wins" = "Wins", "Draws" = "Draws",
                                "Losses" = "Losses", "Win Percentage" = "`Win Percentage`", 
                                "Loss Percentage" = "`Loss Percentage`", "Total Goals For" = "`Total Goals For`", 
                                "Total Goals Against" = "`Total Goals Against`", 
                                "Total Goal Difference" = "`Total Goal Difference`", "Goals For per Game" = "`Goals For per Game`",
                                "Goals Against per Game" = "`Goals Against per Game`", "Points" = "Points"), 
                    selected = "Points"), 
        
        br(), 
        
        selectInput(inputId = "xvariable", 
                    label = "X Variable: ", 
                    choices = c("Season" = "Season"), 
                    selected = "Season"), 
        
        br(), 
        
        radioButtons(inputId = "venue", 
                     label = "Venue: ", 
                     choices = c("Both", levels(france_stats_all_venue$venue)),
                     selected = "Both"), 
        
        br(),
        
        
        helpText("Examine any team's season statistics since 1932. See how stats (unsurprisingly) differ for home versus away games."), 
        
        br(), 
        
        helpText(h4("Author: Marine de Franciosi")), 
        
        helpText(p("My Shiny app code can be found", 
                   a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                   ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                   a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), ".")), 
        
        helpText(p("The data used to create this Shiny app comes from Professor Curley's github repository", 
                   tags$a(href = "https://github.com/jalapic/engsoccerdata", "engsoccerdata", target = "_blank"), "."))
        
        
      ),
  
      
      # conditional panel 3: Animated Scatterplots
      
      conditionalPanel(
        'input.dataset === "Animated Scatterplots"',
        
        br(), 
        
      selectInput(inputId = "Xvariable", 
                    label = "X Variable:", 
                    choices = c( "Points" = "Points", "Win Percentage" = "`Win Percentage`", 
                                "Loss Percentage" = "`Loss Percentage`", "Total Goals For" = "`Total Goals For`", 
                                "Total Goals Against" = "`Total Goals Against`", 
                                "Total Goal Difference" = "`Total Goal Difference`"), 
                    selected = "Points"), 
      
      br(), 
      
      selectInput(inputId = "Yvariable", 
                  label = "Y Variable:", 
                  choices = c("Points" = "Points", "Win Percentage" = "`Win Percentage`", 
                              "Loss Percentage" = "`Loss Percentage`", "Total Goals For" = "`Total Goals For`", 
                              "Total Goals Against" = "`Total Goals Against`", 
                              "Total Goal Difference" = "`Total Goal Difference`"), 
                  selected = "`Win Percentage`"),
      
      br(), 
      
      helpText("Explore how relationships between different statistics evolve over time. Points represent the 20 teams in each season and are colored by the number of titles each team has won since 1932. You can hover over points."),

      br(), 
      
      helpText(h4("Author: Marine de Franciosi")), 
      
      helpText(p("My Shiny app code can be found", 
                 a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                 ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                 a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), ".")), 
      
      helpText(p("The data used to create this Shiny app comes from Professor Curley's github repository", 
                 tags$a(href = "https://github.com/jalapic/engsoccerdata", "engsoccerdata", target = "_blank"), "."))
      
    ),
    
    # conditional panel 4: champions 
    
    conditionalPanel(
      'input.dataset === "Ligue 1 Champions"',
      
      
      selectInput(inputId = "season", 
                  label = "Season  ", 
                  choices = levels(france_champions$season),
                  selected = "2015"),
      
      br(), 
      
      helpText("Find out which team was crowned Ligue 1 Champions in any season from 1932 to 2016."),
      
      br(), 
      
      helpText(h4("Author: Marine de Franciosi")), 
      
      helpText(p("My Shiny app code can be found", 
                 a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                 ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                 a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), ".")), 
      
      helpText(p("The data used to create this Shiny app comes from Professor Curley's github repository", 
                 tags$a(href = "https://github.com/jalapic/engsoccerdata", "engsoccerdata", target = "_blank"), "."))
    ),
    
    # panel 5: all-time records stats table 
    
    conditionalPanel(
      'input.dataset === "All-time Records Statistics Table"', 
      
      sliderInput(inputId = "Seasons", 
                  label = "Season(s): ", 
                  min = 1932, 
                  max = 2016, 
                  value = c(1932, 2016), 
                  sep = "", 
                  ticks = FALSE), 
      
      br(),
      
      selectInput(inputId = "Teams", 
                  label = "Team :", 
                  choices = c("All Teams", levels(france_raw$home)), 
                  selected = "All Teams"), 

      
      selectInput(inputId = "Opponent", 
                  label = "Opponent: ", 
                  choices = c("All Teams", levels(france_raw$visitor)), 
                  selected = "All Teams"), 
 
      
      radioButtons(inputId = "VENUE", 
                   label = "Venue: ", 
                   choices = c("Both", "Home", "Away"),
                   selected = "Both"), 
      
      
      selectInput(inputId = "record", 
                  label = "Choose Record:",
                  choices = c("Overall (all-time) Record",
                              "Season Record",
                              "Biggest Wins",
                              "Worst Losses", 
                              "Highest Scoring Games"),
                  selected = "Overall (all-time) Record"),
      
      
      conditionalPanel(
        "input.record == 'Overall (all-time) Record' | input.record == 'Season Record'",
        
        selectInput(inputId = "sort.record", 
                    label = "Sort Records by:",
                    choices = c("Wins",
                                "Draws",
                                "Losses",
                                "Total Goals For",
                                "Total Goals Against", 
                                "Total Goal Difference",
                                "Points", 
                                "Win Percentage"), 
                    selected = "Wins"),
      
        
        radioButtons(inputId = "order",
                     label = "Order:",
                     choices = c("Descending", "Ascending"), 
                     selected = "Descending",
                     inline = TRUE)), 
      
      numericInput(inputId = "observations",
                   label = "Number of Observations to View: ",
                   value = 20), 
      
      checkboxInput(inputId = "download", 
                    label = "Download Raw Data:", 
                    value = FALSE),
      
      br(),
      
      
      helpText("Explore overall records in this interactive table."),
      helpText(p("Professor Curley's", 
                     a(href = "https://jalapic.shinyapps.io/engsoccerbeta/", "Shiny app", target = "_blank"), 
                     tags$a(href = "https://github.com/jalapic/shinyapps/tree/master/engsoccerbeta", "code", target = "_blank"), 
                     "was of great help to produce this table.")),
      
      br(), 
      
      helpText(h4("Author: Marine de Franciosi")), 
      
      helpText(p("My Shiny app code can be found", 
                 a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                 ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                 a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), ".")), 
      
      helpText(p("The data used to create this Shiny app comes from Professor Curley's github repository", 
                 tags$a(href = "https://github.com/jalapic/engsoccerdata", "engsoccerdata", target = "_blank"), "."))
    ), 
    
    
    # conditional panel 6: download data
    
    conditionalPanel(
      'input.download === true || input.data === "Download Raw Data"',
      
      br(),
      
      radioButtons(inputId = "Filetype",
                   label = "Select Filetype:",
                   choices = c("csv", "dta", "rds", "xlsx"),
                   selected = "csv"),
      
      br(), 
      
      
      checkboxGroupInput(inputId = "selected",
                         label = "Select Variables:",
                         choices = names(france),
                         selected = c("Season", "Home Team", "Away Team", "Scoreline")), 
      
      br(), 
      
      helpText(h4("Author: Marine de Franciosi")), 
      
      helpText(p("My Shiny app code can be found", 
                 a(href = "https://github.com/marinecdf/football-shiny-app/blob/master/frenchleague_app.R", "here", target = "_blank"),
                 ". Any suggestion for improvement would be greatly appreciated and if you have errors to report or comments to make, please contact",
                 a(href = "https://twitter.com/marinecdf", "me on twitter", target = "_blank"), "."))
      )
    ),
    
    
    # main panel
    
    mainPanel(
      width = 8, 
      tabsetPanel(
        id = 'dataset',
        tabPanel('Table Games per Season', DT::dataTableOutput(outputId = "games_table")),
        tabPanel('Season Stats with Barplots', plotlyOutput(outputId = "barplot")), 
        tabPanel('Animated Scatterplots', plotlyOutput(outputId = "scatterplot")),
        tabPanel('Ligue 1 Champions', br(), 
                 img(src = "champions.jpg", height = 235, width = 352), 
                 img(src='champions4.jpg', height = 235, width = 352, align = "right"),
                 img(src = "champions2.jpg", height = 235, width = 352), 
                 img(src='champions3.jpg', height = 235, width = 352, align = "right"), 
                 br(), br(), 
                 textOutput(outputId = "champions_text"), 
                 tags$head(tags$style("#champions_text{
                                 font-size: 30px;
                                 font-style: bold;
                                 }")), 
                 br(),
                 textOutput(outputId = "champions_text2"), 
                 tags$head(tags$style("#champions_text{
                                 font-size: 25px;
                                 font-style: bold;
                                 }")),
                 br(), br(), 
                 tableOutput(outputId = "champion_table"),
        br()),
        tabPanel('All-time Records Statistics Table', tableOutput(outputId = "stats_table")),
        tabPanel('Download Raw Data',
                 br(), 
                 h3("Select your preferred filetype and variables to be included, then hit 'Download Data' to download the raw data"),
                 br(), br(), br(), 
                 downloadButton(outputId = "download_data", label = "Download Data"), 
                br(), br(), br(),
                 h4(HTML(paste0("The data comes from Professor James Curley’s GitHub repository ", 
                             tags$a("engsoccerdata", href = "https://github.com/jalapic/engsoccerdata"), 
                             " and the raw data for French League games and other European Leagues can also be found ", 
                             tags$a("here", href = "https://github.com/jalapic/engsoccerdata/tree/master/data"), 
                             "."))))
      )
    )
  )
))


#################################
######## Server Function ########
#################################



server <- function(input, output) {
  
  # output 1 
  
  output$games_table <- DT::renderDataTable({
    req(input$team)
    req(input$Season)
    req(input$opponent)
    req(input$Venue)
    france_all_selected <- france_all %>% 
      filter(team %in% input$team)
    
    
    if(input$Season != "All")  {
      france_all_selected <- france_all_selected %>% 
        filter(Season %in% input$Season)
    }
    france_all_selected
    
    if(input$opponent != "All") {
      france_all_selected <- france_all_selected %>% 
        filter(Opponent %in% input$opponent)
    }
    france_all_selected
    
    
    if(input$Venue != "Both") {
      france_all_selected <- france_all_selected %>% 
        filter(Venue == input$Venue)
    }
    france_all_selected
    
    DT::datatable(data = france_all_selected, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # output 2 
  
  france_stats_all_venue_subset <- reactive({
    req(input$Team)
    req(input$Season)
    req(input$xvariable)
    req(input$yvariable)
    req(input$venue)
    if (input$venue == "Both") {
      france_stats_all %>% filter(Team %in% input$Team) 
    }
    else {
      france_stats_all_venue %>% 
        filter(Team %in% input$Team) %>% 
        filter(venue %in% input$venue)
    }
  })

  
  output$barplot <- renderPlotly({
    ggplotly({
    ggplot(data = france_stats_all_venue_subset(), 
           aes_string(x = input$xvariable, y = input$yvariable)) + 
      geom_bar(stat = "identity", fill = "cornflowerblue") + 
      scale_x_continuous(breaks = seq(from = 1930, to = 2016, by = 5)) + theme_light() +
      theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
            axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
            axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
            axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"))
   })
 })
  
  
  # output 3: animated scatterplots

  plot <- reactive({

    
    if (input$Xvariable == "Points" & input$Yvariable == "`Win Percentage`") {
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = Points, y = `Win Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
    else 
      if(input$Xvariable == "`Win Percentage`" & input$Yvariable == "Points"){
        
          plot <- ggplot(data = france_stats_all_plotly, 
                   aes(x = `Win Percentage`, y = Points, color = `Number of League Titles`)) + 
              geom_point(aes(frame = Season, 
                             text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                             size = `Number of League Titles`, 
                             alpha = I(0.75))) + 
              theme_light() + labs(size = "League Title(s)") + 
              theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                    axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                    axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                    axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                    legend.title = element_blank(), 
                    legend.text = element_text(color = "black", size = 10, face = "bold"))
      }
      
else 
  if(input$Xvariable == "Points" & input$Yvariable == "`Total Goals For`"){
    
        plot <- ggplot(data = france_stats_all_plotly, 
               aes(x = Points, y = `Total Goals For`, color = `Number of League Titles`)) + 
          geom_point(aes(frame = Season, 
                         text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                         size = `Number of League Titles`, 
                         alpha = I(0.75))) + 
          theme_light() + labs(size = "League Title(s)") + 
          theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                legend.title = element_blank(), 
                legend.text = element_text(color = "black", size = 10, face = "bold"))
  }
  
  else 
    if(input$Xvariable == "`Total Goals For`" & input$Yvariable == "Points"){
      
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals For`, y = Points, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }

  else 
    if(input$Xvariable == "Points" & input$Yvariable == "`Total Goals Against`"){
    
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = Points, y = `Total Goals Against`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals Against`" & input$Yvariable == "Points"){
      
       plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals Against`, y = Points, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "Points" & input$Yvariable == "`Total Goal Difference`"){

          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = Points, y = `Total Goal Difference`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
  else 
    if(input$Xvariable == "`Total Goal Difference`" & input$Yvariable == "Points"){
    
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goal Difference`, y = Points, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }

  else 
    if(input$Xvariable == "Points" & input$Yvariable == "`Loss Percentage`"){
      
         plot <-  ggplot(data = france_stats_all_plotly, 
                 aes(x = Points, y = `Loss Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
  else 
    if(input$Xvariable == "`Loss Percentage`" & input$Yvariable == "Points"){
    
        plot <-  ggplot(data = france_stats_all_plotly, 
                 aes(x = `Loss Percentage`, y = Points, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
  else 
    if(input$Xvariable == "`Win Percentage`" & input$Yvariable == "`Total Goals For`"){
    
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Win Percentage`, y = `Total Goals For`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold")) 
    }
  
  
  else 
    if(input$Xvariable == "`Total Goals For`" & input$Yvariable == "`Win Percentage`"){
      
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals For`, y = `Win Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Win Percentage`" & input$Yvariable == "`Total Goals Against`"){
    
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Win Percentage`, y = `Total Goals Against`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals Against`" & input$Yvariable == "`Win Percentage`"){

         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals Against`, y = `Win Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Win Percentage`" & input$Yvariable == "`Total Goal Difference`"){
      
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Win Percentage`, y = `Total Goal Difference`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goal Difference`" & input$Yvariable == "`Win Percentage`"){
  
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goal Difference`, y = `Win Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Win Percentage`" & input$Yvariable == "`Loss Percentage`"){
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Win Percentage`, y = `Loss Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Loss Percentage`" & input$Yvariable == "`Win Percentage`"){
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Loss Percentage`, y = `Win Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals For`" & input$Yvariable == "`Total Goals Against`"){
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals For`, y = `Total Goals Against`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
   else 
    if(input$Xvariable == "`Total Goals Against`" & input$Yvariable == "`Total Goals For`"){
      
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals Against`, y = `Total Goals For`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals For`" & input$Yvariable == "`Total Goal Difference`"){
    
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals For`, y = `Total Goal Difference`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
  else 
    if(input$Xvariable == "`Total Goal Difference`" & input$Yvariable == "`Total Goals For`"){
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goal Difference`, y = `Total Goals For`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  
  else 
    if(input$Xvariable == "`Total Goals For`" & input$Yvariable == "`Loss Percentage`"){

         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals For`, y = `Loss Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Loss Percentage`" & input$Yvariable == "`Total Goals For`"){
      
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Loss Percentage`, y = `Total Goals For`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals Against`" & input$Yvariable == "`Total Goal Difference`"){
      
        plot <- gplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals Against`, y = `Total Goal Difference`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goal Difference`" & input$Yvariable == "`Total Goals Against`"){
      
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goal Difference`, y = `Total Goals Against`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goals Against`" & input$Yvariable == "`Loss Percentage`"){
      
          plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goals Against`, y = `Loss Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Loss Percentage`" & input$Yvariable == "`Total Goals Against`"){
      
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Loss Percentage`, y = `Total Goals Against`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Total Goal Difference`" & input$Yvariable == "`Loss Percentage`"){
      
         plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Total Goal Difference`, y = `Loss Percentage`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }
  
  else 
    if(input$Xvariable == "`Loss Percentage`" & input$Yvariable == "`Total Goal Difference`"){
    
        plot <- ggplot(data = france_stats_all_plotly, 
                 aes(x = `Loss Percentage`, y = `Total Goal Difference`, color = `Number of League Titles`)) + 
            geom_point(aes(frame = Season, 
                           text = paste0("Team: ", Team, "\n" , `Number of League Titles`, " League Title(s)"),
                           size = `Number of League Titles`, 
                           alpha = I(0.75))) + 
            theme_light() + labs(size = "League Title(s)") + 
            theme(axis.title.x = element_text(color = "midnightblue", size = 15, face = "bold", hjust = 0.5), 
                  axis.title.y = element_text(color = "midnightblue", size = 15, face = "bold", vjust = 0.5), 
                  axis.text.x = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  axis.text.y = element_text(color = "midnightblue", size = 10, face = "bold"), 
                  legend.title = element_blank(), 
                  legend.text = element_text(color = "black", size = 10, face = "bold"))
    }

 })
  
  output$scatterplot <- renderPlotly({
    ggplotly({plot()}, tooltip = "text") %>% 
      animation_slider(currentvalue = list(prefix = "SEASON: ", font = list(color = "red"))) %>% 
      add_annotations(text = sprintf("<b>Title(s)</b>"), xref = "paper", yref = "paper",
                      x = 1.02, xanchor = "left",
                      y = 0.8, yanchor = "bottom",    
                      legendtitle = TRUE, showarrow = FALSE) %>%
      layout(legend = list(y = 0.8, yanchor = "top"))
  })
  
  # output 4: text champions 
  
  output$champions_text <- renderText({
    req(input$season)
    paste("The Ligue 1 Champions for the ", france_champions$season[france_champions$season == input$season], 
          " Season were ", france_champions$Team[france_champions$season == input$season], ". ")
  })
  
    output$champions_text2 <- renderText({
      req(input$season)
      paste("Below is a summary of key statistics for the League Champions in the ", 
            france_champions$season[france_champions$season == input$season], " Season.")
  }) 

  
  output$champion_table <- renderTable({
    france_champions[france_champions$season == input$season, -1]},  
    bordered = TRUE, hover = TRUE)
  
  # output 5: statistics table 
  
  #### first reactive to define all teams options, season span and venue
  
  
  france_reactive <- reactive({
    
    
    if (input$Teams != "All Teams" & input$Opponent != "All Teams"){
      
      
      if(input$VENUE == "Home"){
        
        france_reactive <- france_raw %>%
          filter(Season >= input$Seasons[1]) %>%
          filter(Season <= input$Seasons[2]) %>%
          filter(home == input$Teams & visitor == input$Opponent)  %>%
          mutate(Venue = "Home") %>%
          select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                  `Goals Against` = vgoal, `Goal Difference` = goaldif, Result = result, Venue)
        
      }
      else
        
        if(input$VENUE == "Away"){
          
          france_reactive <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            filter(home == input$Opponent & visitor == input$Teams)  %>%
            mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
            select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                   `Goals Against` = hgoal, `Goal Difference`, Venue)
          
        }
      else
        
        if(input$VENUE == "Both") {
          
          frh <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            filter(home == input$Teams & visitor == input$Opponent)  %>%
            mutate(Venue = "Home") %>%
            select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                   `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
          
          frv <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            filter(home == input$Opponent & visitor == input$Teams)  %>%
            mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
            select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                   `Goals Against` = hgoal, `Goal Difference`, Venue)
       
          france_reactive <- rbind(frh, frv)
          
        }
    }
    
    else
      
      if (input$Teams == "All Teams" & input$Opponent != "All Teams"){

        if(input$VENUE == "Home"){
          
          france_reactive <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            filter(visitor == input$Opponent)  %>%
            mutate(Venue = "Home") %>%
            select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                   `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
          
        }
        else
          
          if(input$VENUE == "Away"){
            
            france_reactive <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(home == input$Opponent)  %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
          }
        else
          
          if(input$VENUE == "Both") {
            
            frh <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(visitor == input$Opponent)  %>%
              mutate(Venue = "Home") %>%
              select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                     `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
            
            frv <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(home == input$Opponent)  %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
            
            france_reactive <- rbind(frh, frv)
            
          }
      }
    
    else
      
      if (input$Teams != "All Teams" & input$Opponent == "All Teams"){
        
        
        if(input$VENUE == "Home"){
          
          france_reactive <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            filter(home == input$Teams)  %>%
            mutate(Venue = "Home") %>%
            select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                   `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
          
        }
        else
          
          if(input$VENUE == "Away"){
            
            france_reactive <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(visitor == input$Teams) %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
          }
        else
        
          if(input$VENUE == "Both"){
            
            frh <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(home == input$Teams)  %>%
              mutate(Venue = "Home") %>%
              select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                     `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
          
            frv <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              filter(visitor == input$Teams)  %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
            
            france_reactive <- rbind(frh, frv)
            
          }
        
      }
    
    
    else
      
      if (input$Teams == "All Teams" & input$Opponent == "All Teams"){
      
        
        if(input$VENUE == "Home"){
          
          france_reactive <- france_raw %>%
            filter(Season >= input$Seasons[1]) %>%
            filter(Season <= input$Seasons[2]) %>%
            mutate(Venue = "Home") %>%
            select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                   `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
          
        }
        else
          
          if(input$VENUE == "Away"){
            
            france_reactive <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
          }
        else
          
          if(input$VENUE == "Both") {
            
            frh <- france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              mutate(Venue = "Home") %>%
              select(Season, Team = home, Opponent = visitor, `Goals For` = hgoal,
                     `Goals Against` = vgoal, `Goal Difference` = goaldif, Venue)
            
            frv <-  france_raw %>%
              filter(Season >= input$Seasons[1]) %>%
              filter(Season <= input$Seasons[2]) %>%
              mutate(`Goal Difference` = -goaldif, Venue = "Away") %>%
              select(Season, Team = visitor, Opponent = home, `Goals For` = vgoal,
                     `Goals Against` = hgoal, `Goal Difference`, Venue)
            
            
            france_reactive <- rbind(frh, frv)
            
          }
        
      }
    
    return(france_reactive)
    
    
  })
  
  
  #### reactive 2 to defined record, sort record by and ascending/descending order
  
  
  france_table_reactive <- reactive({
    
    
    
    if(input$record == "Biggest Wins") {
      
      france_reactive() %>% 
        filter(`Goal Difference` > 0) %>%
        arrange(desc(`Goal Difference`), desc(`Goals For`)) %>%
        mutate(`Final Score` = paste(`Goals For`, `Goals Against`, sep = "-")) %>%
        select(Season, Team, Opponent, `Final Score`, `Goal Difference`, Venue)
      
    }
    
    else
      
      
      if(input$record == "Worst Losses") {
        
        france_reactive() %>% 
          filter(`Goal Difference` < 0) %>% 
          arrange(`Goal Difference`, `Goals For`) %>%
          mutate(`Final Score` = paste(`Goals For`, `Goals Against`, sep = "-")) %>%
          select(Season, Team, Opponent, `Final Score`, `Goal Difference`, Venue)
        
      }
    
    else
      
      
      if(input$record == "Overall (all-time) Record") {
        
        overall <- france_reactive() %>%
          group_by(Team) %>%
          summarise(`Games Played` = n(), 
                    Wins = sum(`Goal Difference` > 0), 
                    Draws = sum(`Goal Difference` == 0), 
                    Losses = sum(`Goal Difference` < 0), 
                    `Total Goals For` = sum(`Goals For`), 
                    `Total Goals Against` = sum(`Goals Against`), 
                    `Total Goal Difference`= sum(`Goal Difference`), 
                    Points = round(((Wins*3) + (Draws))), 
                    `Win Percentage` = round(((Wins/`Games Played`)*100), 2)
          )
        
        if (input$sort.record == "Games Played" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(`Games Played`))
        }
        if(input$sort.record == "Wins" & input$order == "Descending"){ 
          overall <-  overall %>% arrange(desc(Wins))
        }
        if (input$sort.record == "Draws" & input$order == "Descending"){ 
          overall <-  overall %>% arrange(desc(Draws))
        }
        if (input$sort.record == "Losses" & input$order == "Descending"){ 
          overall <-  overall %>% arrange(desc(Losses))
        }
        if (input$sort.record == "Total Goals For" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(`Total Goals For`))
        }
        if (input$sort.record == "Total Goals Against" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(`Total Goals Against`))
        }
        if (input$sort.record == "Total Goal Difference" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(`Total Goal Difference`))
        }
        if (input$sort.record == "Points" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(Points))
        }
        if (input$sort.record == "Win Percentage" & input$order == "Descending"){
          overall <-  overall %>% arrange(desc(`Win Percentage`))
        }
        
        
        if (input$sort.record == "Games Played" & input$order == "Ascending"){
          overall <-  overall %>% arrange(`Games Played`)
        }
        if(input$sort.record == "Wins" & input$order == "Ascending"){ 
          overall <-  overall %>% arrange(Wins)
        }
        if (input$sort.record == "Draws" & input$order == "Ascending"){ 
          overall <-  overall %>% arrange(Draws)
        }
        if (input$sort.record == "Losses" & input$order == "Ascending"){ 
          overall <-  overall %>% arrange(Losses)
        }
        if (input$sort.record == "Total Goals For" & input$order == "Ascending"){
          overall <-  overall %>% arrange(`Total Goals For`)
        }
        if (input$sort.record == "Total Goals Against" & input$order == "Ascending"){
          overall <-  overall %>% arrange(`Total Goals Against`)
        }
        if (input$sort.record == "Total Goal Difference" & input$order == "Ascending"){
          overall <-  overall %>% arrange(`Total Goal Difference`)
        }
        if (input$sort.record == "Points" & input$order == "Ascending"){
          overall <-  overall %>% arrange(Points)
        }
        if (input$sort.record == "Win Percentage" & input$order == "Ascending"){
          overall <-  overall %>% arrange(`Win Percentage`)
        }
        
      
        return(overall)
        
      }
    
    
    
    else
      
      
      if(input$record == "Season Record") {
        
        season_df <- france_reactive() %>%
          group_by(Season, Team) %>%
          summarise(`Games Played` = n(), 
                    Wins = sum(`Goal Difference` > 0), 
                    Draws = sum(`Goal Difference` == 0), 
                    Losses = sum(`Goal Difference` < 0), 
                    `Total Goals For` = sum(`Goals For`), 
                    `Total Goals Against` = sum(`Goals Against`), 
                    `Total Goal Difference`= sum(`Goal Difference`), 
                    Points = round(((Wins*3) + (Draws))), 
                    `Win Percentage` = round(((Wins/`Games Played`)*100), 2)
          ) %>%
          ungroup() 
        
        if (input$sort.record == "Games Played" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(`Games Played`))
        }
        if(input$sort.record == "Wins" & input$order == "Descending"){ 
          season_df <- season_df %>% arrange(desc(Wins))
        }
        if (input$sort.record == "Draws" & input$order == "Descending"){ 
          season_df <- season_df %>% arrange(desc(Draws))
        }
        if (input$sort.record == "Losses" & input$order == "Descending"){ 
          season_df <- season_df %>% arrange(desc(Losses))
        }
        if (input$sort.record == "Total Goals For" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(`Total Goals For`))
        }
        if (input$sort.record == "Total Goals Against" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(`Total Goals Against`))
        }
        if (input$sort.record == "Total Goal Difference" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(`Total Goal Difference`))
        }
        if (input$sort.record == "Points" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(Points))
        }
        if (input$sort.record == "Win Percentage" & input$order == "Descending"){
          season_df <- season_df %>% arrange(desc(`Win Percentage`))
        }
        
        
        if (input$sort.record == "Games Played" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(`Games Played`)
        }
        if(input$sort.record == "Wins" & input$order == "Ascending"){ 
          season_df <- season_df %>% arrange(Wins)
        }
        if (input$sort.record == "Draws" & input$order == "Ascending"){ 
          season_df <- season_df %>% arrange(Draws)
        }
        if (input$sort.record == "Losses" & input$order == "Ascending"){ 
          season_df <- season_df %>% arrange(Losses)
        }
        if (input$sort.record == "Total Goals For" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(`Total Goals For`)
        }
        if (input$sort.record == "Total Goals Against" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(`Total Goals Against`)
        }
        if (input$sort.record == "Total Goal Difference" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(`Total Goal Difference`)
        }
        if (input$sort.record == "Points" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(Points)
        }
        if (input$sort.record == "Win Percentage" & input$order == "Ascending"){
          season_df <- season_df %>% arrange(`Win Percentage`)
        }

        return(season_df)
        
      }
    
    else 
      if(input$record == "Highest Scoring Games") {
      
          
          france_reactive() %>%
            mutate(`Total Goals` = `Goals Against` + `Goals For`,
                   `Final Score` = paste(`Goals For`, `Goals Against`, sep = "-")) %>%
            arrange(desc(`Total Goals`), desc(`Goal Difference`), desc(`Goals For`)) %>%
            mutate(Result = ifelse((`Goal Difference` > 0), "Win",
                                   ifelse(`Goal Difference` < 0, "Loss", "Draw"))) %>%
            select(Season, Team, Opponent, `Final Score`, `Total Goals`, `Goal Difference`, Result,  Venue)
      }
  
  })
  
  
     # defining output 5: 
  
  output$stats_table <- renderTable({
 
    req(input$observations)
    
    head(france_table_reactive(), n = input$observations)}, 
    striped = TRUE, bordered = TRUE,  
    hover = TRUE)
  


  # output 6: download raw data
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("france.", input$Filetype)
    },
    content = function(file) { 
      if(input$Filetype == "csv"){ 
        readr::write_csv(france %>% select(input$selected), path = file) 
      }
      if(input$Filetype == "dta"){ 
        foreign::write.dta(france %>% select(input$selected), file = file) 
      }
      if(input$Filetype == "rds"){ 
        readr::write_rds(france %>% select(input$selected), path = file) 
      }
      if(input$Filetype == "xlsx"){ 
        readr::write_excel_csv(france %>% select(input$selected), path = file) 
      }
    }
  )
  
  observeEvent(input$download, {
    if(input$download){
      showTab(inputId = "dataset", target = "Download Raw Data", select = TRUE)
    } else {
      hideTab(inputId = "dataset", target = "Download Raw Data")
    }
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)


