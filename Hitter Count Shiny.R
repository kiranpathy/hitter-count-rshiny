#App for Hitting (Counts)

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)
library(paletteer)
library(shinyWidgets)

#setting generalized strike zone dimensions (while strikezone changes batter-to-batter, this provides an average/estimate to be consistent)
left <- -8.5/12
right <- 8.5/12
top <- 44.08/12
bottom <- 18.29/12
width <- (right - left) / 3
height <- (top - bottom) / 3

df <- read_csv("df.csv")

#alterations to ensure pitches are labeled correctly (differs by data, here are examples of alterations)
#additionally, for this specific project I filtered out any unidentified pitches as well as only focused on data pertaining to my specific team
df <- df %>%
  mutate(TaggedPitchType = ifelse(
    TaggedPitchType == "SInker", "Sinker", TaggedPitchType
  ),
  TaggedPitchType = ifelse(TaggedPitchType == "ChangeUp", "Changeup", TaggedPitchType)) %>%
  mutate(Count = paste0(Balls, "-", Strikes)) %>%
  filter(TaggedPitchType != "Undefined",
         TaggedPitchType != "Other",
         BatterTeam == "My_Team")

#in-zone calculations, chase set up, whiff, custom game id (to simplify game names)
df <- df %>%
  mutate(in_zone = ifelse(PlateLocSide < left | PlateLocSide > right | 
                            PlateLocHeight < bottom | PlateLocHeight > top, "0", "1"),
         chase = ifelse(PitchCall %in% c("Foul", "FoulBall", "FoulBallFieldable", 
                                         "FoulBallNotFieldable", "InPlay", "StrikeSwinging") & in_zone == "0", "1", "0"),
         whiff = ifelse(PitchCall == "StrikeSwinging", "1", "0")) %>%
  mutate(
    in_zone = as.numeric(in_zone),
    chase = as.numeric(chase)) %>%
  mutate(CustomGameID = paste0(
    Date, ":", AwayTeam, " at ", HomeTeam
  ))

#UI
ui <- navbarPage("Hitters",
                 theme = "flatly",
                 tabPanel("Strike Zone",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Team", label = "Select Team",
                                          choices = levels(as.factor(df$BatterTeam))),
                              selectInput("Batter", label = "Select Batter",
                                          choices = levels(as.factor(df$Batter))),
                              pickerInput(
                                inputId = "GameInput",
                                label = HTML("Select Game"),
                                choices = levels(as.factor(df$CustomGameID)),
                                options = list(`actions-box` = TRUE),
                                multiple = T),
                              checkboxGroupInput("Pitch", label = "Select Pitch Type",
                                                 choices = levels(as.factor(df$TaggedPitchType))),
                            checkboxGroupInput("Count", label = "Select Count",
                                               choices = levels(as.factor(df$Count))),
                            width = 2),
                            mainPanel(
                              fluidRow(plotOutput("KZone"), plotOutput("Whiff"), DTOutput("WhiffStats"), plotOutput("Chase"), DTOutput("ChaseStats"))))),
                 tabPanel("Heatmap",
                            mainPanel(
                              fluidRow(plotOutput("Heatmap"))))
                 )
#server
server = function(input, output, session) {
  
#Select Team --> Show those batters  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Batter", "Select Batter",
                      choices = levels(factor(filter(df,
                                                     BatterTeam == isolate(input$Team))$Batter))))
#select batter --> show pitches he saw
  observeEvent(
    input$Batter,
    updateCheckboxGroupInput(session,
                             "Pitch", "Select Pitch Type",
                             choices = levels(factor(filter(df,
                                                            Batter == isolate(input$Batter))$TaggedPitchType))))
  
#select batter --> update games he played in
  observeEvent(
    input$Batter,
    updatePickerInput(session,
                      inputId = "GameInput",
                      choices = sort(unique(df$CustomGameID[df$Batter == input$Batter])),
                      selected = sort(unique(df$CustomGameID[df$Batter == input$Batter]))))
  
#select batter --> update counts he hit in
  observeEvent(
    input$Batter,
    updateCheckboxGroupInput(session,
                             "Count", "Select Count",
                             choices = levels(factor(filter(df,
                                                            Batter == isolate(input$Batter))$Count))))
#strike zone plot
  #start with strike zone plot - general for all pitches seen
  output$KZone <- renderPlot({
    df %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           x = "",
           y = "") +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
    
  })
  #strike zone plot just for chased pitches
  output$Chase <- renderPlot({
    df %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             chase == 1,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           subtitle = "Chase",
           x = "",
           y = "") +
      geom_text(aes(label = PitchNo)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
  })
  #strike zone plot just for whiffed pitches
  output$Whiff <- renderPlot({
    df %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             whiff == 1,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           subtitle = "Whiff",
           x = "",
           y = "") +
      geom_text(aes(label = PitchNo)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
  })
  #data table for the stats on whiffed pitches
  output$WhiffStats <- renderDT({
    whiffdf <- df %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             whiff == 1,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput))
    
    req(nrow(whiffdf) > 0)
    
    whiffdf %>%
      select(PitchNo, Count, PitcherThrows, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, VertApprAngle)
    
  })
  #data table for the stats on chased pitches
  output$ChaseStats <- renderDT({
    chasedf <- df %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             chase == 1,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput))
    
    req(nrow(chasedf) > 0)
    
    chasedf %>%
      select(PitchNo, Count, PitcherThrows, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, VertApprAngle)
    
  })
  #heatmap that shows original kzone from above, but separated out by different pitches - since it's interactive, will change when you select different counts for different pitches as well
  output$Heatmap <- renderPlot({
    
    heat_colors_interpolated <- colorRampPalette(paletteer_d("RColorBrewer::RdBu", 
                                                             n = 9,
                                                             direction = -1))(16)
    FB <- df %>%
      filter(TaggedPitchType == "Fastball")

    FBplot <- FB %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Fastball")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CH <- df %>%
      filter(TaggedPitchType == "Changeup")
    
    CHplot <- CH %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Changeup")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    SL <- df %>%
      filter(TaggedPitchType == "Slider")
    
    SLplot <- SL %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Slider")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CB <- df %>%
      filter(TaggedPitchType == "Curveball")
    
    CBplot <- CB %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Curveball")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CUT <- df %>%
      filter(TaggedPitchType == "Cutter")
    
    CUTplot <- CUT %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Cutter")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    SNK <- df %>%
      filter(TaggedPitchType == "Sinker")
    
    SNKplot <- SNK %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Sinker")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    FS <- df %>%
      filter(TaggedPitchType == "Splitter")
    
    FSplot <- FS %>%
      filter(BatterTeam == input$Team,
             Batter == input$Batter,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Splitter")) +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    ggarrange(FBplot, CHplot, SLplot, CBplot, CUTplot, SNKplot, FSplot, ncol = 3, nrow = 3)
    
  })
    
}

shinyApp(ui = ui, server = server)