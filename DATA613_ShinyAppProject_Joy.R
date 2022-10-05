#Data 613 Shiny App Project
#Jeremy Joy

library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)

#pga <- read_csv("PGADataClean.csv")
pga <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQlhkEdoneo02n_r_hu_4VT5YOv8LLK08URzV7c1X88f368Lbs8Cp47ZjDdddk-QYUtA6p80qlEjpg0/pub?output=csv")
#view(pga)

#which(is.na(pga), arr.ind = T)

stat1 <- pga %>% #used for category input tab 1 - deselecting SGStats, Name, and Year
  select(Rounds, AvgScore, AvgDist, FairwayPct, GiR, 
         AvgPutts, AvgScrambling, Top10, Wins, Points, Money)

stat2 <- pga %>% #used for category in tab 2 - deselecting Name and Year
  select(Rounds, AvgScore, AvgDist, FairwayPct, GiR, 
         AvgPutts, AvgScrambling, SGOffTee, SGApproach, SGAround_Green,
         SGPutts, SGTotal, Top10, Wins, Points, Money)

stat3 <- pga %>% #used for success variable in tab 3
  select(AvgScore, Top10, Wins, Points, Money)



#PGA Tour Player Analysis Shiny App

{
  shinyApp(
    ui <- fluidPage(
      setBackgroundColor(color = c("yellow", "seagreen"), gradient = "radial",
                         direction = c("top", "left")),
      titlePanel("PGA Tour Player Statistics (2010-2018)"),
      tags$h6("The data includes the following descriptive variables for each season:", 
              tags$b("Rounds"),"- rounds of golf played,",tags$b("AvgScore"),"- average 
      score per round,",tags$b("AvgDist"),"- average driving distance,",tags$b("FairwayPct"),
      "- percentage of fairways hit,",tags$b("GiR"),"- Greens in Regulation (%),",tags$b("AvgPutts"),
      "- average number of putts per round,",tags$b("AvgScrambling"),"- percentage of 
      time a player misses the GiR but makes par or better,",tags$b("SGOffTee"),
      "- strokes gained off the tee (%),",tags$b("SGApproach"),"- strokes gained on 
      approach (%),",tags$b("SGAroundGreen"),"- strokes gained around the green (%),",
      tags$b("SGPutts"),"- strokes gained putting (%),",tags$b("SGTotal"),"- total 
      strokes gained (%),",tags$b("Top10"),"- number of tournament top 10 finishes,",
      tags$b("Wins"),"- number of tournament wins,",tags$b("Points"),"- number of 
      FedEX Cup Points,",tags$b("Money"),"- amount of tournament winnings in 
      thousands of dollars."),
      tabsetPanel(
        tabPanel("Yearly Player Stats", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(selectInput("var1", "Name", 
                                            choices = pga$Name,
                                            selected = 1),
                                selectInput("var2", "Category",
                                            choices = names(stat1))),
                   mainPanel(
                     plotOutput("plot")))),
        tabPanel("Yearly Top 20", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(selectInput("var3", "Year",
                                            choices = pga$Year,
                                            selected = 1),
                                selectInput("var4", "Category", 
                                            choices = names(stat2))),
                   mainPanel(
                     plotOutput("factor")))),
        tabPanel("Measures of Success", fluid = TRUE, 
                 sidebarLayout(
                   sidebarPanel(selectInput("var5", "Success Variable",
                                            choices = names(stat3)),
                                selectInput("var6", "Measure",
                                            choices = names(stat2))),
                   mainPanel(
                     plotOutput("point")))),
        tabPanel("Data by Player", fluid = TRUE,
                 mainPanel(selectInput("var7", "Name",
                                       choices = pga$Name,
                                       selected = 1)),
                 DT::dataTableOutput("table"))
      )
    ),
    
    server <- function(input, output) {
      playerfilter <- reactive({                  #Tab 1 - Yearly Player Stats
        pga %>%
          filter(Name %in% input$var1)
      })
      output$plot <- renderPlot({
        ggplot(data = playerfilter(), aes(x = Year, y = .data[[input$var2]])) +
          geom_bar(stat="identity", color = "black", fill = "orangered2") +
          geom_text(aes(label = .data[[input$var2]]), vjust=1.5, color = "black") +
          theme(axis.text.y = element_text(face = "bold"),
                axis.text.x = element_text(face = "bold")) 
      })
      yearfilter <- reactive({                      #Tab 2 - Yearly Top 20
        if (input$var4 %in% c("AvgScore", "AvgPutts")) {
          pga %>% filter(Year %in% input$var3) %>%
            top_n(-20, .data[[input$var4]])
        } else {
          pga %>% filter(Year %in% input$var3) %>%
            top_n(20, .data[[input$var4]])
        }
      })
      output$factor <- renderPlot({
        if (input$var4 %in% c("AvgScore", "AvgPutts")) {
          ggplot(data = yearfilter(), aes(x= .data[[input$var4]], 
                                          y = fct_reorder(Name, .data[[input$var4]], .desc = T),
                                          color = .data[[input$var4]])) +
            geom_point(size = 5) +
            scale_x_continuous(trans = "reverse") +
            scale_color_gradient(low="orangered4", high="orangered1") +
            theme(legend.position = "none",
                  axis.text.y = element_text(face = "bold"),
                  axis.text.x = element_text(face = "bold")) +
            labs(y = "Name")
        }  else {
          ggplot(data = yearfilter(), aes(x= .data[[input$var4]], 
                                          y = fct_reorder(Name, .data[[input$var4]]),
                                          color = .data[[input$var4]])) +
            geom_point(size = 5) +
            scale_color_gradient(low="orangered1", high="orangered4") +
            theme(legend.position = "none",
                  axis.text.y = element_text(face = "bold"),
                  axis.text.x = element_text(face = "bold")) +
            labs(y = "Name")  
        }
      })
      output$point <- renderPlot({                #Tab 3 - Measures of Success
        ggplot(data = pga, aes(x = .data[[input$var6]], y = .data[[input$var5]],
                               color = Year)) +
          geom_point() +
          scale_color_gradient(low="orangered1", high="orangered4") +
          theme(axis.text.y = element_text(face = "bold"),
                axis.text.x = element_text(face = "bold"))
      })
      tablefilter <- reactive({                   #Tab 4 - Data by Player  
        pga %>%
          filter(Name %in% input$var7) %>%
          select(2:18) %>%
          transpose(make.names = "Year", fill = NA, ignore.empty = TRUE, keep.names = "Category")
      })
      output$table <- DT::renderDataTable(
        tablefilter(),options = list(pageLength = 16,
                                     paging = FALSE,
                                     searching = FALSE)
      )
    }
  ) 
}




