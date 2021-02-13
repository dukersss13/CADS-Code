setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Class Work/Activity 6")
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
load("twdkills.RData")
head(kills)

kills_df <- melt(kills,id="Season")
kills_df[is.na(kills_df)] <- 0

#working w walking dead dataset

ui2 = fluidPage(
  titlePanel("Walking Dead Kills", window = "CS614 Lecture 7"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput("CharInput","Character",choices=c("Rick","Morgan","Glen","Daryl","Carl","Michonne","Carol","Maggie"),selected="Rick"),
                 radioButtons("BarInput","Show Bar Chart",choices=c("Yes","No"),
                              selected = "No"),
                 textInput("yInput","Y-axis for line graph","")
    ),
    mainPanel(textOutput("nrow"),
              br(),
              plotOutput("myplot"),
              br(),
              tableOutput("mytable")
    )
  ))

server2 = function(input,output){
  output$myplot = renderPlot({
    filtered = kills_df %>%
      filter(variable %in% input$CharInput)
    if (input$BarInput =="No"){
      ggplot(filtered,aes(x=Season,y=value,group=variable)) +
        geom_point(size=2,aes(col=variable))+
        geom_line(size=1,aes(col=variable))+
        scale_x_continuous("Season", labels = c("Season 1", "Season 2", "Season 3", 
                                                "Season 4", "Season 5", "Season 6"), 
                           breaks = unique(filtered$Season)) +
        labs(col="Character")
    }
    else{
      ggplot(filtered, aes(x = Season,y = value, group = variable)) +
        geom_bar(stat = "identity", aes(fill = variable))+
        scale_x_continuous("Season", labels = c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5", "Season 6"), breaks = unique(filtered$Season))+
        labs(fill = "Character")
    }
  })
}

#can also print(ui) to see raw html, if want
shinyApp(ui = ui2 ,server = server2)
