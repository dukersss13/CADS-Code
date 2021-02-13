library(dplyr)
library(shiny)
library(reshape2)
library(ggplot2)
library(lubridate)
#library(rsconnect)

setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Homework/HW6")

load("hw6data.Rdata")
attach(medtn)

newDate = date(medtn$date)
newTime = hour(logged_at_local) + minute(logged_at_local)/60

medtn = cbind(medtn, newDate, newTime)

ui = fluidPage(
  titlePanel("Meditation Data"),
  sidebarLayout(
    sidebarPanel(selectInput("IDInput", "Participants ID",
                              choices = unique(medtn$ID),
                             selected = "1"),
                 uiOutput("DateOutput")),
  
    mainPanel(plotOutput("plot", brush = brushOpts("plotBrush")),
            textOutput("plotBrush_info"),
            textOutput("favorite"))
  )
)

server = function(input, output){
  
  filtered = reactive({
    medtn %>%
      filter(ID %in% input$IDInput)
    })
  
  output$plot = renderPlot({
    ggplot(filtered(), aes(x = date(date), y = hour(logged_at_local) + minute(logged_at_local)/60)) +
      geom_point(size = 3, aes(shape = session_type, color = duration_in_sec/60)) +
      scale_y_continuous(breaks = seq(0,24,4),
                         labels=c("12AM","4AM","8AM","12PM","4PM","8PM","12AM")) +
      scale_color_gradient(low = "yellow", high = "red", "Duration (min)") +   
      scale_shape_manual(values=c(16,17,15,3,7,8,10), "Activity Type") +
      labs(x = "Date", y = "Start Time") 

 })
  output$DateOutput = renderUI({dateRangeInput("DateInput", "Date Range",
                                     start  = date(min(filter(medtn, ID %in% input$IDInput)$date)),
                                     end    = date(max(filter(medtn, ID %in% input$IDInput)$date)),
                                     format = "yyyy-mm-dd",
                                     separator = " to ")
    })
  output$plotBrush_info = renderText({
    brushedData = brushedPoints(filtered(), input$plotBrush, xvar = "newDate", yvar = "newTime")
    
    if(is.null(brushedData)) return("Please click and drag on the graph to select data.")
    
    paste0("There are ", nrow(brushedData), " observations selected. The average duration is ",
           round(mean(brushedData$duration_in_sec)/60,2), " mins.")

  })
  output$favorite = renderText({
    paste0("Participant ", unique(filtered()$ID), "'s favorite program is ", max(filtered()$program_title),".")
  })
}

shinyApp(ui = ui, server = server)

