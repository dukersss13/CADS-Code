library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)


setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Class Work/Activity 6")
load("twdkills.RData")

kills.long = melt(kills, id.vars = "Season", 
                  variable.name = "Characters", value.name  = "nkills")
kills.long[is.na(kills.long)] = 0

ui = fluidPage(
  titlePanel("Walking Dead"),
  sidebarLayout(
    sidebarPanel(uiOutput("CharOutput"),
      radioButtons("BarInput","Show Bar Chart", choices = c("Yes","No"),
                              selected = "No"),
                 
                 textInput("yInput","Y-axis for line graph","")),
    
    mainPanel(textOutput("nrow"),
              br(),
              plotOutput("plot"),
              br(),
              plotOutput("plot2"))
  )
)

server = function(input, output){
  filtered = reactive({
    if(is.null(input$CharInput)) return(NULL)
    
    kills.long %>% 
      filter(Characters %in% input$CharInput) })

  output$plot = renderPlot({  
    if(is.null(filtered())) {return()}
    
    if (input$BarInput == "No"){
      ggplot(filtered(), aes(x = Season, y = nkills, group = Characters)) +
        geom_line(size = 1, aes(col = Characters)) +
        geom_point(size = 1, aes(col = Characters)) +
        scale_x_continuous("Season", labels = c("Season 1", "Season 2", "Season 3", 
                                                "Season 4", "Season 5", "Season 6"), 
                           breaks = unique(kills.long$Season)) +
        labs(y = input$yInput) 
     }
})

  output$plot2 = renderPlot({
    if(is.null(filtered())) {return()}

    if (input$BarInput == "Yes"){
     ggplot(filtered(), aes(x = Season, y = nkills, group = Characters)) +
      geom_line(size = 1, aes(col = Characters)) +
      geom_point(size = 1, aes(col = Characters)) +
      scale_x_continuous("Season", labels = c("Season 1", "Season 2", "Season 3",
                                              "Season 4", "Season 5", "Season 6"),
                         breaks = unique(kills.long$Season)) +
      labs(y = input$yInput)
      
      ggplot(filtered(), aes(x = Characters, y = nkills, group = Characters)) +
        geom_bar(stat = "identity", aes(fill = Characters, col = Characters),
                 position = position_dodge()) +
        labs(y = input$yInput) +
        theme(axis.text.x = element_text(angle = 90)) +
        facet_wrap(vars(Season),labeller = labeller(Season = c("1" = "Season 1", "2" = "Season 2","3" = "Season 3", "4" = "Season 4", "5" = "Season 5", "6" = "Season 6")))
    }
  })
  
  output$nrow = renderText({
    paste0("You have selected ", length(input$CharInput), " characters.")
  })
  
  output$CharOutput = renderUI({
    checkboxGroupInput("CharInput", "Characters", 
                       choices = unique(kills.long$Characters), 
                       selected = "Rick")
  })
}

shinyApp(ui = ui, server = server)
