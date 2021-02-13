library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(ggcorrplot)

#setwd("C:/Users/520ha/Desktop/Chapman/2020 Fall/CS 614/Assignments/Group Module 2")
#setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Class Work/Group Module 2")

# Data Processing ####
data = read.csv("UOF.csv")

# Select county and all percentage columns:
data = data %>%
  select(county, starts_with('pct_')) %>%
  select(-c("pct_officers_injured", "pct_subjects_injured"))

# Rename the percentage columns:
for(i in 2:dim(data)[2]){
  split_names = str_split(names(data)[i],'_', simplify = TRUE)
  if(length(split_names)==3){
    names(data)[i] = paste("Percent", str_to_title(split_names[1,2]),
                         str_to_title(split_names[1,3]))
  } else{
    names(data)[i] = paste("Percent", str_to_title(split_names[1,2]))
  }
}
    
data[is.na(data)] = 0     
attach(data)

# UI ####
#if (interactive()) {
  ui = fluidPage(titlePanel("New Jersey Police Data"),
    sidebarLayout(
      sidebarPanel(radioButtons("corrInput","Show Correlation Graph", 
                                choices = c("Yes","No"), selected = "No"),
                   conditionalPanel(condition = "input.corrInput == 'No'",
                                    uiOutput("countyOutput1"),
                                    uiOutput("countyOutput2")
                                    ),
                   conditionalPanel(condition = "input.corrInput == 'Yes'",
                                    uiOutput("countyOutput")),
                   uiOutput("percentOutput1"),
                   uiOutput("percentOutput2")),
        
      mainPanel(conditionalPanel(
                    condition = "input.corrInput == 'No'",
                    #textOutput('kendall'),
                    #br(),
                    plotOutput("plot1"),
                    br(),
                    textOutput('t_test1'),
                    br(),
                    plotOutput("plot2"),
                    br(),
                    textOutput('t_test2'),
                    br()),
                  conditionalPanel(
                    condition = "input.corrInput == 'Yes'",
                    plotOutput("correlation"),
                    br(),
                    br(),
                    plotOutput("corrMatrix"))
                  )
    )
  )
#}

# Server ####
server = function(input, output) {
  filtered1 = reactive ({
    if(is.null(input$countySelect1)) return(NULL)
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
    data %>%
      filter(county %in% input$countySelect1) %>%
      select(county, input$percentSelect1, input$percentSelect2)
    
  })
  
  filtered2 = reactive ({
    if(is.null(input$countySelect2)) return(NULL)
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
    data %>% 
      filter(county %in% input$countySelect2)%>%
      select(county, input$percentSelect1, input$percentSelect2)
  })
  
  consolidate.df = reactive ({
    rbind(filtered1(), filtered2())
  })
  
  filtered3 = reactive({
    if(is.null(input$countySelect3)) return(NULL)
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
    data %>%
      filter(county %in% input$countySelect3) %>%
      select(county, input$percentSelect1, input$percentSelect2)
  })
# countySelect1 ####  
  output$countyOutput1 = renderUI ({
      selectInput("countySelect1", "Select County 1",
                choices = sort(unique(county)), selected = 'Atlantic')
  })
# countySelect2 ####  
  output$countyOutput2 = renderUI({
    dropdown2 = data %>% filter(!county %in% input$countySelect1)
      
    selectInput("countySelect2", "Select County 2",
                choices = sort(unique(dropdown2$county)),selected = 'Bergen')
  })
#countySelect3 ####  
  output$countyOutput = renderUI({
    selectInput("countySelect3", "Select County to See Correlation",
                choices = sort(unique(county)),selected = 'Atlantic')
  })
# percentSelect1 #### 
  output$percentOutput1 = renderUI ({
    selectInput("percentSelect1", "Select Police Use of Force",
                choices = names(data)[2:8],selected = "Percent Complaince Hold")
  })
# percentSelect2 ####  
  output$percentOutput2 = renderUI({
    selectInput("percentSelect2", "Select Civilian Use of Force",
                choices = names(data)[9:length(data)],selected = "Percent Hands Fists")
  })
# plot1 ####
  output$plot1 = renderPlot({
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
    ggplot(consolidate.df(), aes(x = consolidate.df()[,2]  , y = ..density..)) +
      geom_density(aes(color = county, fill = county), alpha = 0.2, size = 0.7) +
      labs(x = names(consolidate.df())[2], y = "Density") +
      guides(fill = guide_legend(title= "County"),
             color = guide_legend(title = "County")) + theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 11))
  })
# plot2 ####  
  output$plot2 = renderPlot({
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    if(is.null(input$countySelect2)) return(NULL)
    
    ggplot(consolidate.df(), aes(x = consolidate.df()[,3] , y = ..density..)) +
      geom_density(aes(color = county, fill = county), alpha = 0.2, size = 0.7) +
      labs(x = names(consolidate.df())[3], y = "Density") +
      guides(fill = guide_legend(title= "County"),
             color = guide_legend(title = "County")) + theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 11)) 
    
  })
# Kendall Coeff Text ####  
  output$kendall = renderText({
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    if(is.null(input$countySelect3)) return(NULL)
    
    kd = round(cor(filtered3()[,2],filtered3()[,3],method = "kendall"),4)
    paste("Kendall Rank Coefficient:", kd)
  })
# t.test1 text #### 
  output$t_test1 = renderText({
    if(is.null(input$countySelect2)) return(NULL)
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
    test = t.test(consolidate.df()[,2]~county,
                  data = consolidate.df(), paired = FALSE)
    if(test$p.value > 0.05){
      paste("The P-value of the 2-sample t-test is:", round(test$p.value,2), "which suggests
          that there is no sufficient evidence that the", input$percentSelect1, "between
            the two counties are statistically different at a significance level 
            of 0.05.")
    } else{
      paste("The P-value of the 2-sample t-test is:", round(test$p.value,2), "which suggests
          that the", input$percentSelect1, "between the two counties are 
          statistically different at a significance level of 0.05.")
    }
  })  
# t.test2 text ####  
  output$t_test2 = renderText({
    if(is.null(input$countySelect2)) return(NULL)
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    
      test = t.test(consolidate.df()[,3]~county,
                    data = consolidate.df(), paired = FALSE)
      if(test$p.value > 0.05){
        paste("The P-value of the 2-sample t-test is:", round(test$p.value,2), "which suggests
            that there is no sufficient evidence that the", input$percentSelect2, "between
              the two counties are statistically different at a significance level 
              of 0.05.")
      } else{
        paste("The P-value of the 2-sample t-test is:", round(test$p.value,2), "which suggests
            that the", input$percentSelect2, "between the two counties are 
            statistically different at a significance level of 0.05.")
      }
  })  
# Pearson Scatter Plot ####
  output$correlation = renderPlot({
    if(is.null(input$percentSelect1)) return(NULL)
    if(is.null(input$percentSelect2)) return(NULL)
    if(is.null(input$countySelect3)) return(NULL)
    
    ggplot(filtered3(), aes(x = filtered3()[,2], y = filtered3()[,3])) +
      geom_point(size = 3, color = "#56B4E9") + 
      geom_smooth(method = "lm", se = F, size = 0.8, color = "black") +
      labs(title = paste("Police-Civilian Use of Force in", input$countySelect3),
           subtitle = paste("with Pearson Correlation:", round(cor(filtered3()[,2],filtered3()[,3]),4)),
           x = names(filtered3()[2]), y = names(filtered3()[3])) + 
      theme_bw() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5), 
            plot.subtitle = element_text(size = 13, hjust = 0.5))
  })
# Kendall Matrix ####
  output$corrMatrix = renderPlot({
    if(is.null(input$countySelect3)) return(NULL)
    
    filtered4 = data %>%
      filter(county %in% input$countySelect3) %>%
      select(-county)
    
    filtered4 = filtered4[, colSums(filtered4 != 0) > 0]
    
    ggcorrplot(cor(filtered4[1:as.integer(length(filtered4)/2)], 
                   filtered4[as.integer(length(filtered4)/2+1):length(filtered4)], 
                             method = "kendall"), 
               hc.order = T, lab= T, 
               ggtheme = ggplot2::theme_light, outline.color = "white", 
               title = paste("Police(x) - Civilian(y) Violence in", input$countySelect3, 
                             "\n (Kendall Rank Correlation Matrix)"),
               colors = c("#6D9EC1", "white", "#E46726"), 
               legend.title = "Kendall Correlation") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)
