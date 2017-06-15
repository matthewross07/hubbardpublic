library(shiny)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(rio)
library(stringr)
library(plotly)
library(ggthemes)
library(devtools)


#DATA

imported_data <- readRDS("precip_stream_data_long.rds")

my_theme <- theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        text = element_text(family = "Helvetica", size = 20), 
        legend.position = "right", legend.direction = "vertical", legend.title = element_blank(),
        strip.text = element_text(hjust = 1, size = 30, face = "bold"), 
        axis.title= element_text(NULL), axis.title.x= element_blank(), 
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

my_palette <- c("#EAB641", "#8BC057", "#19BA94","#25A6C0", "#8D85BA","#BC6487")


ui <- fluidPage(theme = "exploratory.css",
  tags$head(tags$style(HTML(
      "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
  fluidRow(
    h1("Water Chemistry Exploration"),
    column(5, h2("I am interested in data from a")),
    column(6, selectInput(inputId = "filter",
                          label = "", 
                          choices = c("Watershed", "Solute", "Water Source"),
                          selected = "Watershed")
  )),
  
  fluidRow(
    column(2, h2("specifically,")),
    column(6, 
           conditionalPanel(condition = "input.filter == 'Watershed'", 
                            selectInput(inputId = "filterws",
                                        label = "",
                                        choices= c("1","2", "3","4", "5","6", "7","8","9"),
                                        selected = "6")),
           
            conditionalPanel(condition = "input.filter == 'Solute'", 
                             selectInput(inputId = "filtersolute",
                                         label = "",
                                         choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                         selected = "Ca")),
           conditionalPanel(condition = "input.filter == 'Water Source'", 
                            selectInput(inputId = "filtersource",
                                        label = "",
                                        choices= c("flow","precip"),
                                        selected = "precip")))),
  
  fluidRow(
    column(10,
           tabsetPanel( 
             tabPanel("view1", plotOutput("view1")), 
             tabPanel("view2", plotOutput("view2")))
           ),
    column(2,
           conditionalPanel(condition = "input.filter == 'Watershed' || input.filter == 'Solute'",
                              checkboxGroupInput(inputId = "source",
                              label = "Choose source to display",
                              choices= c("flow","precip"),
                              selected = "precip")
          ),

          conditionalPanel(condition = "input.filter == 'Solute' || input.filter == 'Water Source'",
                           checkboxGroupInput(inputId = "ws",
                                              label = "Choose ws to display",
                                              choices= c("1","2", "3","4", "5","6", "7","8","9"),
                                              selected = "6")),

          conditionalPanel(condition = "input.filter == 'Water Source' || input.filter == 'Watershed'",
                           checkboxGroupInput(inputId = "solute", label = "Choose solutes to display",
                                              choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                              selected = "Ca")),
           
           selectInput(inputId = "granularity",
                       label = "Granularity", 
                       choices = c("Month", "Year"),
                       selected = "Year"),
          selectInput(inputId = "units",
                      label = "Concentration Units", 
                      choices = c("uEquivalent/L","uMole/L", "uMg/L", "flux"),
                      selected = "uEquivalent/L"),
           sliderInput("timeframe", label = "Select time frame",
                min = as.Date("1963/06/01"), 
                max = as.Date("2013/06/01"), 
                # step = 30, #is there a way to make the steps go by month?
                value = c(as.Date("1963/06/01"), as.Date("2013/06/01"))),
          checkboxInput("logscale", label = "log", value = FALSE),  
          actionButton(inputId = "update", label = "update"))
 ))


server <- function(input, output) {



  filter <- eventReactive(input$update, {input$filter})
  
  reactivedata <- eventReactive(input$update,{
    data <- imported_data
    if(input$filter == "Watershed"){
      data <- data[data$source %in% input$source,]
      data <- data[data$solute %in% input$solute,]
      data <- data[data$ws %in% input$filterws,]
      data
    }
    else if(input$filter == "Water Source"){
      data <- data[data$source %in% input$filtersource,]
      data <- data[data$solute %in% input$solute,]
      data <- data[data$ws %in% input$ws,]
      data
    }
    else{
      data <- data[data$source %in% input$source,]
      data <- data[data$solute%in% input$filtersolute,]
      data <- data[data$ws %in% input$ws,]
      data
    }
  })
  
  
  x <- reactive({
    if(input$granularity == "Month"){"water_date"}
    else if(input$granularity == "Year"){"water_year"}
  })
  
  y <- reactive({
    if(input$granularity == "Month" & input$units =="uMg/L"){"concentration_mg"}
    else if(input$granularity == "Year" & input$units =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity == "Month" & input$units =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity == "Year" & input$units =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity == "Month"& input$units =="uMole/L"){"concentration_umol"}
    else if(input$granularity == "Year"& input$units =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity == "Month"& input$units =="flux"){"flux"}
    else if(input$granularity == "Year"& input$units =="flux"){"flux_weighted_average"}
  })
  
  variable_a <- reactive({
    if(filter() == "Watershed"){"source"}
    else if(filter() == "Solute"){"ws"}
    else{"solute"}
  })

  variable_b <- reactive({
    if(filter() == "Watershed"){"solute"}
    else if(filter() == "Solute"){"source"}
    else{"ws"}
  })
  
  
  
  
  output$view1 <- renderPlot({
    
      if(input$logscale){
        ggplot(data=reactivedata())+ my_theme +
          geom_line(aes(x = get(x()), y = log(get(y())),color = get(variable_a())), size = 2)+ 
          geom_point(aes(x = get(x()), y = log(get(y())), color = get(variable_a())), size = 3) + facet_wrap(~get(variable_b()) , ncol = 1)+ 
          xlim(min(input$timeframe[1]), max(input$timeframe[2])) +
          labs(x = "Water Year", y = paste("log", "(",input$units, ")")) + 
          scale_shape_manual(values = c(16, 1)) + scale_color_manual(values = my_palette)}
      
      else{
        ggplot(data=reactivedata())+ my_theme +
          geom_line(aes(x = get(x()), y = get(y()),color = get(variable_a())), size = 2)+ 
          geom_point(aes(x = get(x()), y = get(y()), color = get(variable_a())), size = 3) + facet_wrap(~get(variable_b()) , ncol = 1) + 
          xlim(min(input$timeframe[1]), max(input$timeframe[2])) +
          labs(x = "Water Year", y = input$units) + 
          scale_shape_manual(values = c(16, 1)) + scale_color_manual(values = my_palette)}
    
  
  })
  
  
  output$view2 <- renderPlot({
    if(input$logscale){
      ggplot(data=reactivedata())+ my_theme +
        geom_line(aes(x = get(x()), y = log(get(y())), color = get(variable_b())), size = 2)+ 
        geom_point(aes(x = get(x()), y = log(get(y())), color = get(variable_b())), size = 3)+ 
        facet_wrap(~get(variable_a()) , ncol = 1)+ 
        xlim(min(input$timeframe[1]), max(input$timeframe[2])) +
        labs(x = "Water Year", y = paste("log", "(",input$units, ")")) + 
        scale_shape_manual(values = c(16, 1)) + scale_color_manual(values = my_palette)}
      
    else{
      ggplot(data=reactivedata())+ my_theme +
        geom_line(aes(x = get(x()), y = get(y()), color = get(variable_b())), size = 2)+ 
        geom_point(aes(x = get(x()), y = get(y()), color = get(variable_b())), size = 3)+ 
        facet_wrap(~get(variable_a()) , ncol = 1)+ 
        xlim(min(input$timeframe[1]), max(input$timeframe[2])) +
        labs(x = "Water Year", y = input$units) + 
        scale_shape_manual(values = c(16, 1)) + scale_color_manual(values = my_palette)}
    })
  
}

shinyApp(ui = ui, server = server)