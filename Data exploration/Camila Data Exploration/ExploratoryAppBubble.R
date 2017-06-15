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
library(magrittr)
library(animation)
library(gganimate)

#DATA

imported_data <- readRDS("precip_stream_data_wide.rds")

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
                  column(10,
                         tabsetPanel( 
                           tabPanel("view1", plotOutput("view1")), 
                           tabPanel("view2", plotOutput("view2")))
                  ),
                  column(2,
                        checkboxGroupInput(inputId = "source",
                                            label = "",
                                            choices= c("precip", "flow"),
                                            selected = c("precip")),
                        selectInput(inputId = "xaxis", label = "Choose solutes to display x",
                                           choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                           selected = "Ca"),
                        selectInput(inputId = "yaxis", label = "Choose solutes to display y",
                                            choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                            selected = "Ca"),
                        checkboxGroupInput(inputId = "ws",
                                     label = "",
                                     choices= c("1","2","3","4","5","6","7","8","9"),
                                     selected = "6"),
                         selectInput(inputId = "granularity",
                                     label = "Granularity", 
                                     choices = c("Month", "Year"),
                                     selected = "Year"),
                         selectInput(inputId = "units",
                                     label = "Concentration Units", 
                                     choices = c("uEquivalent/L" = "^concentration_ueq_","uMole/L" = "^concentration_umol_", "uMg/L" = "^concentration_mg_", "flux" = "^flux_"),
                                     selected = "uEquivalent/L"),
                         sliderInput("timeframe", label = "Select time frame",
                                     min = as.Date("1963/06/01"), 
                                     max = as.Date("2013/06/01"), 
                                     # step = 30, #is there a way to make the steps go by month?
                                     value = c(as.Date("1963/06/01"), as.Date("2013/06/01"))),
                        sliderInput("speed", label = "Select speed",
                                    min = 0.1, 
                                    max = 1, 
                                    step = 0.3, #is there a way to make the steps go by month?
                                    value = 0.4, 
                                    ticks = FALSE),
                         checkboxInput("logscale", label = "log", value = FALSE),  
                         actionButton(inputId = "update", label = "update"))
                ))


server <- function(input, output) {
  
  
  
  reactivedata <- eventReactive(input$update,{
    data <- imported_data
    data <- data[data$ws %in% input$ws,]
    data <- data[data$source %in% input$source,]
    unit_columns <-colnames(data_test_bubble[,(grep(input$units, colnames(data))), with = FALSE])
    basic_columns <- c("ws","date","water_date","water_year","source","water_mm_pm")
    needed_columns <- c(unit_columns,basic_columns)
    data <- data[,needed_columns, with = FALSE]
    #data <- data[water_date %between% c(input$timeframe[1], input$timeframe[2])]
  })
  
  
  x <- eventReactive(input$update,{
    colnames(reactivedata()[,grep(input$xaxis, names(reactivedata())), with = FALSE])
  })
  
  y <- eventReactive(input$update,{
    colnames(reactivedata()[,grep(input$yaxis, names(reactivedata())), with = FALSE])
  })
  
  
  
  output$view1 <- renderPlot({

   ggplot(data=reactivedata()) + 
      geom_point(aes(x = get(x()), y = get(y()), shape = source, size = water_mm_pm, color = ws), stroke = 2)+
      scale_shape_manual(values= c(1, 16))
  
  
    
  })
  
  output$view2 <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      p <- ggplot(data=reactivedata()) + 
        geom_point(aes(x = get(x()), y = get(y()), shape = source, size = water_mm_pm, color = ws, frame = water_year), stroke = 2)+
        scale_shape_manual(values= c(1, 16))
      
      gganimate(p,"outfile.gif", ani.width = 600, ani.height = 450, interval=input$speed)
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternaate text"
      )}, deleteFile = TRUE)}

shinyApp(ui = ui, server = server)