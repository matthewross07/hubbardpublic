library(ggplot2)
library(lubridate)
library(gridExtra)
library(readr)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(utils)


#read in the data
precip_dis <- readRDS("precip_stream_data.rds")

#Write a function that converts the Source code from precip and 
#flow to Precipitation and Streamwater Discharge
source_change <- function(df){
  df$Source <- ifelse(df$Source == "precip", "Precipitation",
                       "Discharge")
  return(df)
}

#Create a data frame called "monthly" that contains precipitation
#and discharge quantities in the correct format by month
monthly <- precip_dis %>%
  select(ws, water_year, water_date, 
         solute, water_mm_pm, source)%>%
  filter(ws == 6 | ws == 2, solute == "Ca")
colnames(monthly) = c("ws", "water.year", "water.date",
                      "solute", "value", "Source")
monthly$ws <- paste("Watershed", monthly$ws, sep = " ")
monthly$date <- paste(monthly$water.date)
monthly <- source_change(monthly)

#Create a data frame called "yearly" that contains precipitation
#and discharge quantities in the correct format by year
yearly <- precip_dis %>%
  select(ws, water_year, water_date, 
         solute, water_mm_pm, source)%>%
  filter(ws == 6 | ws == 2, solute == "Ca") %>%
  group_by(ws, source, water_year) %>%
  summarize(value = sum(water_mm_pm))
colnames(yearly) <- c("ws", "Source", "water.year", "value")
yearly$ws <- paste("Watershed", yearly$ws, sep = " ")
yearly$date <- paste(yearly$water.year)
yearly <- source_change(yearly)


#Function to filter and format original data frame to
#make it work as a reactive value for a specific ion and 
#watershed
format <- function(df, unit, ion, date.scale){
  if (date.scale == "month"){
    if (unit == "ueq/L"){
      formatted <- df %>%
        filter(ws == 2| ws == 6, solute == ion) %>%
        select(ws, water_date, water_year, source, solute, concentration_ueq)
      colnames(formatted) = c("ws", "water.date","water.year",
                              "Source","solute","value")
      formatted$date = paste(formatted$water.date)
      formatted$ws = paste("Watershed", formatted$ws, sep = " ")
      formatted <- source_change(formatted)
      return(formatted)
    }else{
      formatted <- df %>%
        filter(ws == 2| ws==6, solute == ion) %>%
        select(ws, water_date, water_year, source, solute, flux)
      colnames(formatted) = c("ws", "water.date", "water.year",
                              "Source", "solute", "value")
      formatted$date = paste(formatted$water.date)
      formatted$ws = paste("Watershed", formatted$ws, sep = " ")
      formatted <- source_change(formatted)
      return(formatted)
    }
  }else{
    if (unit == "ueq/L"){
      formatted <- df %>%
        filter(ws == 2| ws==6, solute == ion) %>%
        select(ws, water_date, water_year, source, solute, ueq_weighted_average)
      colnames(formatted) = c("ws", "water.date", "water.year",
                              "Source", "solute", "value")
      formatted$date = paste(formatted$water.year)
      formatted$ws = paste("Watershed", formatted$ws, sep = " ")
      formatted <- source_change(formatted)
      return(formatted)
    }else{
      formatted <- df %>%
        filter(ws == 2|ws == 6, solute == ion) %>%
        select(ws, water_date, water_year, source, solute, flux_weighted_average)
      colnames(formatted) = c("ws", "water.date","water.year",
                              "Source","solute","value")
      formatted$date = paste(formatted$water.year)
      formatted$ws = paste("Watershed", formatted$ws, sep = " ")
      formatted <- source_change(formatted)
      return(formatted)
    }
  }
}

#Function to plot the formatted data frame in ggplot2
plot.formatted.df <- function(df, timescale, date.input, y.lab, title.lab){
  p <- ggplot(df,aes(x= get(timescale),y=value, color=Source, label=date)) +
    geom_line() +
    geom_point() +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
    labs(x = "Date (In Water Years)", 
         y = y.lab,
         title = title.lab)+
    facet_wrap(~ws, ncol = 1)
  return(p)
}

#Server function
shinyServer(function(input, output) {
  
  #Reactive value for the units, either in
  #concentration or flux
  unit <- reactive({input$units})
  
  #Reactive value for the type of inputted solute
  sol <- reactive({
    if (input$solute == "Na"){
      "Sodium"
    }else if (input$solute == "Ca"){
      "Calcium"
    }else if (input$solute == "Mg"){
      "Magnesium"
    }else if (input$solute == "K"){
      "Postassium"
    }else if (input$solute == "SO4"){
      "Sulfate"
    }else if (input$solute == "NO3"){
      "Nitrate"
    }else if (input$solute == "Cl"){
      "Chlorine"
    }else{
      "Hydrogen Ion"
    }
    
  })
  
  #Reactive value selecting the discharge quantity data set by the time scale,
  #annually or monthly
  discharge.data <- reactive({
    if (input$scale.dis == "month"){
      monthly
    }else{
      yearly
    }
    
  })
  
  #Reactive value selecting the data set for discharge chemistry
  #by time scale, unit and solute
  solute.data <- reactive({
    format(df = precip_dis, unit = input$units,
           ion = input$solute, date.scale = input$scale)
    
  })
  
  #Plot output for water chemistry
  output$s.plot <- renderPlotly({
    y = ifelse(input$units == "ueq/L", "ueq/L", "Eq/ha-yr")
    title = paste(sol(), ifelse(input$units == "ueq/L",
                                "Concentration", "Flux"), sep = " ")
    if (input$scale == "month"){
      s <- "water.date"
    }else{
      s <- "water.year"
    }
    ggplotly( 
      plot.formatted.df(df = solute.data(),
                        timescale = s,
                        date.input = input$dates, y.lab = y, 
                        title.lab = title),
      tooltip = c("y", "label"))
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    y = "Discharge (mm)"
    title = "Discharge by Watershed"
    if (input$scale.dis == "month"){
      s2 <- "water.date"
    }else{
      s2 <- "water.year"
    }
    ggplotly(
      plot.formatted.df(df = discharge.data(), 
                        timescale = s2,
                        date.input = input$dates.dis,
                        y.lab = y, title.lab = title),
      tooltip= c("y", "label"))
    
    
    
  })
  
})