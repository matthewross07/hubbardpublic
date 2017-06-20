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
library(ggiraph)
library(grid)

#read in the data
precip_dis <- readRDS("precip_stream_data.rds")

#Add columns of the natural logarithm of relevant values
ln_concentration_ueq <- as.data.frame(log(precip_dis$concentration_ueq))
ln_ueq_weighted_average <- as.data.frame(log(precip_dis$ueq_weighted_average))
ln_flux <- as.data.frame(log(precip_dis$flux))
ln_flux_sum <- as.data.frame(log(precip_dis$flux_sum))

precip_dis <- cbind(as.data.frame(precip_dis), ln_concentration_ueq,
                    ln_ueq_weighted_average, ln_flux,
                    ln_flux_sum)
colnames(precip_dis) <- c("ws", "date", "water_date", "water_year", "solute",
                          "concentration_mg", "source", "water_mm_pm", "MW", "z",
                          "concentration_ueq", "concentration_umol", "flux",
                          "mg_weighted_average", "umol_weighted_average",
                          "ueq_weighted_average","flux_sum",
                          "ln_concentration_ueq", "ln_ueq_weighted_average",
                          "ln_flux", "ln_flux_sum")


#Write a function that converts the Source code from precip and 
#flow to Precipitation and Streamwater Discharge
source_change <- function(df){
  df$source <- ifelse(df$source == "precip", "Precipitation",
                      "Discharge")
  return(df)
}

# a function that formats the data to use in the plot 
#based on the user input parameters
format_data <- function(df, watersheds, ion, precipitation, 
                        c.units, t.scale){
  df <- as.data.frame(df)
  df1 <- source_change(df)
  df2 <- df1[df1$ws %in% watersheds,]
  df3 <- filter(df2, solute == ion)
  if (precipitation == "precip"){
    df4 <- df3
  }else{
    df4 <- filter(df3, source == "Discharge") 
  }
  if (c.units == "Eq/ha-yr"){
    if (t.scale == "year"){
      df4$value = df4$flux_sum
      df4$date.st = paste(df4$water_year)
    }else{
      df4$value = df4$flux
      df4$date.st = paste(df4$water_date)
    }
  }else if (c.units == "ueq/L"){
    if (t.scale == "year"){
      df4$value = df4$ueq_weighted_average
      df4$date.st = paste(df4$water_year)
    }else{
      df4$value = df4$concentration_ueq
      df4$date.st = paste(df4$water_date)
    }
  }else if (c.units == "ln(ueq/L)"){
    if (t.scale == "year"){
      df4$value = df4$ln_ueq_weighted_average
      df4$date.st = paste(df4$water_year)
    }else{
      df4$value = df4$ln_concentration_ueq
      df4$date.st = paste(df4$water_date)
    }
  }else{
    if (t.scale == "year"){
      df4$value = df4$ln_flux_sum
      df4$date.st = paste(df4$water_year)
    }else{
      df4$value = df4$ln_flux
      df4$date.st = paste(df4$water_date)
    }
  }
 df5 <- select(df4, ws, water_date, water_year,
               date.st, source, solute, value) 
 colnames(df5) = c("ws","water.date", "water.year",
                   "date", "Source", "solute",
                   "value")
 df5$ws = paste("Watershed", df5$ws, sep = " ")
 return(df5)
}

#a function used to generate the ggplot grob based on the user inputs
df_ggplot <- function(df, timescale, date.input, y.lab, title.lab, addprecip, ws){
  if (length(ws) %in% c(1,2,3)){
    col = 1
  }else if (length(ws) == 4){
    col = 2
  }else if (length(ws) == 5){
    col = 1
  }else if (length(ws) == 6){
    col = 2
  }else if (length(ws) == 7){
    col = 1
  }else if (length(ws) == 8){
    col = 2
  }else if (length(ws) == 9){
    col = 3
  }
  if (addprecip == "precip"){
    if (timescale == "month"){
      p = ggplot(df, aes(x = water.date, y = value, color = Source, label = date)) +
        geom_line() +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
    }else{
      p = ggplot(df, aes(x = water.year, y = value, color = Source, label = date)) +
        geom_line() + 
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)
        
    }
  }else{
    if (timescale == "month"){
      p = ggplot(df, aes(x = water.date, y = value, label = date)) +
        geom_line(color = "red") +
        geom_point(color = "blue") +
        geom_smooth(color = "green", method = "lm", se = FALSE)
    }else{
      p = ggplot(df, aes(x = water.year, y = value, label = date)) +
        geom_line(color = "red") +
        geom_point(color = "blue") +
        geom_smooth(color = "green", method = "lm", se = FALSE)
    }
  }
  if (length(ws) > 1){
    plot.df <- p +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = col)
  }else{
    plot.df <- p +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)
  }
  return(plot.df)
}


shinyServer(function(input, output) {
  
  #a reactive data set for plotting
  solute.data <- reactive({
    format_data(df = precip_dis,
                watersheds = input$watershed,
                ion = input$solute,
                precipitation = input$p,
                c.units = input$units,
                t.scale = input$scale)
  })
  
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
  
  #the plot output
  output$splot <- renderPlotly({
    len <- length(input$watershed)
    if (len == 1){
      l = 300
    }else if (len == 2){
      l = 300
    }else if (len == 3){
      l = 400
    }else if (len == 4){
      l = 400
    }else if (len == 5){
      l = 600
    }else if (len == 6){
      l = 600
    }else if (len == 7){
      l = 700
    }else if (len == 8){
      l = 700
    }else{
      l = 700
    }
    y = input$units
    if (input$units == "ueq/L"){
      title = paste(sol(), "Concentration", sep = " ")
    }else if (input$units == "ln(ueq/L)"){
      title = paste("Natural Log of ", sol(), "Concentration", sep = " ")
    }else if (input$units == "Eq/ha-yr"){
      title = paste(sol(), "Flux", sep = " ")
    }else{
      title = paste("Natural Log of", sol(), "Flux", sep = " ")
    }
    ggplotly(
      df_ggplot(solute.data(), timescale = input$scale,
                date.input = input$dates,
                y.lab = y,
                title.lab = title,
                addprecip = input$p,
                ws = input$watershed), tooltip = c("y", "label"),
      height = l, width = 500
    )
    })
  
  })