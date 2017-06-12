library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)

#load in the data
load("public_data.Rdata")

#create a new data frame populated by the data from chem.monthly.ueq
#that will be manipulated
monthly.ueq <- chem.monthly.ueq

#For the monthly data on discharge, add a "water.date" column
#that gives the full date in terms of the water year
monthly.ueq$water.date <- as.Date(monthly.ueq$datetime) %m-% months(5)

#Create a new data frame with the water date separated into year, month, and day
#in addition to the other entries
monthly.ueq.sep <- separate(monthly.ueq, col = "water.date", 
                            into = c("Year", "Month", "Day"), sep = "-")

#Paste the year and the month together from the previous data frame so that
#the water year and month form a new column, water_year_month
monthly.ueq$water_year_month <- paste(monthly.ueq.sep$Year,
                                      monthly.ueq.sep$Month,
                                      sep = "-")

#Create a new data frame that contains just the discharge data from Watershed 2
monthly.ueq.ws2 <- filter(monthly.ueq, ws == 2)

#Make a new data frame that contains just the discharge data from Watershed 6
monthly.ueq.ws6 <- filter(monthly.ueq, ws == 6)

#Write a function that adds the flux of each ion to the data frame called data
flux <- function(data){
  Na_flux <- data$Na * data$Q/100
  Mg_flux <- data$Mg * data$Q/100
  Ca_flux <- data$Ca * data$Q/100
  K_flux <- data$K * data$Q/100
  SO4_flux <- data$SO4 * data$Q/100
  NO3_flux <- data$NO3 * data$Q/100
  Cl_flux <- data$Cl * data$Q/100
  H_flux <- data$H * data$Q/100
  return(cbind(data, Na_flux, Mg_flux, Ca_flux, K_flux, 
               SO4_flux, NO3_flux, Cl_flux, H_flux))
}

#Apply the flux function to the data sets for Watersheds 2 and 6
monthly.ueq.ws2 <- flux(monthly.ueq.ws2)
monthly.ueq.ws6 <- flux(monthly.ueq.ws6)

#Input the watershed labels as "Watershed 2" and "Watershed 6"
#instead of just 2 and 6 for facetting later
monthly.ueq.ws2$ws <- paste("Watershed", monthly.ueq.ws2$ws, sep = " ")
monthly.ueq.ws6$ws <- paste("Watershed", monthly.ueq.ws6$ws, sep = " ")

#Bind the two data frames for Watersheds 2 and 6 together for facetting
monthly.ueq.ws.2.6 <- rbind(monthly.ueq.ws2, monthly.ueq.ws6)

#Ensure that the water date of monthly.ueq.ws.2.6 is in date format
monthly.ueq.ws.2.6$water.date <- as.Date(monthly.ueq.ws.2.6$water.date)

#Create a data frame with just the discharge data
discharge.ws.2.6 <- select(monthly.ueq.ws.2.6, ws, water.date, Q)

#Ensure theat the water date of the discharge data frame is in date format
discharge.ws.2.6$water.date <- as.Date(discharge.ws.2.6$water.date)

#Add a "date" column to the discharge data frame that copies the date as a string
discharge.ws.2.6$date <- paste(discharge.ws.2.6$water.date)

#Name the columns of the discharge data frame according to 
#what you want to see when you hover over a point in plotly
colnames(discharge.ws.2.6) <- c("ws", "water.date", "value", "date")

format <- function(df, watershed, unit, ion){
  if (unit == "ueq/L"){
    formatted <- df %>%
      filter(ws == watershed) %>%
      select(ws, water.date, get(ion))
    colnames(formatted) = c("ws", "water.date", "value")
    formatted$date = paste(formatted$water.date)
    return(formatted)
  }else{
    formatted <- df %>%
      filter(ws == watershed) %>%
      select(ws, water.date, get(paste(ion, "flux", sep = "_")))
    colnames(formatted) = c("ws", "water.date", "value")
    formatted$date = paste(formatted$water.date)
    return(formatted)
  }
}

shinyServer(function(input, output) {
  
  unit <- reactive({input$units})
  
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
  
  
  solute.data <- reactive({
    x <- format(df = monthly.ueq.ws.2.6, 
                watershed = "Watershed 2",
                unit = input$units,
                ion = input$solute)
    y <- format(df = monthly.ueq.ws.2.6, 
                watershed = "Watershed 6",
                unit = input$units,
                ion = input$solute)
    rbind(x, y)
    
  })
  
  output$s.plot <- renderPlotly({
    ggplotly(ggplot(solute.data()) +
               geom_line(aes(x = water.date, y = value, label = date), color = "red") +
               geom_point(aes(x = water.date, y = value, label = date), color = "blue") +
               coord_cartesian(xlim = c(as.Date(input$dates[1]), as.Date(input$dates[2])))+
               labs(x = "Date (In Water Years)", 
                    y = ifelse(input$units == "ueq/L", "ueq/L", "Eq/ha-yr"),
                    title = paste(sol(), ifelse(input$units == "ueq/L",
                                                "Concentration", "Flux"), sep = " "))+
               facet_wrap(~ws, ncol = 1), tooltip = c("y", "label"))
  })
  
  output$d.plot <- renderPlotly({
    ggplotly(ggplot(discharge.ws.2.6) +
               geom_line(aes(x = water.date, y = value, label = date), 
                         color = "red") +
               geom_point(aes(x = water.date, y = value, label = date),
                          color = "blue") +
               coord_cartesian(xlim = c(as.Date(input$dates.dis[1]), 
                                        as.Date(input$dates.dis[2]))) +
               labs(x = "Date (In Water Years)",
                    y = "Discharge (mm)",
                    title = "Discharge by Watershed")+
               facet_wrap(~ws, ncol = 1), tooltip= c("y", "label"))
    
    
    
  })
  
})