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

#Apply the flux function to the data set
monthly.ueq <- flux(monthly.ueq)

#Input the watershed labels as "Watershed 1", "Watershed 2", etc.
#for facetting later
monthly.ueq$ws <- paste("Watershed", monthly.ueq$ws, sep = " ")

#Ensure that the water date of monthly.ueq is in date format
monthly.ueq$water.date <- as.Date(monthly.ueq$water.date)

#Create a data frame with just the discharge data
discharge.ws.2.6 <- monthly.ueq %>%
  select(ws, water.date, Q) %>%
  filter(ws == c("Watershed 2", "Watershed 6"))

#Ensure theat the water date of the discharge data frame is in date format
discharge.ws.2.6$water.date <- as.Date(discharge.ws.2.6$water.date)

#Add a "date" column to the discharge data frame that copies the date as a string
discharge.ws.2.6$date <- paste(discharge.ws.2.6$water.date)

#Name the columns of the discharge data frame according to 
#what you want to see when you hover over a point in plotly
colnames(discharge.ws.2.6) <- c("ws", "water.date", "value", "date")

#create a new data frame populated by the data from chem.annual.ueq
#that will be manipulated
annual.ueq <- chem.annual.ueq

#For the annual chemistry data on discharge, add a "water.date" column
#that gives the full date in terms of the water year
annual.ueq$water.date <- as.Date(annual.ueq$datetime) %m-% months(5)

#Input the watershed labels as "Watershed 1", "Watershed 2", etc.
#for facetting later
annual.ueq$ws <- paste("Watershed", annual.ueq$ws, sep = " ")

#Ensure that the water date of annual.ueq is in date format
annual.ueq$water.date <- as.Date(annual.ueq$water.date)

#Filter monthly.ueq by watershed to return only data from Watershed 2
monthly.ueq.ws2 <- filter(monthly.ueq, ws == "Watershed 2")

#Filter monthly.ueq by watershed to output only data from Watershed 6
monthly.ueq.ws6 <- filter(monthly.ueq, ws == "Watershed 6")

#Separate the monthly.ueq.ws2 by water.date into water year, year.w
#water month, month.w, and water day, day.w
monthly.ueq.sep.ws2 <- separate(monthly.ueq.ws2, col = "water.date",
                                into = c("year.w", "month.w", "day.w"),
                                sep = "-")

#Compute the annual discharge for Watershed 2 by summing up the discharge grouped 
#by water year, year.w
annual.discharge.ws2 <- monthly.ueq.sep.ws2 %>%
  group_by(year.w) %>%
  summarize(value = sum(Q))


#reformat annual.discharge by adding a row for 1962
extra.row = c(1962, NA)

annual.discharge.ws2 <- rbind(annual.discharge.ws2, extra.row)

annual.discharge.ws2 <- arrange(annual.discharge.ws2, year.w)

#Separate the monthly.ueq.ws6 by water.date into water year, year.w
#water month, month.w, and water day, day.w
monthly.ueq.sep.ws6 <- separate(monthly.ueq.ws6, col = "water.date",
                                into = c("year.w", "month.w", "day.w"),
                                sep = "-")

#Compute the annual discharge for Watershed 2 by summing up the discharge grouped 
#by water year, year.w
annual.discharge.ws6 <- monthly.ueq.sep.ws6 %>%
  group_by(year.w) %>%
  summarize(value = sum(Q))


#reformat annual.discharge by adding a row for 1962
extra.row = c(1962, NA)

annual.discharge.ws6 <- rbind(annual.discharge.ws6, extra.row)

annual.discharge.ws6 <- arrange(annual.discharge.ws6, year.w)

#Write a function that adds the flux of each ion to the data frame called data
#using the discharge data frame
flux.annual <- function(data, discharge){
  Na_flux <- data$Na * discharge$value/100
  Mg_flux <- data$Mg * discharge$value/100
  Ca_flux <- data$Ca * discharge$value/100
  K_flux <- data$K * discharge$value/100
  SO4_flux <- data$SO4 * discharge$value/100
  NO3_flux <- data$NO3 * discharge$value/100
  Cl_flux <- data$Cl * discharge$value/100
  H_flux <- data$H * discharge$value/100
  return(cbind(data, Na_flux, Mg_flux, Ca_flux, K_flux, 
               SO4_flux, NO3_flux, Cl_flux, H_flux))
}

#Add the fluxes of solutes for Watershed 2
annual.ueq.ws2 <- filter(annual.ueq, ws == "Watershed 2")
annual.ueq.ws2 <- flux.annual(annual.ueq.ws2, annual.discharge.ws2)

#Add the fluxes of solutes for Watershed 6
annual.ueq.ws6 <- filter(annual.ueq, ws == "Watershed 6")
annual.ueq.ws6 <- flux.annual(annual.ueq.ws6, annual.discharge.ws6)

#Make a combined data frame containing data from Watersheds 2 and 6
annual.ueq.ws.2.6 <- rbind(annual.ueq.ws2, annual.ueq.ws6)

#Attach a column indicating watershed to annual.discharge.ws2
ws = rep("Watershed 2", ncol(annual.discharge.ws2))
annual.discharge.ws2 = cbind(annual.discharge.ws2, ws)

#Attach a column indicating watershed to annual.discharge.ws6
ws = rep("Watershed 6", ncol(annual.discharge.ws6))
annual.discharge.ws6 = cbind(annual.discharge.ws6, ws)

#Make a combined data frame for discharge amounts
#containing data from Watersheds 2 and 6
annual.discharge.ws.2.6 <- rbind(annual.discharge.ws2, annual.discharge.ws6)

#Rename columns of annual.discharge.ws.2.6 to match discharge.ws.2.6
colnames(annual.discharge.ws.2.6) = c("water.date", "value", "ws")

#Paste -01-01 to the end of the water.date of 
#annual.discharge.ws.2.6 to get this data frame in the same format 
#as discharge.ws.2.6
annual.discharge.ws.2.6$water.date = paste(annual.discharge.ws.2.6$water.date,
                                           "01-01", sep = "-")
#Set water.date to a date format
annual.discharge.ws.2.6$water.date = as.Date(annual.discharge.ws.2.6$water.date)

#Add a "date" column to annual.discharge.ws.2.6 for the purpose
#of supplying data to the plotly hover function
annual.discharge.ws.2.6$date = as.character(paste(annual.discharge.ws.2.6$water.date))

#Function to filter and format original data frame to
#make it work as a reactive value for a specific ion and 
#watershed
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
      discharge.ws.2.6
    }else{
      annual.discharge.ws.2.6
    }
    
  })
  
  #Reactive value selecting the data set for discharge chemistry
  #by time scale, unit and solute
  solute.data <- reactive({
    if (input$scale == "month"){
      x <- format(df = monthly.ueq, 
                  watershed = "Watershed 2",
                  unit = input$units,
                  ion = input$solute)
      y <- format(df = monthly.ueq, 
                  watershed = "Watershed 6",
                  unit = input$units,
                  ion = input$solute)
      rbind(x, y)
    }else{
      x2 <- format(df = annual.ueq.ws.2.6, 
                  watershed = "Watershed 2",
                  unit = input$units,
                  ion = input$solute)
      y2 <- format(df = annual.ueq.ws.2.6, 
                  watershed = "Watershed 6",
                  unit = input$units,
                  ion = input$solute)
      rbind(x2, y2)
    }
    
  })
  
  #Plot output for water chemistry
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
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    ggplotly(ggplot(discharge.data()) +
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