library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)


shinyUI(fluidPage(
  # Application title
  titlePanel("Effects of Deforestation on Streamwater"),
  
  tabsetPanel(
    tabPanel("Solute Concentrations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("solute", label = h3("Solute"),
                             choices = list("Sodium" = "Na",
                                            "Calcium" = "Ca",
                                            "Magnesium" = "Mg",
                                            "Potassium" = "K",
                                            "Sulfate" = "SO4",
                                            "Nitrate" = "NO3",
                                            "Chlorine" = "Cl",
                                            "Hydrogen Ion" = "H"),
                             selected = "Na"),
                 selectInput("units", label = h3("Units"),
                             choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                            "ueq/L" = "ueq/L"),
                             selected = "ueq/L"),
                 selectInput("scale", label = h3("Time Scale"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "month"),
                 sliderInput("dates", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1971-01-01")))),
               mainPanel(plotlyOutput("s.plot"))
             )),
    tabPanel("Discharge and Precipitation Quantities",
             sidebarLayout(
               sidebarPanel(
                 selectInput("scale.dis", label = h3("Time Scale"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "month"),
                 sliderInput("dates.dis", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1971-01-01")))),
               
               mainPanel(plotlyOutput("d.plot")))
             
             
             
    )
    
    
  )
  
  
))