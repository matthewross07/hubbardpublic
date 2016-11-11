# Tab layout and sidebar panels for a shiny app that helps user


library(shiny)
library(xts)
library(dygraphs)
ws <- c(1:9)
elements <- c('Ca','Mg','K','Na','Al','NH4','SO4','NO3','Cl','PO4','SiO2','H')


shinyUI(fluidPage(navbarPage(
  'Hubbard Brook Long Term Data',
  tabPanel('Intro',
           fluidRow(column(
             6,
             h5('This shiny app is currently under construction. Look at the
                "data exploration" tab or "acid rain" tab under "data stories" to see a sample of what is to come')
           ),
           column(
             6,
             h6('')
           ))),
  tabPanel('Data Stories',
    # headers, dynamic based on the number of data stories?
    uiOutput('datastories')
  ),
  tabPanel('Data Exploration',
    source("Data exploration/explore.ui.R",local=T)
  ),

  tabPanel('Suggest a Graph Gallery')
)))
