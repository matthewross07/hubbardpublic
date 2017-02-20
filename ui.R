# Tab layout and sidebar panels for a shiny app that helps user


library(shiny)
library(xts)
library(dygraphs)
ws <- c(1:9)
elements <- c('Ca','Mg','K','Na','Al','NH4','SO4','NO3','Cl','PO4','SiO2','H')


shinyUI(fluidPage(navbarPage(
  'Hubbard Brook Long Term Data',
  tabPanel('Intro',
             fluidRow(column(6,
             h3('Welcome to the Hubbard Brook Data Explorer web application!'),
             br(),
             br(),
             h5('We are excited to announce that we have been selected to participate in the', 
                a('Data+ REU Program' , href='http://bigdata.duke.edu/data'), 'We will mentor a team of three undergraduate
                data scientists in a ten-week summer program to expand this data visualization platform. Stay tuned for exciting new data visualizations from the longest continuous record of stream and precipitation chemistry in the world!'),
            br(),
            br(),
            h5('In the interim, please click the tabs above to see what we are brewing up!'),
            br(),
            br(),
            h5('Contributors: Richard Marinos, Matt Ross, Aaron Berdanier, Emily Bernhardt'),
            br()      
                   
           ),
           column(
             6,
            img(src='http://bigdata.duke.edu/sites/bigdata.duke.edu/files/HBEFBigData_2.jpg')
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
