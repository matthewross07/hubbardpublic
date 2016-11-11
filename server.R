library(shiny)
library(xts)
library(dygraphs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(tidyr)
library(devtools)
library(ggplot2)
#devtools::install_github("hadley/ggplot2",force=T)

load('public_data.RData')

# The palette with grey:


chem.annual$yr <- ymd(paste(chem.annual$water.year,'01-01',sep='-'))
hydro.annual$ws <- as.numeric(hydro.annual$ws)
elements <- c('Ca','Mg','K','Na','H','Al','NH4','PO4','Cl','SO4','NO3')
mr.cols <- rev(brewer.pal(length(elements),name='RdBu'))
df.cols <- data.frame(el=elements,col=mr.cols,stringsAsFactors = F)
df.cols$el <- factor(df.cols$el)


# Server that grabs separate shiny apps. 
shinyServer(function(input, output) {

  dstabs = gsub(".*/(.*)","\\1",list.dirs("Data Stories")[-1])
  # lapply(dstabs, function(nm) source(paste0("Data Stories/",nm,"/server.R"),local=TRUE) )
  myTabs = lapply(dstabs, function(nm){
    source(paste0("Data Stories/",nm,"/server.R"),local=TRUE)
    tabPanel(nm, br(), source(paste0("Data Stories/",nm,"/ui.R"),local=T))
  })
  output$datastories = renderUI(do.call(tabsetPanel, c(myTabs,type="pills")) )
  
  source("Data exploration/explore.server.R", local=T)
})
