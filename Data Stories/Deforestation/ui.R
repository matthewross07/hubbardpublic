library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggiraph)


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
                                            "ueq/L" = "ueq/L",
                                            "ln(ueq/L)" = "ln(ueq/L)",
                                            "ln(Eq/ha-yr)" = "ln(Eq/ha-yr)"),
                             selected = "ueq/L"),
                 selectInput("scale", label = h3("Time Scale"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "year"),
                 selectInput("p", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"),
                 sliderInput("dates", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1971-01-01")))),
               mainPanel(plotlyOutput("s.plot"),
                         p("*The black horizontal line on the graph represents the date of 
                           the deforestation of Watershed 2."),
                         h4("Consequences of Deforestation"),
                         p("Deforestation, the removal of forest trees,
                           is harmful to the environment for a number of 
                           reasons, some of which are less obvious than others.
                           Deforestation results in habitat loss for woodland-
                           dwelling species, causing die-offs and concurrent declines
                           in biodiversity. Around eighty percent of land animals
                           and plants on Earth reside in forests (National Geographic Society
                            2017). It follows that the impact of widespread deforestation
                           on wildlife is not insignificant. Deforestation also 
                           accelerates climate change, as the loss of forests
                           that absorb carbon dioxide tips the balance 
                           so that more of this greenhouse gas enters 
                           that atmosphere, causing global warming. On
                           the smaller scale, deforestation can trigger
                           regional climate change because the ground 
                           cover from the trees is eliminated, allowing sun
                           rays to penetrate where they were previously 
                           blocked. This causes soils to dry out, which can
                           transform once forested land into deserts. 
                           Without canopy cover to block sunlight during
                           the day and retain heat during the night, 
                           temperature fluctuations become more severe
                           and harmful to wildlife. Deforestation also
                           leads to drier climates because less water 
                           is transpired, or released into the air, by
                           trees. This negatively impacts the water cycle
                           (National Geographic Society 2017)."),
                         p("Using our data set, one can visualize two more
                           consequences of deforestation- leaching 
                           of nutrients from the environment and 
                           increases in water runoff.  Trees prevent 
                           erosion by fixing soil in place with their
                           roots and creating natural dams with fallen
                           leaves and branches. When trees are removed,
                           erosion increases, carrying away the nutrients
                           in the soil. At the same time, deforestation 
                           inhibits water uptake by trees, resulting in
                           heightened runoff and more water moving through
                           the soil, dissolving and washing away more 
                           nutrients. Thus, the concentration of nutrients
                           in streamwater rises following 
                           deforestation (Likens et al. 1970)."),
                         p("At Hubbard Brook, an experiment was conducted 
                           in November and December 1965 in which all the
                           trees in Watershed 2 were cut, left in place,
                           and limbed so that no branches were more that 
                           1.5 meters high. Herbicide was applied 
                           periodically to Watershed 2 for two years 
                           after the deforestation event, so that forest
                           regrowth was prevented. Chemical and quantity
                           measurements of precipitation and streamwater
                           discharge were recorded. At the same time, these
                           measurements were taken at other Hubbard Brook 
                           watersheds, particullarly Watershed 6, which 
                           was left undisturbed (Likens et al. 1970). 
                           These graphs compare data from the deforested
                           Watershed 2 and the undisturbed Watershed 6, 
                           so you can explore the effects of deforestation yourself."),
                         h4("References"),
                         p("\"Deforestation and Its Effect on the Planet.\"", 
                           em("National Geographic."), 
                           "National Geographic Society, 24 May 2017. Web. 16 June 2017.
                           <http://www.nationalgeographic.com/environment/global-warming/deforestation/>."),
                         p("Likens, Gene E., F. Herbert Bormann, Noye M. Johnson, D. W. Fisher,
                           and Robert S. Pierce. \"Effects of Forest Cutting and Herbicide Treatment 
                           on Nutrient Budgets in the Hubbard Brook Watershed-Ecosystem.\"", 
                           em("Ecological Monographs")," 40.1 (1970): 23-47. Web. 16 June 2017.")
          
                         )
             )),
    tabPanel("Discharge and Precipitation Quantities",
             sidebarLayout(
               sidebarPanel(
                 selectInput("scale.dis", label = h3("Time Scale"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "year"),
                 selectInput("units.dis", label = h3("Units"),
                             choices = list("mm" = "mm",
                                            "ln(mm)" = "ln(mm)"),
                             selected = "mm"),
                 selectInput("p.dis", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"),
                 sliderInput("dates.dis", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1971-01-01")))),
               
               mainPanel(plotlyOutput("d.plot"),
                         p("*The black horizontal line on the graph represents the date of 
                           the deforestation of Watershed 2."),
                         h4("Consequences of Deforestation"),
                         p("Deforestation, the removal of forest trees,
                           is harmful to the environment for a number of 
                           reasons, some of which are less obvious than others.
                           Deforestation results in habitat loss for woodland-
                           dwelling species, causing die-offs and concurrent declines
                           in biodiversity. Around eighty percent of land animals
                           and plants on Earth reside in forests (National Geographic Society
                           2017). It follows that the impact of widespread deforestation
                           on wildlife is not insignificant. Deforestation also 
                           accelerates climate change, as the loss of forests
                           that absorb carbon dioxide tips the balance 
                           so that more of this greenhouse gas enters 
                           that atmosphere, causing global warming. On
                           the smaller scale, deforestation can trigger
                           regional climate change because the ground 
                           cover from the trees is eliminated, allowing sun
                           rays to penetrate where they were previously 
                           blocked. This causes soils to dry out, which can
                           transform once forested land into deserts. 
                           Without canopy cover to block sunlight during
                           the day and retain heat during the night, 
                           temperature fluctuations become more severe
                           and harmful to wildlife. Deforestation also
                           leads to drier climates because less water 
                           is transpired, or released into the air, by
                           trees. This negatively impacts the water cycle
                           (National Geographic Society 2017)."),
                         p("Using our data set, one can visualize two more
                           consequences of deforestation- leaching 
                           of nutrients from the environment and 
                           increases in water runoff.  Trees prevent 
                           erosion by fixing soil in place with their
                           roots and creating natural dams with fallen
                           leaves and branches. When trees are removed,
                           erosion increases, carrying away the nutrients
                           in the soil. At the same time, deforestation 
                           inhibits water uptake by trees, resulting in
                           heightened runoff and more water moving through
                           the soil, dissolving and washing away more 
                           nutrients. Thus, the concentration of nutrients
                           in streamwater rises following 
                           deforestation (Likens et al. 1970)."),
                         p("At Hubbard Brook, an experiment was conducted 
                           in November and December 1965 in which all the
                           trees in Watershed 2 were cut, left in place,
                           and limbed so that no branches were more that 
                           1.5 meters high. Herbicide was applied 
                           periodically to Watershed 2 for two years 
                           after the deforestation event, so that forest
                           regrowth was prevented. Chemical and quantity
                           measurements of precipitation and streamwater
                           discharge were recorded. At the same time, these
                           measurements were taken at other Hubbard Brook 
                           watersheds, particullarly Watershed 6, which 
                           was left undisturbed (Likens et al. 1970). 
                           These graphs compare data from the deforested
                           Watershed 2 and the undisturbed Watershed 6, 
                           so you can explore the effects of deforestation yourself."),
                         h4("References"),
                         p("\"Deforestation and Its Effect on the Planet.\"", 
                           em("National Geographic."), 
                           "National Geographic Society, 24 May 2017. Web. 16 June 2017.
                           <http://www.nationalgeographic.com/environment/global-warming/deforestation/>."),
                         p("Likens, Gene E., F. Herbert Bormann, Noye M. Johnson, D. W. Fisher, 
                           and Robert S. Pierce. \"Effects of Forest Cutting and Herbicide Treatment 
                           on Nutrient Budgets in the Hubbard Brook Watershed-Ecosystem.\"", 
                           em("Ecological Monographs")," 40.1 (1970): 23-47. Web. 16 June 2017.")))
             
             
             
    )
    
    
  )
  
  
))