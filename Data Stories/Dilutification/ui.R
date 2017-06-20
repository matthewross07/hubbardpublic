shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dilutification of Streamwater"),
  
  # Sidebar with input panels
  sidebarLayout(
    sidebarPanel(
      #Dropdown select box for the solute
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
      #Group checkbox input for the watersheds to display
      checkboxGroupInput("watershed", label = h3("Watersheds to Display"),
                         choices = list("Watershed 1" = 1,
                                        "Watershed 2" = 2,
                                        "Watershed 3" = 3,
                                        "Watershed 4" = 4,
                                        "Watershed 5" = 5,
                                        "Watershed 6" = 6,
                                        "Watershed 7" = 7,
                                        "Watershed 8" = 8,
                                        "Watershed 9" = 9),
                         selected = 1),
      #Dropdown select box for the units for the data to display
      selectInput("units", label = h3("Units"),
                  choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                 "ueq/L" = "ueq/L",
                                 "ln(ueq/L)" = "ln(ueq/L)",
                                 "ln(Eq/ha-yr)" = "ln(Eq/ha-yr)"),
                  selected = "ueq/L"),
      #Dropdown select box for the time scale, month or year
      selectInput("scale", label = h3("Time Scale"),
                  choices = list("By month" = "month",
                                 "By year" = "year"),
                  selected = "year"),
      #Dropdown select box for choosing whether or not to display precipitation
      selectInput("p", label = h3("Adding Precipitation"),
                  choices = list("Without Precipitation" = "noprecip",
                                 "With Precipitation" = "precip"),
                  selected = "noprecip"),
      #Slider box for choosing the date range to display
      sliderInput("dates", label = h3("Date Range"),
                  min = as.Date("1962-01-01"),
                  max = as.Date("2014-01-01"),
                  value = c(as.Date("1965-01-01"), as.Date("2010-01-01")))
    ),
    
    # Show a plot of the concentrations of solutes over time
    #in various watersheds
    mainPanel(
      plotlyOutput("splot", width = "100%", height = "100%")
    )
  )
))