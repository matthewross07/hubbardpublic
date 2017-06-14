  sidebarLayout(
    sidebarPanel(
      selectizeInput('acidrain_ww', 'Choose a watershed:', choices=unique(chem.annual.ueq$ws), selected=6),
      sliderInput("acidrain_range", "Choose a year range:", min=1964, max=2013, value=c(1964,2013), sep="")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Charge balance timeseries",
          plotOutput("acidrain_plot")
        ),
        tabPanel("Cation-anion scatterplots",
          plotOutput("acidrain_scatterplot")
        ),
        tabPanel( #Devri code.... obv needs optimization
         "Compound manipulation in w6",
            fluidRow( column(4,
                             #make an if-statement to have the radio buttons use yearly vs monthly data!!
                             
                             #radioButtons to select compounds - try to pair up P&Q on same graph
                             radioButtons("rbP1", label = "Choose a first precipitation compound",
                                          choices = c("SO4" = "SO4.precip", "Mg" = "Mg.precip", "K" = "K.precip", 
                                                      "Na" = "Na.precip", "Al" = "Al.precip", "NH4" = "NH4.precip", 
                                                      "NO3" = "NO3.precip", "Cl" = "Cl.precip", "PO4" = "PO4.precip", 
                                                      "SiO2" = "SiO2.precip", "Ca" = "Ca.precip", "pH" = "pH.precip")),
                             radioButtons("rbQ1", label = "Choose a first discharge compound",
                                          choices = c("SO4" = "SO4.flow", "Mg" = "Mg.flow", "K" = "K.flow", 
                                                      "Na" = "Na.flow", "Al" = "Al.flow", "NH4" = "NH4.flow",
                                                      "NO3" = "NO3.flow", "Cl" = "Cl.flow", "PO4" = "PO4.flow", 
                                                      "SiO2" = "SiO2.flow", "Ca" = "Ca.flow", "pH" = "pH.flow"))),
                      column(4,
                             radioButtons("rbP2", label = "Choose a second precipitation compound",
                                          choices = c("pH" = "pH.precip", "Ca" = "Ca.precip", "Mg" = "Mg.precip", 
                                                      "K" = "K.precip", 
                                                      "Na" = "Na.precip", "Al" = "Al.precip", "NH4" = "NH4.precip", 
                                                      "NO3" = "NO3.precip", "Cl" = "Cl.precip", "PO4" = "PO4.precip", 
                                                      "SiO2" = "SiO2.precip", "SO4" = "SO4.precip")),
                             radioButtons("rbQ2", label = "Choose a second discharge compound",
                                          choices = c("pH" = "pH.flow", "Ca" = "Ca.flow", "Mg" = "Mg.flow", 
                                                      "K" = "K.flow", 
                                                      "Na" = "Na.flow", "Al" = "Al.flow", "NH4" = "NH4.flow",
                                                      "NO3" = "NO3.flow", "Cl" = "Cl.flow", "PO4" = "PO4.flow", 
                                                      "SiO2" = "SiO2.flow", "SO4" = "SO4.flow"))),
                      column(4,
                             #use slider to view data in specific time ranges
                             sliderInput("dateSlide", label = "Input date range",
                                         min = as.Date("1963/06/01"), 
                                         max = as.Date("2013/06/01"),
                                         value = c(as.Date("1963/06/01"), as.Date("2013/06/01")),
                                         timeFormat="%b %Y"),
                             #switch between monthly and yearly data
                             selectInput("selDate", label = "Timescale granularity",
                                         choices = c("Monthly" = "date", "Yearly" = "year"))),
                      fluidRow(
                        column(12, plotOutput("cmpd1"))),
                      fluidRow(
                        column(12, plotOutput("cmpd2"))
                      )
            )
          )
          
        )
      )
  )
