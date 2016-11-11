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
        )
      )
    )
  )
