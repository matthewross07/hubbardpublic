

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
         "Acid Rain Story",
         fluidPage(theme = "Solar_bootstrap.min.css",
                   fluidRow(column(width = 12, offset = 1, h1(strong(em("Acid Rain & HBEF"))))),
                   fluidRow(column(width = 10, offset = 1,
                                   tags$img(src = "DeadForest.jpg", width = '1270px', height = '650px')),
                            column(1)),
                   fluidRow(column(width = 8, offset = 1,
                                   h3("Experiencing nature for many people means traveling to see awe-inspiring 
                                      views and wildlife in national parks or forests. Perhaps the place pictured above used to be one of
                                      those nature hubs. These national sites, as well as anywhere in nature, are composed of many diverse 
                                      ecosystems that maintain an important balance. 
                                      ")),
                            column(width = 2,
                                   p(strong("Ecosystem:"), "a network of animals, plants, and the physical features 
                                     of where they live")),
                            column(1)),
                   fluidRow(column(width = 8, offset = 1,
                                   h3("Starting in the early 1950s (soon after Disney first released Cinderella 
                                      and Peter Pan) this balance within ecosystems everywhere began to tip.  
                                      What caused this shift in so many ecosystems?  Well, the weather did 
                                      believe it or not.  More specifically, the precipitation that fell 
                                      on the ecosystems."),
                                   h3('“But don’t plants and animals need the rain and snowmelt to survive” you 
                                      ask?  Yes, point for you.  Though the precipitation at this time was 
                                      acid rain, and had become polluted to a point of concern.  Many 
                                      plants and aquatic creatures specifically were harmed by the increasing 
                                      acidity of the water, which began to disrupt the flow of the ecosystems.'),
                                   h3('Acid rain hasn’t always been around to harm ecosystems though.  It became 
                                      an issue as humans increasingly emitted sulfur dioxide (SO2) and 
                                      nitrogen oxides (NOx).  These chemicals came mostly from burning 
                                      fossil fuels (namely coal) to produce electricity, and from car 
                                      emissions.  They then rise into the atmosphere to react with water, 
                                      oxygen, etc. and are carried quite far from where they originated.  
                                      When they fall back to earth, in rain, snow, or even fog, it is called acid rain.
                                      ')),
                            column(3)),
                   fluidRow(column(width = 12, offset = 1
                                   #insert widget that links to a quizlet or something here
                   )),
                   fluidRow(column(width = 12, offset = 1,
                                   h2(strong("Chemistry")))),
                   fluidRow(column(width = 8, offset = 1,
                                   h3(" Though sulfur dioxide and nitrogen oxides have different effects on their 
                                      own, when combined in acid rain they do a number on nature.  One way they harm 
                                      ecosystems is by wearing down the natural soil buffer.")),
                            column(width = 2, #make a text box
                                   p(strong("Soil buffer:"), "chemicals naturally present in the soil, which neutralize the 
                                     strong acidity of acid rain at the expense of losing base cations in the 
                                     neutralizing reactions")),
                            column(1)
                                   ),
                   fluidRow(column(width = 8, offset = 1,
                                   h3("The acid rain reacts with the base cations in the soil, causing them to be 
                                      washed out of the ecosystem.  Try exploring this pattern using the graph 
                                      below.  You can see that calcium (Ca) discharge increases even though the Ca 
                                      precipitation remains relatively stable.")),     
                            column(width = 2, 
                                   p(strong("Base cations:"), "positively charged elements present in the soil that help
                                     neutralize acid rain (ie. Ca, Mg, K)")
                                   ),
                            column(1)),
                   fluidRow(column(width = 12, offset = 2,
                                   #use slider to view data in specific time ranges
                                   sliderInput("dateSlide", label = "Input date range",
                                               min = as.Date("1963/06/01"), 
                                               max = as.Date("2013/06/01"),
                                               value = c(as.Date("1963/06/01"), as.Date("2013/06/01")),
                                               timeFormat="%b %Y")
                                   #switch between monthly and yearly data
                                   #selectInput("selDate", label = "Timescale granularity",
                                   #            choices = c("Monthly" = "date", "Yearly" = "year"))
                   )
                   ),
                   fluidRow(column(width = 12,
                                   ggiraphOutput("CaTime") #make this be able to switch between base cations
                   )),
                   fluidRow(column(width = 8, offset = 1,
                                   h3("One effect of the base cation loss was the poor growth of Sugar Maples, 
                                      which rely heavily on Ca to grow.  Another danger to the ecosystem balance 
                                      was caused by acid rain reacting to release aluminum from the soil.  Aluminum 
                                      is toxic once released from its stable soil state, and makes it hard for trees 
                                      to take up water.  Taking the pH of the precipitation and streamflow also show 
                                      these effects of acid rain, because the inflow is acidic when the outflow is 
                                      much less so.  Acid is coming in, reacting, and staying.  Sounds like an 
                                      unwelcome house guest.")),
                            column(3))
                   )
         
          )
          
        )
      )
  )
