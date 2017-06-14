#Devri data... clean up asap
w6precip <- read_csv("D:/Duke/Work(Environ)/Data/w6_precip_chem.txt")
w6stream <- read_csv("D:/Duke/Work(Environ)/Data/w6_stream_chem.txt")
public_data <- load("D:/Duke/Work(Environ)/Data/public_data.Rdata")

#rename columns to have ".flow" and ".precip"
#OPTIMIZE
w6stream <- plyr::rename(w6stream, c("Ca" = "Ca.flow",
                                     "Mg" = "Mg.flow",
                                     "K" = "K.flow",
                                     "Na" = "Na.flow",
                                     "Al" = "Al.flow",
                                     "NH4" = "NH4.flow",
                                     "pH" = "pH.flow",
                                     "SO4" = "SO4.flow",
                                     "NO3" = "NO3.flow",
                                     "Cl" = "Cl.flow",
                                     "PO4" = "PO4.flow",
                                     "SiO2" = "SiO2.flow"))
w6precip <- plyr::rename(w6precip, c("Ca" = "Ca.precip",
                                     "Mg" = "Mg.precip",
                                     "K" = "K.precip",
                                     "Na" = "Na.precip",
                                     "Al" = "Al.precip",
                                     "NH4" = "NH4.precip",
                                     "pH" = "pH.precip",
                                     "SO4" = "SO4.precip",
                                     "NO3" = "NO3.precip",
                                     "Cl" = "Cl.precip",
                                     "PO4" = "PO4.precip",
                                     "SiO2" = "SiO2.precip"))

#merge precip and stream df
w6chem <- merge(w6stream, w6precip, by = c("ws", "year", "mo"))

#unite year:mo
w6chem <- unite(w6chem, date, year:mo, sep = '/')
#make into date format
w6chem$date <- as.character(w6chem$date)
w6chem['date'] <- apply(w6chem[, 'date', drop=F], 2, function(x){paste0(x, "/01")})

#make all -3 values NA
w6chem[w6chem == -3] <- NA

#convert compounds to be in ueq/L #########make a function or loop for this?
conc_mg_to_ueqConverter <- function(initial, z, MW){
  ueqValue <- initial*1000*z/MW
}
#calculation: conc_mg * 1000*z/MW
w6chem$Ca.flow <- w6chem$Ca.flow * 1000 * 2 / 40.078
w6chem$Mg.flow <- w6chem$Mg.flow * 1000 * 2 / 24.305
w6chem$K.flow <- w6chem$K.flow * 1000 * 1 / 39.098
w6chem$Na.flow <- w6chem$Na.flow * 1000 * 1 / 22.990
w6chem$Al.flow <- w6chem$Al.flow * 1000 * 3 / 26.982 #figure out Al charge
w6chem$NH4.flow <- w6chem$NH4.flow * 1000 * 1 / 18.040
w6chem$SO4.flow <- w6chem$SO4.flow * 1000 * 2 / 96.060 #negative charge
w6chem$NO3.flow <- w6chem$NO3.flow * 1000 * 1 / 62.004 #negative charge
w6chem$Cl.flow <- w6chem$Cl.flow * 1000 * 1 / 35.450 #negative charge
w6chem$PO4.flow <- w6chem$PO4.flow * 1000 * 3 / 94.971 #negative charge
w6chem$SiO2.flow <- w6chem$SiO2.flow * 1000 * 1 / 60.080 #Net charge?

w6chem$Ca.precip <- w6chem$Ca.precip * 1000 * 2 / 40.078
w6chem$Mg.precip <- w6chem$Mg.precip * 1000 * 2 / 24.305
w6chem$K.precip <- w6chem$K.precip * 1000 * 1 / 39.098
w6chem$Na.precip <- w6chem$Na.precip * 1000 * 1 / 22.990
w6chem$Al.precip <- w6chem$Al.precip * 1000 * 3 / 26.982 #figure out Al charge
w6chem$NH4.precip <- w6chem$NH4.precip * 1000 * 1 / 18.040
w6chem$SO4.precip <- w6chem$SO4.precip * 1000 * 2 / 96.060 #negative charge
w6chem$NO3.precip <- w6chem$NO3.precip * 1000 * 1 / 62.004 #negative charge
w6chem$Cl.precip <- w6chem$Cl.precip * 1000 * 1 / 35.450 #negative charge
w6chem$PO4.precip <- w6chem$PO4.precip * 1000 * 3 / 94.971 #negative charge
w6chem$SiO2.precip <- w6chem$SiO2.precip * 1000 * 1 / 60.080 #Net charge?

#split date 
as.character(w6chem$date)
w6chemsplit <- separate(w6chem, col = date, into = c("year", "month", "day"), sep = "/")
#add year column for later granularity interactive manipulation
w6chem$year <- w6chemsplit$year
#move the pH values to end of the table so they don't get aggregated
w6chem <- w6chem[, c(1:9, 11:22, 24:28, 10, 23, 29)]
#aggregate by year column to get yearly averages
w6chemyear <- group_by(w6chem, year) %>%
  summarise_each(funs(sum), flow:SiO2.precip)#this works great..
#format new year column as date in both w6chemyear AND in w6chem
w6chemyear$year <- as.character(w6chemyear$year)
w6chemyear['year'] <- apply(w6chemyear[, 'year', drop = F], 2, function(x){paste0(x, '/06/01')})
as.Date(w6chemyear$year)

w6chem$year <- as.character(w6chem$year)
w6chem['year'] <- apply(w6chem[, 'year', drop = F], 2, function(x){paste0(x, '/06/01')})
as.Date(w6chem$year)
#rename all vars to have 'yearly' before merging
w6chemyear <- plyr::rename(w6chemyear, c("Ca.precip" = "Ca.precipyear",
                                         "Mg.precip" = "Mg.precipyear",
                                         "K.precip" = "K.precipyear",
                                         "Na.precip" = "Na.precipyear",
                                         "Al.precip" = "Al.precipyear",
                                         "NH4.precip" = "NH4.precipyear",
                                         "SO4.precip" = "SO4.precipyear",
                                         "NO3.precip" = "NO3.precipyear",
                                         "Cl.precip" = "Cl.precipyear",
                                         "PO4.precip" = "PO4.precipyear",
                                         "SiO2.precip" = "SiO2.precipyear",
                                         "Ca.flow" = "Ca.flowyear",
                                         "Mg.flow" = "Mg.flowyear",
                                         "K.flow" = "K.flowyear",
                                         "Na.flow" = "Na.flowyear",
                                         "Al.flow" = "Al.flowyear",
                                         "NH4.flow" = "NH4.flowyear",
                                         "SO4.flow" = "SO4.flowyear",
                                         "NO3.flow" = "NO3.flowyear",
                                         "Cl.flow" = "Cl.flowyear",
                                         "PO4.flow" = "PO4.flowyear",
                                         "SiO2.flow" = "SiO2.flowyear",
                                         "flow" = "flowyear",
                                         "precip" = "precipyear"))
#now add these columns to w6chem
w6chemDandY <- merge(w6chem, w6chemyear, by = "year")

#make w6chem a date again
w6chem$date <- as.Date(w6chem$date)


# ACID RAIN STORY
years = reactive(input$acidrain_range)
ws <- reactive(input$acidrain_ww)



output$acidrain_plot = renderPlot({
  #Rearrange data
  data <- chem.annual.umol.charge %>%
    gather(key='Solute',value='Concentration',-ws,-water.year,-datetime) %>%
    filter(!is.na(Concentration)) %>% 
    filter(ws == ws()) %>%
    filter(water.year >= years()[1] & water.year <= years()[2])
  col.df <- df.cols[which(df.cols$el %in% unique(data$Solute)),] 
  col.df$el <- droplevels(col.df$el)
  data$Solute <- factor(data$Solute,levels=col.df$el)
  data <- arrange(data,ws,Solute)
  #Plot data
  ggplot(data,aes(x=water.year,y=Concentration,fill=Solute)) + 
    geom_area(position ='stack') + 
    xlab('Water Year') +
    ylab(expression('VWC ('*mu*'eq L'^-1*')'))+
    theme_bw()+
    facet_grid(ws~.,space='free_y')+
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
    ggtitle(paste('Annual Charge Balance, HBEF (', years()[1],'-',years()[2],')', sep='')) + 
    scale_fill_manual(values=col.df$col)
})

# Anion Cation scatterplots
d2 = reactive({
  chem.annual.ueq %>% filter(ws==input$acidrain_ww[1] & water.year>1963) %>%
    rowwise() %>% dplyr::mutate(basecations=sum(Ca,Mg,K,Na),so4no3=sum(SO4,NO3)) %>%
    select(water.year,basecations,so4no3) %>% filter(!is.na(so4no3))
})
output$acidrain_scatterplot = renderPlot({
  fills = rep("white",nrow(d2()))
  win = which(d2()$water.year>years()[1]&d2()$water.year<years()[2])
  fills[win] = "blue"
  plot(d2()$basecations~d2()$so4no3,
       xlim=c(0,200),ylim=c(0,200),pch=21,col="blue",bg=fills,bty="n",
       ylab="Sum of Base Cations (ueq/L)",xlab="Sum of SO4+NO3 (ueq/L)")
  abline(0,1,lty=2)
  mm = lm(basecations~so4no3, data=d2()[win,])
  yrng = range(d2()[win,'water.year'])
  newx = d2() %>% filter(water.year%in%yrng) %>% select(so4no3)
  newy = predict(mm, newx)
  abline(mm,lty=3,col="royalblue")
  arrows(newx$so4no3[1], newy[1], newx$so4no3[2], newy[2],lwd=3)
})

#Devri code...obv needs optimization
output$cmpd1 <- renderPlot({
  ggplot(w6chem, aes(x = as.Date(date)))+
    geom_line(aes(y = get(input$rbP1), col = "Precip"))+ #maybe make Ca, Na, K, etc. columns&
    geom_line(aes(y = get(input$rbQ1), col = "Discharge"))+ # group.by source within the cmpd?
    labs(colour = "Source", x = "Year", y = "First compound (ueq/L)")+
    xlim(min(input$dateSlide[1]), max(input$dateSlide[2])) #use the date slider to change x axis
})
#plot of P and Q from another compound
output$cmpd2 <- renderPlot({
  ggplot(w6chem, aes(x = as.Date(date)))+
    geom_line(aes(y = get(input$rbP2), col = "Precip"))+
    geom_line(aes(y = get(input$rbQ2), col = "Discharge"))+
    labs(colour = "Source", x = "Year", y = "Second compound (ueq/L)")+
    xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))
})
