
#load in all the data from Camila download
precip_stream_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")
#simplify dataset to make Ca graph
Cadata <- precip_stream_data[precip_stream_data$solute == "Ca",]
Cadata <- Cadata[Cadata$ws == "6",]


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

#Devri code for Ca ggiraph
output$CaTime <- renderggiraph({
  CaTime <- ggplot(Cadata, aes(x = as.Date(date)))+
    geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
    labs(colour = "Source", x = "Year", y = "Ca (ueq/L)")+
    xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
    ggtitle("Calcium's acid rain peak in 1970 and subsequent decline")+
    theme(plot.background = element_rect(fill = 'gray', colour = 'gray'))+
    theme(panel.background = element_rect(fill = 'black'))
  
  my_ggCa <- CaTime +
    geom_point_interactive(aes(y = concentration_ueq, col = source, tooltip = date, data_id = date), size = 6)+ 
    labs(colour = "Source", x = "Year", y = "Ca (ueq/L)")+
    xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
    theme(axis.title.x =  element_blank(),
          axis.text.x  =  element_text(angle=0, vjust=0.5, size=30), 
          axis.title.y = element_text(face="bold", size=35),
          axis.text.y  = element_text(angle=0.3, vjust=0.5, size=30),
          legend.title = element_text(size=40, face="bold"),
          legend.text = element_text(size = 30),
          plot.title = element_text(face="bold", size=55))
  
  ggiraph(code = print(my_ggCa), width = .8, width_svg = 45,
          height_svg = 15, hover_css = "cursor:pointer;fill:black;stroke:black;")
})

