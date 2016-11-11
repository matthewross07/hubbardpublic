
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
