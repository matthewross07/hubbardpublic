


output$dy1 <- renderDygraph({
  
  
  shed1 <- left_join(chem.annual,hydro.annual,by=c('ws','water.year')) %>%
    filter(ws == input$ws.mr[1]) %>%
    select_(.dots=paste(c('Q',sort(input$element.mr),'yr'),sep=','))
  
  xts1 <- xts(shed1 %>% select(-yr),order.by=shed1$yr)
  
  cols <- df.cols %>% filter(el %in% as.vector(input$element.mr)) %>%
    arrange(el)
  cols <- as.vector(cols$col)
  
  dy1 <-  dygraph(xts1,group='explore',xlab='',
                  ylab='Volume Weighted Mean Concentration (mg/L)',
                  main=paste('Watershed',input$ws.mr[1])) %>%
    dyOptions(useDataTimezone=T,fillAlpha=0.2) %>%
    dySeries(name='Q',color='gray40',fillGraph=T,axis='y2')  %>%
    dySeries(input$element.mr[1],color=cols[1],axis='y',strokeWidth=3) %>%
    dyAxis('y2',label='Q (cubic meter year)',independentTicks=FALSE)
  
  
  if(length(input$element.mr) == 2) {
    dy1 <- dySeries(dy1,input$element.mr[2],color=cols[2],strokeWidth=3)
  }
  if(length(input$element.mr) == 3) {
    dy1 <- dySeries(dy1,input$element.mr[2],color=cols[2],strokeWidth=3) %>%
      dySeries(input$element.mr[3],color=cols[3],strokeWidth=3)
  }
  
  dy1
  
})

output$dy2 <- renderDygraph({
  
  
  shed1 <- left_join(chem.annual,hydro.annual,by=c('ws','water.year')) %>%
    filter(ws == input$ws.mr[2]) %>%
    select_(.dots=paste(c('Q',sort(input$element.mr),'yr'),sep=','))
  
  xts1 <- xts(shed1 %>% select(-yr),order.by=shed1$yr)
  
  cols <- df.cols %>% filter(el %in% as.vector(input$element.mr)) %>%
    arrange(el)
  cols <- as.vector(cols$col)
  
  dy1 <-  dygraph(xts1,group='explore',xlab='',
                  ylab='Volume Weighted Mean Concentration (mg/L)',
                  main=paste('Watershed',input$ws.mr[2])) %>%
    dyOptions(useDataTimezone=T,fillAlpha=0.2) %>%
    dySeries(name='Q',color='gray40',fillGraph=T,axis='y2')  %>%
    dySeries(input$element.mr[1],color=cols[1],axis='y',strokeWidth=3) %>%
    dyAxis('y2',label='Q (cubic meter year)',independentTicks=FALSE)
  
  
  if(length(input$element.mr) == 2) {
    dy1 <- dySeries(dy1,input$element.mr[2],color=cols[2],strokeWidth=3)
  }
  if(length(input$element.mr) == 3) {
    dy1 <- dySeries(dy1,input$element.mr[2],color=cols[2],strokeWidth=3) %>%
      dySeries(input$element.mr[3],color=cols[3],strokeWidth=3)
  }
  
  dy1
  
})
