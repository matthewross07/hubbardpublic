tabPanel('Data exploration',
         fluidRow(column(3,
                         selectizeInput('ws.mr',label='Select up to 2 watersheds',
                                        choices=ws,selected=ws[1:2],
                                        options=list(maxItems=2),multiple=T),
                         selectizeInput('element.mr',label='Select up to 3 Elements',
                                        choices=elements,selected=elements[1],
                                        options = list(maxItems = 3))),
                  column(9,
                         dygraphOutput('dy1',width='95%',height='200px'),
                         br(),
                         dygraphOutput('dy2',width='95%',height='200px')
                  ))
         
)