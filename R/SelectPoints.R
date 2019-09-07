SelectPoints <- function(boundingBox, graticuleInterval = 0.1, trackPoints = trackTable){
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    p()
  )

  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval, trackPoints = trackTable
    )})
    
    observeEvent(input$myMap_click, {
      click <- input$myMap_click
      clat <- click$lat
      clng <- click$lng
      
      print(c(clng, clat))
      print(click)
      
      # selectedPoints <<-c(list(c(lon = clng, lat = clat)), selectedPoints)
      selectedPoints <<- rbind(selectedPoints, c(lon = clng, lat = clat))
      
      leafletProxy('myMap') %>%
        addCircles(lng=clng, lat=clat, group='circles',
                   weight=1, radius=10, color='black', fillColor='green',
                   fillOpacity=0.2, opacity=1)
    })
  }
  
  shinyApp(ui, server)
  
}