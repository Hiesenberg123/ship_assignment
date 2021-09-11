server <- function(input, output, session) {
  
  output$ship<- renderUI({ 
    selectInput(
      inputId = "ship",
      label = div(shiny::tags$h3("Select Ship" , style="color:white")),
      choices = unique(ships$SHIPNAME[ships$ship_type== input$ves_type]),
      selected = unique(ships$SHIPNAME[ships$ship_type== input$ves_type])[1],
      width = 200
    )
  })
  
  output$Vessel<- renderText({
    input$ves_type
  })
  
  
  output$ship_total<- renderText({
    paste("Total ", length(unique(ships$SHIPNAME[ships$ship_type==input$ves_type])), input$ves_type, " under observation")
  })
  
  
  output$DWT_total<- renderText({
    paste("Average speed of ", round(mean(ships$SPEED[ships$ship_type==input$ves_type], na.rm = TRUE),2), " knots")
  })
  
  output$shipname<- renderText({
    input$ship
  })
  
  output$port<- renderText({
    paste("Destination: ", ships$DESTINATION[ships$SHIPNAME== input$ship][1] , " port")
  })
  
  output$DWT<- renderText({
    paste("DWT capacity: ", ships$DWT[ships$SHIPNAME == input$ship][1] , " Tonnes; Mean speed: ", round(mean(ships$SPEED[ships$SHIPNAME==input$ship], na.rm = TRUE),2), " knots")
  })
  
  output$map1<- renderLeaflet({
    coord<- max_dist("KAROLI")
    mydf <- data.frame(Observation = c("A"),
                       InitialLat = c(coord$LAT[1]),
                       InitialLong = c(coord$LON[1]),
                       NewLat = c(coord$LAT[2]),
                       NewLong = c(coord$LON[2]),
                       stringsAsFactors = FALSE)
    mydf2 <- data.frame(group = c("A"),
                        lat = c(mydf$InitialLat, mydf$NewLat),
                        long = c(mydf$InitialLong, mydf$NewLong))
    
    leaflet()%>%
      addTiles() %>%
      addCircleMarkers(data = mydf2, lng = ~long, lat = ~lat, group = ~group) %>%
      addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group) %>%
      addMarkers(data = mydf2, lng = ~long, lat = ~lat, group = ~group) 
  })
  
  output$text<- renderText({
    note(input$ship)
  })
  
  observeEvent(input$ship,{
    coord<- max_dist(input$ship)
    
    mydf3 <- data.frame(Observation = c("A"),
                        InitialLat = c(coord$LAT[1]),
                        InitialLong = c(coord$LON[1]),
                        NewLat = c(coord$LAT[2]),
                        NewLong = c(coord$LON[2]),
                        stringsAsFactors = FALSE)
    mydf4 <- data.frame(group = c("A"),
                        lat = c(mydf3$InitialLat, mydf3$NewLat),
                        long = c(mydf3$InitialLong, mydf3$NewLong))
    
    
    leafletProxy("map1", data= mydf4) %>%
      clearShapes() %>%clearMarkers() %>%
      addCircleMarkers(data = mydf4, lng = ~long, lat = ~lat, group = ~group) %>%
      addPolylines(data = mydf4, lng = ~long, lat = ~lat, group = ~group) %>%
      addMarkers(data = mydf4, lng = ~long, lat = ~lat, group = ~group)
  })
  
  output$plot2 <- renderPlot(
    plot(mtcars[[input$plot1xaxis]], mtcars$cyl, col = 'red',
         xlab = input$plot1xaxis, ylab = "cyl")
  )
  
  
  output$table <- DT::renderDataTable(
    semantic_DT(ships[ships$SHIPNAME== input$ship, c(16, 21, 15, 6, 3,4,5,20)], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  )
  
  
  
  output$text2 <- renderText({
    desc
  })
  
  output$welcomeModal <- renderUI({
    create_modal(modal(
      id = "simple-modal",
      title = "Information",
      header = h2(class = "ui header", icon("anchor"), div(class = "content", "Ship Movement")),
      content = shiny.semantic::grid(
        grid_template = grid_template(
          default = list(
            areas = rbind(c("photo", "text")),
            cols_width = c("50%", "50%")
          ),
          mobile = list(
            areas = rbind(c("photo"), c("text")),
            cols_width = c("100%"),
            rows_height = c("50%", "50%")
          )
        ),
        container_style = "grid-gap: 20px",
        area_styles = list(text = "padding-right: 20px"),
        photo = tags$img(src = "https://images.unsplash.com/photo-1582517378602-f109b395ce40?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=2851&q=80", style = "width: 75%", alt = ""),
        text = HTML(
          sprintf(
            intro
          )
        )
      )
    ))
  })
}
