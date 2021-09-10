library(shiny)
library(shiny.semantic)
library(DT)
library(leaflet)
library(readr)
library(geosphere)
library(shinyjs)
library(testthat)

ships <- readr::read_csv(unzip("ships_04112020.zip", "ships.csv"))
ships$Time <- format(ships$DATETIME,"%H:%M:%S")


myGrid <- grid_template(
    default = list(
        # Here we define the data.frame describing our layout
        # The easiest way is to use rbind so that the layout can be 'visualized' in code
        areas = rbind(
            c("header", "info1", "info2"),
            c("map",   "map",   "map"),
            c("note",   "note",   "note"),
            c("input1",   "input2",   "general")
        ),
        # Then we define the dimensions of the different elements of the layout
        # We can use any valid css units to make the layout behave exactly as desired
        rows_height = c("100px", "2fr", "1fr", "100px"),
        cols_width = c("34%", "33%", "33%")
    ),
    # This is optional, but we can define a specific layout for mobile (screen width below 768px)
    mobile = list(
        areas = rbind(
            c("header", "info1", "info2"),
            c("map",   "map",   "map"),
            c("note",   "note",   "note"),
            c("input1",   "input2",   "general")
        ),
        rows_height = c("100px", "2fr", "1fr", "100px"), # Notice how we changed the rows heights here
        cols_width = c("34%", "33%", "33%")
    )
)

#-------------------------------------------------------------------------------#
ui <- semanticPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dark_modee.css")
    ),
    
    uiOutput("welcomeModal"),
    title = h2(class = "ui header", icon("anchor"), div(class = "content", "Ship Movement")), 
    includeCSS("dark_modee.css"),
    grid(myGrid,
         # We can define the css style of the grid using container_style
         container_style = "",
         # We can define the css style of each of the grid elements using area_styles
         area_styles = list(header = "margin-top: 5px; margin-left: 5px",
                            info1 = "",
                            info2 = "",
                            map = "border: 1px solid black;",
                            note = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                            input1 ="color: white; margin-left: 5px",
                            input2 ="",
                            general ="margin-top: 10px;"),
         # Finally, we define the ui content we would like to have inside each element
         header = h1(class="ui header", icon("anchor"), theme = "cosmo", div(style = "margin-left: 50px", class="content", "Ship Movement")),
         info1 = card(class = "red",
                      div(class = "content",
                          div(class = "header", textOutput("Vessel")),
                          div(class = "meta", textOutput("ship_total")),
                          div(class = "description", textOutput("DWT_total"))
                      )
         ),
         map = tabset(
             tabs = list(
                 list(menu = "Map", class="ui blue ribbon label", content = leafletOutput("map1"), id = "first_tab"),
                 list(menu = "Observations", class = "basic", content = semantic_DTOutput("table"), id = "second_tab")
             ),
             active = "first_tab",
             id = "exampletabset"
         ),
         input1 = selectInput(
             inputId = "ves_type",
             label = div(shiny::tags$h3("Select Vessel Type" , style="color:white")),
             choices = unique(ships$ship_type),
             selected = unique(ships$ship_type)[1],
             width = 200
         ),
         input2 = uiOutput("ship"),
         note = textOutput("text"),
         info2 = card(class = "red",
                      div(class = "content",
                          div(class = "header", textOutput("shipname")),
                          div(class = "meta", textOutput("port")),
                          div(class = "description", textOutput("DWT"))
                      )
         ),
         general = textOutput("text2")
    )
)

#--------------------------------------------------------------------------------#
    
server <- function(input, output, session) {

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
        semantic_DT(ships[ships$SHIPNAME== input$ship,c(16, 22, 15, 6, 3,4,5,20)], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    )
    
    output$ship<- renderUI({ 
        selectInput(
            inputId = "ship",
            label = div(shiny::tags$h3("Select Vessel Type" , style="color:white")),
            choices = unique(ships$SHIPNAME[ships$ship_type== input$ves_type]),
            selected = unique(ships$SHIPNAME[ships$ship_type== input$ves_type])[1],
            width = 200
        )
    })
    
    output$text2 <- renderText({
        desc
    })
    
    output$welcomeModal <- renderUI({
        create_modal(modal(
            id = "simple-modal",
            title = "Information",
            header = h2(class = "ui header", icon("anchor"), div(class = "content", "Ship Movement")),
            content = grid(
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

#-------------------------------------------------------------------------------#

shiny::shinyApp(ui, server)

