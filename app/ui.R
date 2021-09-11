
semanticPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_modee.css")
  ),
  
  uiOutput("welcomeModal"),
  title = h2(class = "ui header", icon("anchor"), div(class = "content", "Ship Movement")), 
  includeCSS("dark_modee.css"),
  shiny.semantic::grid(myGrid,
                       # We can define the css style of the grid using container_style
                       container_style = "",
                       # We can define the css style of each of the grid elements using area_styles
                       area_styles = list(header = "margin-top: 5px; margin-left: 5px",
                                          info1 = "",
                                          info2 = "",
                                          map = "",
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