library(shiny, warn.conflicts = FALSE)
library(shiny.semantic)
 
ships <- readr::read_csv(unzip("ships_04112020.zip", "ships.csv"))
ships$Time <- format(ships$DATETIME,"%H:%M:%S")

shinyAppDir("app")

