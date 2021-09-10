max_dist<- function(ship){
  n<- which(ships$SHIPNAME == ship)
  ship2_new<- ships[n,]
  
  ship2_new$dist[1]<- 0
  for (i in 2:dim(ship2_new)[1]) {
    ship2_new$dist[i]<- distm(c(ship2_new$LON[i-1], ship2_new$LAT[i-1]), c(ship2_new$LON[i], ship2_new$LAT[i]), fun = distHaversine)
  }
  
  m<- which(ship2_new$dist== max(ship2_new$dist))
  result<- ship2_new[c(m[1]-1,m[1]),]
  
  return(result)
}



note<- function(ship){
  n<- which(ships$SHIPNAME == ship)
  ship2_new<- ships[n,]
  
  ship2_new$dist[1]<- 0
  for (i in 2:dim(ship2_new)[1]) {
    ship2_new$dist[i]<- distm(c(ship2_new$LON[i-1], ship2_new$LAT[i-1]), c(ship2_new$LON[i], ship2_new$LAT[i]), fun = distHaversine)
  }
  
  str1<- paste( ship," is a ", ship2_new$ship_type[1], " type ship. Ship has covered ") 
  str2<- paste("around ",  round(sum(ship2_new$dist, na.rm = TRUE),2)," metres in the observation period," )
  str3<- paste(" at an average speed of ", round(mean(ship2_new$SPEED, na.rm = TRUE),2), " knots. " )
  str4<- paste(ship," is", ship2_new$WIDTH[1], " mtrs wide and ", ship2_new$LENGTH[1], " mtrs long in size. " )
  str5<- paste("It has a dead weightage tonne capacity of ", ship2_new$DWT[1], " tonnes." )
  str6<- paste(" Out of all trips under observation, ", ship2_new$SHIPNAME[1], " has the longest ")
  str7<- paste(" trip of ",round(max(ship2_new$dist),2), " metres around ", ship2_new$Time[ship2_new$dist==max(ship2_new$dist)], " on ", ship2_new$date[ship2_new$dist==max(ship2_new$dist)])
  str8<- paste(". Origin pt. and destination pt. of respective trip is displayed")
  str9<- paste(" in the map on the right.")
  
  content<- HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, str9, sep = ''))
  return(content)
}

ships_ves<- function(vessel){
  
  ships_ves<- unique(ships$SHIPNAME[ships$ship_type==vessel])%>% as.data.frame()
  for (i in 1:dim(ships_ves)[1]){
    ships_ves$DWT[i]<- ships$DWT[ships$SHIPNAME== ships_ves$.][1]
  }
  return(ships_ves)

}


intro <- as.character("Maritime transport (or ocean transport) and fluvial transport, or more generally waterborne transport, is the transport of people (passengers) or goods (cargo) via waterways. Freight transport by sea has been widely used throughout recorded history. App goes into the details of movement of differreent categories of ships in a certain time period. This application has been developed to observe the patterrns of maritime movement in Poland.")
desc<- as.character("Note: Dashboard is created by using Shiny.semantic packag of Appsilon. It is developed to observe the trends and patterns about ships in Poland.")
                    