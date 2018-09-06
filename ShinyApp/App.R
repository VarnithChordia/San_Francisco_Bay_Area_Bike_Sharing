

library('googleway')
library('tidyverse')
library('httr')
library('leaflet')
library('shinydashboard')
library('shiny')
library('jsonlite')
library('rvest')
library("stringr")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(uiOutput("Start_Dst"),
                   uiOutput("End_Dst"),
                   actionButton(inputId = "getRoute", label = "Get Route")),
  dashboardBody(
    leafletOutput("leaf",width = "100%",height =1600)
    # google_mapOutput("myMap")
    
  )
)

server <- function(input, output){
  
  url1<-"http://feeds.bayareabikeshare.com/stations/stations.json"
  
  live_data<-fromJSON(url1)
  
  live_data_stations<-live_data$stationBeanList
  
  api_key <- "AIzaSyCawF0upPqRR-2llxmO7NbXsbsrdEQXVN4"
  
  
  
  
  
  output$Start_Dst<-renderUI({
    selectizeInput('var1',label='Select Start Destination',multiple=T,choices=live_data_stations$stationName)
  })
  
  output$End_Dst<-renderUI({
    selectizeInput('var2',label='Select End Destination',multiple=T,choices=live_data_stations$stationName)
  })

  htmlGen <- function(shape){
    html <- paste0("<b>",shape$stationName,"</b><br>",
                   "Bikes Available: ",shape$availableBikes,"<br>",
                   "Docks Available: ",shape$availableDocks
    )
  }
  
  
  
  output$leaf <- renderLeaflet({
    map <- leaflet(data = live_data_stations)%>% addMarkers(popup = htmlGen(live_data_stations)) %>%
      setView(-122.4194, 37.7749, zoom = 13) %>%
      addTiles()
    return(map)
    
    
  })
  
  
  observeEvent(input$getRoute, {
    key <- "AIzaSyCawF0upPqRR-2llxmO7NbXsbsrdEQXVN4"
    o <- live_data_stations %>% filter(live_data_stations$stationName==input$var1[1]) 
    o1<-c(o$latitude,o$longitude)
    d <- live_data_stations %>% filter(live_data_stations$stationName==input$var2[1])
    d1<-c(d$latitude,d$longitude)
    
    k<-rbind(o,d)
    
    
    res <- google_directions(origin = o1,
                             destination = d1,
                             key = key)
    
    df_polyline <- decode_pl(res$routes$overview_polyline$points)
    
    
    
    
    z<-res$routes$legs 
    q<-z[[1]]
    
    w<-q$steps[[1]]
    w$html_instructions
    j<-w$distance
    
    
    a<-str_replace_all(w$html_instructions,"<b>","") %>% str_replace_all(.,"</b>","") %>% str_replace_all(.,"[[:punct:]]","") %>% 
      str_replace_all(.,"<div style=fontsize09em>", " for ") %>% str_replace_all(.,"<div>", "") %>% str_split(.,"for")
    
    a<-unlist(a)
    
    s<-c()
    
    for (i in 1:length(a)){
      
      s[i]<-paste("<br>",a[i],"for",j$text[i],"<br/>")
      
      if(i==length(a)){
        s[i]<-paste("<br>",a[i],"<br/>")
        
      }
      
      
    }
    
    
    
    q<-w$duration
    round(sum(q$value)/60)
    
    b<-round(nrow(df_polyline)/2)
    
    df_polyline$lon[b]
    df_polyline$lat[b]
    
    
    
    output$leaf <- renderLeaflet({
      map1<-leaflet(data=k) %>% addMarkers(popup = htmlGen(k)) %>% addPopups(df_polyline$lon[b],df_polyline$lat[b],paste(as.character(s), sep="' '", collapse="")) %>%
        addTiles() %>%
        addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
      return(map1)
      
      
    
      
    })
    
    
  })
  
}
  
  
shinyApp(ui, server)
  
  
  
  
  
  
  
  
  









