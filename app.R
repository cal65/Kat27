library(shiny)
library(ggplot2)
library(leaflet)
library(ggmap)
library(ggrepel)
library(raster)
library(RColorBrewer)

#setwd("~/Documents/CAL/Real_Life/LogiCal/Kat27")
Kat_locs <-read.csv('Kat_locs3.csv', stringsAsFactors = F)
Kat_locs$lon<-ifelse (Kat_locs$lon <0, Kat_locs$lon+360, Kat_locs$lon) #need to translate all longitudes to positive values because of mapping grid
Kat_locs$Type <- as.factor(Kat_locs$Type)
ui <- fluidPage(
	title = "World Katalogue",
	fixedRow(
	column(12,
		titlePanel(h1("World Katalogue", align = "center")),
		fixedRow(
			column(3,
				checkboxGroupInput(inputId = "Option", label = "Choose Your Type", c('Homes' = 'Home', 'Tournaments' = 'Tournament', 'Friends'= "Friend", 'Trips' = 'Trip'), selected=c('Home', 'Tournament', 'Friend', 'Trip')), #allow selection of  subsets of points,
				sliderInput(inputId = "size", label = "Choose a size", value = 5, min = 1, max = 10, width='200px'),
				h2("Add a new point"), textInput("Location", "Location"), textInput("Detail", "Name/Detail"),  selectInput("Type", label = "Choose the Event Type", choices = c('Home' = 'Home', 'Tournament' = 'Tournament', 'Friend'= "Friend", 'Trip' = 'Trip')), actionButton('Action', 'Submit')
				), 
			column(9,
				leafletOutput("map", width = "80%", height = "500px") 
				)
			)
		)
	)
)

server <- function(input, output, session) {
	
	pal <- colorFactor(brewer.pal(6, 'Dark2'), domain=Kat_locs$Type)
	values<-reactiveValues()
	values$df <- Kat_locs
	output$map <- renderLeaflet( {
		leaflet(values$df) %>% addProviderTiles('Esri.NatGeoWorldMap') %>% 
		setView(121.56, 25.03, zoom = 4) %>% addLegend("topleft", pal=pal, values = ~Type, title="Legend", opacity=2)#start off centered in Taipei with a far off zoom
				})
		
	observe({			
		proxy <- leafletProxy("map", data=subset(values$df, Type %in% input$Option))
		proxy %>% clearShapes() %>% addCircles(lng = ~jitter(lon, .15), lat = ~jitter(lat, .15), radius = ~(32*as.numeric(input$size))^2, color='black', weight=1, fillColor = ~pal(Type),  fillOpacity=0.8, popup = ~Detail)  
	})	
	observeEvent(input$Action,
		if (!is.na(input$Location)){
			new_loc <- geocode(input$Location)
			new_lon <- new_loc$lon
			new_lat <- new_loc$lat
			new_line <- data.frame(Location=input$Location, Type=input$Type, Detail=input$Detail, Time="", lon=ifelse(new_lon <0, new_lon + 360, new_lon), lat= new_lat, alt_lon = new_lon)
			isolate(values$df <- rbind(values$df,  new_line))
			updateTextInput(session, "Location", value = "")
			updateTextInput(session, "Detail", value = "")
		}
	)
}

shinyApp(ui = ui, server = server)

#Thunderforest.SpinalMap
#rsconnect::deployApp('~/Documents/CAL/Real_Life/LogiCal/Kat27')