setwd("C:/Users/PC/Documents/Rolando/R/Rutas/")

library(rvest)
library(osrm)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)

dt<-read_html("data/Suchil.kml")

x<-dt%>%
html_elements("coordinates")%>%
html_text()
x
df<-do.call(rbind,str_extract_all(x,"\\d+\\.*\\d*"))
df<-as.data.frame(df)
df$id<-1:nrow(df)
str(df)

df<-df%>%mutate(lon=-1*as.numeric(V1),
lat=as.numeric(V2))%>%
select(id,lon,lat)

trips <- osrmTrip(df[1:25,], returnclass="sf",overview="full")
trip <- trips[[1]]$trip
str(trip)
trip[26,]


leaflet(trip[23:25,]) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lat = df$lat,
                   lng = df$lon,
                   popup = df$id,
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)


trips2 <- osrmTrip(df, returnclass="sf",overview="full")
trip2 <- trips2[[1]]$trip


leaflet(trip2[23:25,]) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lat = df$lat,
                   lng = df$lon,
                   popup = as.character(df$id),
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)

 aux<-df[df$id%in%c(9,7,6),][c(3,2,1),]
 aux

trips3 <- osrmTrip(aux, returnclass="sf",overview="full")
trip3 <- trips3[[1]]$trip
trip3

leaflet(trip3) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lat = df$lat,
                   lng = df$lon,
                   popup = as.character(df$id),
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)


tf<-trip
tf[1:3,]<-trip2[1:3,]
tf[23,]<-trip3[2,]
#tf[24,]<-tf[25,]


 aux<-df[df$id%in%c(6,25),]
 aux

trips3 <- osrmTrip(aux, returnclass="sf",overview="full")
trip3 <- trips3[[1]]$trip
trip3 

tf[24,]<-trip3[1,]


  leaflet(tf[1:24,]) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lat = df$lat,
                   lng = df$lon,
                   popup = as.character(df$id[1:25]),
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)


tf<-tf[1:24,]


save(tf, file = "data/ruta_suchil.RData")

x<-dt%>%html_elements("description")%>%
	html_text()
x
df$name<-x[-1]


dt%>%xml_tag("Data")

dt


rvest::xml_tag

y<-readLines("data/Suchil.kml")
y

str(dt)
dt$dontCheck


#Show names of nodes
dt%>%rvest::html_children()%>%
html_children()%>%
html_children()%>%
html_children()%>%
html_children()

RolTC. romayo10