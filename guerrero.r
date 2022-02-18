#Cargamos las librerias

setwd("C:/Users/PC/Documents/Rolando/R/Rutas/")

library(rvest)
library(osrm)
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)


#Extraemos los datos

dt<-read_html("data/Guerrero.kml")

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

des<-dt%>%html_elements("description")%>%
	html_text()
des<-des%>%str_remove("Descripción: ")%>%str_remove("\\]\\]\\>")

nom<-dt%>%html_elements("name")%>%
  html_text()
nom
df$des<-des
df$name<-nom[-1]
df


#write.csv(df,"data/data_guerrero.csv", row.names=F)

#Generar la ruta más cortas

trips3 <- osrmTrip(df[,1:3], returnclass="sf",overview="full")
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