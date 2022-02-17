setwd("C:/Users/PC/Documents/Rolando/R/Rutas/")

library(rvest)
library(osrm)
library(sf)
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


#save(tf, file = "data/ruta_suchil.RData")

###############################################
# Guardar datos en csv
###############################################

des<-dt%>%html_elements("description")%>%
	html_text()
des<-des%>%str_remove("descripción: ")%>%str_remove("]]>")

nom<-dt%>%html_elements("name")%>%
  html_text()
nom
df$des<-des
df$name<-nom[-1]
aux<-df[1:25,]

#write.csv(aux,"data/data_suchil.csv", row.names=F)




#Show names of nodes
dt%>%rvest::html_children()%>%
html_children()%>%
html_children()%>%
html_children()%>%
html_children()

RolTC. romayo10

###############################################
# Hotra cosa
###############################################

library(data.table)
datos <- data.table(c(seq(1,52,1)),ceiling(runif(52)*100))
colnames(datos) <- c("semana","animales.vistos")

datos <- datos[,grupo := rep(1:12,c(4,4,5,4,4,5,4,4,5,4,4,5))]
resultado <- datos[,suma:=(sum(animales.vistos)),by=c("grupo")]

print(resultado)

datos <- datos[,grupo2 := semana%%4]
print(datos)
datos

load("data/ruta_suchil.Rdata")

class(tf)
str(trips)
class(trips)

st_write(tf , "test.kml", driver = "kml")

sum(tf$duration)

###############################################
# Generando Ruta en Google Maps
###############################################

dir<-"https://www.google.com/maps/dir/remplazar@23.6244738,-103.93449,15z/data=!4m2!4m1!3e0"

struc<-"-32.0623722,-60.6317319/-32.061733,-60.6310392/-32.0609381,-60.6305639/lat,lon"

df$id

df2<-df[c(tf$start,tf$end[nrow(tf)]),]

df2$id

txt<-""
for(i in 1:nrow(df2)){
  txt<-paste0(txt,df2$lat[i],",",df2$lon[i],"/")
}

txt



dir<-str_replace(dir,"remplazar",txt)
browseURL(dir)
dir