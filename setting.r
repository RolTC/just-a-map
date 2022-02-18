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

C

dir<-str_replace(dir,"remplazar",txt)
browseURL(dir)
dir

###############################################
# Mapa con informacion
###############################################

library(htmlwidgets)
library(leaflet.extras)
#install.packages("leaflet.extras")
library(shiny)
# Define HTML for the infobox
info.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),

  # Header / Title
  HTML("Mapa de las tiendas de Suchil"),
  HTML(
    '</div><div class="modal-body">'
  ),

  # Body
  HTML('
    <h2>Descripcion</h2>
<p>En este mapa se presenta la ruta optima para recorrer las tiendas de Suchil Durango. El mapa fue creado con R y leaflet. Solo muestra la ruta a nivel general.</p>
<p>Para m&aacute;s detalle puede acceder a la informacion de cada punto haciendo clic sobre el.</p>
<p>Se han agregado conexiones con Google Maps ya que este tiene mejores caracteristicas de ruteo. En cada punto puede acceder a un bot&oacute;n para ver como llegar a el en Gmaps. Tamb&iacute;en puede ver la ruta general:</p>
<ul>
<li><a href="https://www.google.com/maps/dir/23.6206966,-103.9155234/23.6219358,-103.9165087/23.6255009,-103.9184623/23.6221322,-103.9221664/23.6221408,-103.9223006/23.6197038,-103.9266807/23.6168427,-103.9279619/23.6167314,-103.9290086/23.6171886,-103.929352/23.6199009,-103.931986/23.6224842,-103.932161/23.6210785,-103.934414/23.6193189,-103.9370211/23.6175299,-103.9372357/23.6192627,-103.9329944/23.6178373,-103.9319001/23.6163923,-103.9306931/23.6160366,-103.9300755/23.6157761,-103.9296463/23.6134414,-103.9312288/23.6151765,-103.9286915/23.6155468,-103.9258497/23.6169083,-103.9260375/23.6183587,-103.9242827/23.6261116,-103.9205028/@23.6244738,-103.93449,15z/data=!4m2!4m1!3e0">Ver en Google</a></li>
</ul>
<p></p>
<hr />
<p>Autor: Edson Rolando Tamayo Casta&ntilde;eda</p>
    '),

  # Closing divs
  HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Cerrar</button></div></div>')
))

load("data/ruta_suchil.Rdata")
aux<-read.csv("data/data_suchil.csv", header =T, stringsAsFactor =F)


aux$pos<-1:nrow(aux)
como_llegar<-"https://www.google.com/maps/dir/23.6206966,-103.9155234/data=!4m6!4m5!1m1!4e2!1m2!1m1!1s0x94f6ea0ca3aa1b6d:0x917b75179c5e987e?sa=X"
txt2<-"
<h4>Nombre</h4>
<p>desRem</p>
<ul>
<li><a href=\"https://www.google.com/maps/dir//latremp,lonremp/data=!4m2!4m1!3e0\">Ir a (google maps)</a></li>
</ul>
"

aux$popup<-txt2
aux

aux$popup<-aux$popup%>%str_replace("Nombre",aux$name)%>%
  str_replace("desRem",aux$des)%>%
  str_replace("latremp",as.character(aux$lat))%>%
  str_replace("lonremp",as.character(aux$lon))

leaflet(tf) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lat = aux$lat,
                   lng = aux$lon,
                   popup = aux$popup,
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)%>%
  addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
  addEasyButton(easyButton(
    icon = "fa-info-circle", title = "Map Information",
    onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
  )) %>% # Trigger the infobox
  htmlwidgets::appendContent(info.box)

