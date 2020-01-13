
# SINAMECC ----------------------------------------------------------------

library("junr")
library("dplyr")
library("stringr")
library("tidyr")
library("tmap")
library("haven")
library(readxl)
library(rgdal)
library(tmap)
library(leaflet)
library(jsonlite)
library(networkD3)
library(httr)
library(jsonlite)
library(readxl)

url_base <- "http://sinamecc.cloudapi.junar.com/api/v2/datastreams/"
api_key <-  "e3256cf8b28ab8549daa1df241b51ff9071d0153"
get_index(url_base, api_key)
list_guid(url_base, api_key)
guids<-list_guid(url_base, api_key)

#tablas con 
dimensiones_tablas <- get_dimensions(url_base, api_key)

tablas<-lapply(guids, function(x) get_data(url_base, api_key, x))
names(tablas) <- list_titles(url_base, api_key)
#tablas sin nada
names(tablas[lapply(tablas,  nrow)==0])


names(tablas[lapply(tablas,  nrow)!=0])
#con inventario al inicio del título
Inventarios<-tablas[str_detect(names(tablas), "^Inventario.*")]
consumo<-tablas[str_detect(names(tablas), "^Consumo.*")]

#tablas energía bccr
path2<-"https://activos.bccr.fi.cr/sitios/bccr/cuentasambientales/DocCuentaEnergia/Cuenta_Energia_2011_2015.xlsx"

#download.file(path2, destfile = "data/energia.xlsx", mode = "wb")
hojas<-str_subset(excel_sheets("data/energia.xlsx"), "(USO.*)|(Emi)")
uso<-lapply(hojas, function(x) read_xlsx("data/energia.xlsx", sheet = x, col_names = T, skip = 4))
names(uso)<-hojas
uso<-lapply(uso, function(x) x[!is.na(x$AEE),])
indicador<-str_detect(names(uso), "USO")
uso<-list(uso=uso[indicador], emisiones=uso[!indicador])
uso<-lapply(uso, function(x) bind_rows(x, .id="ano"))
uso<-lapply(uso, function(x) x%>%mutate(ano=as.numeric(paste("20",str_extract(uso$uso$ano, "[:digit:]+"), sep=""))))

ciiu<-read_xlsx("data/energia.xlsx", sheet = 3, skip =1)
names(ciiu)[2]<-"CIIU"
ciiu<-ciiu[c("CIIU", "AEE", "AE")]
ciiu<-ciiu%>%group_by(CIIU, AEE)%>%filter(row_number(AEE)==1)

##Existen CIIUS que tiene al AEE035(Otros servicios) al mismo tiempo por lo que se repiten, por eso quedan 45. lo que hago es asignar las emisiones (y usos) de AEE035 a estos CIIUS y luego se agreagan los CIIUS para dejarlos únicos. Es por eso que ciiu va la izquierda en el left_join.####
##primero los pego
uso<-lapply(uso, function(x) left_join(ciiu, x, by="AEE"))
#segundo agrego las emsiones por CIIU
uso<-lapply(uso, function(x) x%>%group_by(ano, CIIU)%>%select(-contains("Actividad"), -AEE, -AE)%>%summarise_all(.funs = function(x) sum(as.numeric(x))))


#####






#con la base de pinky
base_pegada <- read_dta("data/base_pegada.dta")
names(base_pegada)[names(base_pegada) == 'region'] <- 'REGION'
#quitar zona
base_pegada<-base_pegada%>%group_by(REGION, ano, CIIU, CIIUsec)%>%select_at(vars( names(.)[!str_detect(names(.), "(basu)|(energy)|(zona)")]))%>%summarise_all(.funs=function(x) sum(x, na.rm = T))

base_pegada$REGION<-factor(base_pegada$REGION, labels=str_to_upper(names(attr(base_pegada$REGION, "labels"))))


#esta base indicador tiene el ponderador de cada region dentro de cada actividad
indicador<-left_join(base_pegada%>%group_by(ano, REGION,  CIIU)%>%summarise(HorNorPri=sum(HorNorPri, na.rm = T)),
                     base_pegada%>%group_by(ano, REGION, CIIUsec)%>%summarise(HorNorSec=sum(HorNorSec, na.rm = T)), by=c("ano", "REGION", "CIIU"="CIIUsec") )%>%mutate(HorNorSec=replace_na(data = HorNorSec, replace = 0))%>%group_by(ano, REGION, CIIU)%>%transmute(horas=HorNorSec+HorNorPri)%>%group_by(ano,CIIU)%>%mutate(total=sum(horas))%>%mutate(ponderacion=horas/total)

#ejemplo de como funcionan los ponderadores
indicador%>%filter(ano==2011 & str_detect(CIIU, "Enseñanza"))%>%mutate(acumulado=cumsum(ponderacion))%>%mutate(ponderacion*total)

#esta tiene el resto de variables por actividad y region
variables<-base_pegada%>%select(-HorNorPri, -HorNorSec, -HorTotNorm, -CIIUsec)%>%group_by(ano, REGION, CIIU)%>%summarise_all(.funs=function(x) sum(x, na.rm=T))%>%ungroup()

#aca pego indicador y variables
base_pegada<-left_join(indicador, variables, by=c("ano", "REGION", "CIIU"))



#hasta aquí están pegadas las bases del mister con los ponderadores calculados
#ahora pego las variables regionales con las variables de emision y de uso de energía, se genera una lista similar a "uso"

uso1<-lapply(uso, function(x) left_join(base_pegada, x, by=c("ano", "CIIU") ))

#multiplicar por la ponderación
uso1$uso<-uso1$uso%>%ungroup()%>%mutate_at(.vars = str_subset(names(.), "[^(ponderacion|REGION|CIIU|ano)]"), .funs = function(y) .$ponderacion*y)%>%select(-ponderacion)

uso1$emisiones<-uso1$emisiones%>%ungroup()%>%mutate_at(.vars = str_subset(names(.), "[^(ponderacion|REGION|CIIU|ano)]"), .funs = function(y) .$ponderacion*y)%>%select(-ponderacion)


#mapas
cr_canton <- readOGR("data/temp/cr_canton.shp", encoding = "UTF-8")

#redimensionar las tablas de variables para que queden por regiones
uso1<-lapply(uso1, function(x) x%>%gather(key="var", value = "valor", -ano, -CIIU, -REGION)%>%unite(col = "variable", c(ano, CIIU, var), sep="_")%>%spread(key=variable, value=valor))
cr_canton@data<-left_join(cr_canton@data,uso1$uso,  by=c("REGION"))

var_seleccion<- names(cr_canton@data)[str_detect(names(cr_canton@data), "(2015-)(Agricultura.+-)(Gasolina)")]

# qtm(shp = cr_canton[cr_canton$REGION=="CENTRAL"  & cr_canton$incidencia>20,], fill = "incidencia")
# 
# tm_shape(cr_canton[cr_canton$REGION=="CENTRAL",]) +
#   tm_grid(n.x = 11, n.y = 11, projection = "longlat") +
#   tm_fill(col = "pobres", style = "quantile")
# 
# 
pal <- colorNumeric("Blues", NULL, n = 6)

state_pop <- paste("CANTON: ", 
                   cr_canton$REGION , 
                   round(cr_canton[[var_seleccion]], 2), sep = " ")

mapa <- leaflet(data = cr_canton) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(as.numeric(cr_canton[[var_seleccion]])), weight = 2, opacity = 1,color = "white", layerId = ~id,
              dashArray = "3", fillOpacity = 0.7, highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE), label = state_pop,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))
mapa

library(visNetwork)
nodes <- data.frame(id = 1:6, title = paste("node", 1:6), 
                    shape = c("dot", "square"),
                    size = 10:15, color = c("blue", "red"))
edges <- data.frame(from = 1:5, to = c(5, 4, 6, 3, 3))
visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

# Use subset of data for more readable diagram
Flare$children = Flare$children[1:3]

radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)

