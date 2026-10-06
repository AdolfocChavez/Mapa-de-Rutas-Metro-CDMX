#Abrimos las librerias que necesitamos 
library(sf)
library(osmdata)
library(sfnetworks)
library(ggplot2)
library(tidygraph)

#Decargamos los mapas del metro 
#Existen en un solo archivo Zip tanto Estaciones como Lineas
temp <- tempfile()
download.file("https://datos.cdmx.gob.mx/dataset/1b014317-ddb1-46c7-ac79-7330c652abe3/resource/288b10dd-4f21-4338-b1ed-239487820512/download/288b10dd-4f21-4338-b1ed-239487820512.zip",temp, mode ="wb")
archivos <- unzip(temp, list = TRUE)$Name

#Sacamos las lineas 
archivos_lineas <- archivos[grepl("STC_Metro_lineas_utm14n\\.", archivos, ignore.case = TRUE)]
unzip(temp, files = archivos_lineas, exdir = "stcmetro_shp", junkpaths = TRUE)
lineas <- st_read(file.path("stcmetro_shp", "STC_Metro_lineas_utm14n.shp"))

#Sacamos las estaciones 
archivos_estaciones<- archivos[grepl("STC_Metro_estaciones_utm14n\\.", archivos, ignore.case = TRUE)]
unzip(temp, files = archivos_estaciones, exdir = "stcmetro_shp", junkpaths = TRUE)
estaciones <- st_read(file.path("stcmetro_shp", "STC_Metro_estaciones_utm14n.shp"))

#Estas lineas estan por que a veces es mas facil descargar y descomprimir los mapas localmente 
#Jalamos los mapas de estaciones y lineas del metro 
#lineas <- st_read("C:/Users/adolf/OneDrive/Documents/Mapa Metro/Estaciones y Lineas/STC_Metro_lineas_utm14n.shp")
#estaciones <- st_read("C:/Users/adolf/OneDrive/Documents/Mapa Metro/Estaciones y Lineas/STC_Metro_estaciones_utm14n.shp")

#corregimos un detalle en LINEA en en el df lineas 
lineas <- lineas |>
  mutate(LINEA = if_else(str_detect(LINEA, "^[1-9]$"),
                         paste0("0", LINEA),
                         LINEA))

#Vemos el mapa incial 
ggplot()+
  geom_sf(data = lineas,
          aes(color = LINEA))+
  geom_sf(data = estaciones,
          aes(color = LINEA))

#Ajustamos las coordenadas en estaciones 
estaciones_m <- st_transform(estaciones, 32614) |>
  mutate(EST_num = as.integer(EST),
         node_id = row_number())

#creamos aristas que sirvan como proxy de las vias va a trazar una linea entre cada estacion 
#hacemos las lista de relacion de las aristas de estaciones 
aristas_linea <- estaciones_m |>
  st_drop_geometry() |>
  arrange(LINEA, EST_num) |>
  group_by(LINEA) |>
  mutate(from = node_id, to = lead(node_id)) |>
  ungroup() |>
  filter(!is.na(to)) |>
  transmute(from, to, LINEA, tipo_arista = "via")

#Hacemos aristas que sirvan de transbordo 
#creamos la lista de las estaciones
tabla_nodos <- estaciones_m |> st_drop_geometry() |> select(node_id, NOMBRE)

#creamos la lista de aristas de transborde 
aristas_transbordo <- tabla_nodos |>
  inner_join(tabla_nodos, by = "NOMBRE", suffix = c("_a", "_b")) |>
  filter(node_id_a < node_id_b) |>
  transmute(from = node_id_a, to = node_id_b, LINEA = NA_character_, tipo_arista = "transbordo")

#Juntamos las listas de aristas en un solo df 
aristas <- bind_rows(aristas_linea, aristas_transbordo)

#Creamos la geometria de las aristas 
coords_mat <- st_coordinates(estaciones_m)[, c("X", "Y")]
geom_list <- lapply(seq_len(nrow(aristas)), function(i) {
  st_linestring(coords_mat[c(aristas$from[i], aristas$to[i]), ])
})
aristas_sf <- aristas |>
  mutate(geometry = st_sfc(geom_list, crs = st_crs(estaciones_m))) |>
  st_as_sf()

#creamos la red 
red <- sfnetwork(
  nodes = estaciones_m |> select(node_id, NOMBRE, LINEA, TIPO, geometry),
  edges = aristas_sf,
  node_key = "node_id",
  directed = FALSE)

#verificamos los componentes
red <- red |> 
  activate(nodes) |> 
  mutate(componente = group_components())
red |> 
  activate(nodes) |> 
  st_as_sf() |> 
  count(componente)
#aqui nos debe de dar 1 bajo la columna componente 

#Agregamos pesos a las aristas para que el calculo sea mas realista y agregue peso a los transbordos 
velocidad_tren_kmh <- 35        
velocidad_transbordo_kmh <- 3   
minimo_transbordo_min <- 3      

red <- red |>
  activate("edges") |>
  mutate(dist_m = as.numeric(edge_length()), 
         tiempo_min = case_when(tipo_arista == "via"        ~ dist_m / (velocidad_tren_kmh * 1000 / 60),
                                tipo_arista == "transbordo" ~ (dist_m / (velocidad_transbordo_kmh * 1000 / 60)) + minimo_transbordo_min),
         weight = tiempo_min)
#Hasta aqui es la creacion de la red del metro con pesos. Esta parte solo se necesito hacer una vez. 


#Ya tenemos el mapa ya podemos preguntar como llegar de una estacion a otra 
#Debemos de crear los nodos de origen y destino 
origen <- estaciones_m |> 
  filter(NOMBRE == "Talismán", LINEA == "04") |> #Esta es la estacion de origen
  pull(node_id)

destino <- estaciones_m |> 
  filter(NOMBRE == "Tacuba", LINEA == "02") |> #Esta es la estacion de destino
  pull(node_id)

#Le pedimos la ruta mas corta 
paths <- st_network_paths(red, from = origen, to = destino, weights = "weight")

ruta_edges_sf <- red |>
  activate("edges") |>
  slice(paths$edge_paths[[1]]) |>
  st_as_sf()

ruta_nodes_sf <- red |>
  activate("nodes") |>
  slice(paths$node_paths[[1]]) |>
  st_as_sf()

#Sacamos la ruta en instrucciones 
node_ids <- paths$node_paths[[1]]

ruta_tabla <- red |>
  activate("nodes") |>
  st_as_sf() |>
  st_drop_geometry() |>
  slice(node_ids) |>
  mutate(orden = row_number(),
         cambio_de_linea = LINEA != lag(LINEA)) |> #Si linea no es igual al anterior linea 
  select(orden, NOMBRE, LINEA, TIPO, cambio_de_linea)

instrucciones <- ruta_tabla |>
  mutate(tramo = cumsum(replace_na(cambio_de_linea, FALSE))+1) |>  
  group_by(tramo, LINEA) |> 
  summarise(desde = first(NOMBRE), hasta = last(NOMBRE), .groups = "drop")
print(instrucciones, n = Inf)

# Visualizar la ruta
ggplot() +
  geom_sf(data = lineas, aes(color = LINEA))+
  geom_sf(data = estaciones, aes(color = LINEA))+
  geom_sf(data = ruta_edges_sf, color = "red", linewidth = 2)+
  geom_sf(data = ruta_nodes_sf, color = "black", size = 2)
