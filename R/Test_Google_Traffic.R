
library(dplyr)
library(aopdata)
library(googletraffic)
library(mapview)
library(raster)
library(sf)
library(ggplot2)
library(leaflet)
library(leaflet.providers)

### Set API Key
#google_key <- ""


google_traffic <- function(cidade){

  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  df <- aopdata::read_grid(cidade) %>%
    group_by(abbrev_muni) %>%
    summarise()
  
  df <- df %>%
    st_transform(4326)
  
  r <- gt_make_raster_from_polygon(polygon = df,
                                   zoom = 12,
                                   google_key = google_key)
  
  r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
  names(r_df) <- c("value", "x","y")
  
  temp <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_raster(data = r_df, 
                aes(x = x, y = y, 
                    fill = as.factor(value))) +
    labs(fill = "Traffic\nLevel") +
    scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
    coord_quickmap() + 
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color="white"))
  
  return(temp)
  
}

spo <- google_traffic("spo")
cur <- google_traffic("cur")
for_ <- google_traffic("for")


