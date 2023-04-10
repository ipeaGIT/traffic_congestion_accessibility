##################################################################################################################
############################# SCRIPT PARA CRIAR MAPAS DE TEMPO MINIMO E CMA POR TPC ##############################
##################################################################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
library(cowplot)
library(purrr)
library(ggsn)
library(BAMMtools) 
library(stringi)
library(ggspatial)
library(accessibility)


####### Mapa Acessibilidade cumulativa intervalo ######

CMA <- function(cidade, legenda, mintempo,maxtempo){
  
  setwd("//storage6/usuarios/Proj_acess_oport/git_diego/congestionamento/04AM")
  
  landuse <- aopdata::read_landuse(city=cidade) %>% dplyr::select(id_hex, T001) %>% rename("id" = id_hex)
  
  free <- fread(paste0("OD_TI_", cidade, ".csv")) %>%
    dplyr::select(origin_hex, destination_hex, Total_Time_04AM) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  df_free <- cumulative_interval(travel_matrix = free,
                                 land_use_data = landuse,
                                 interval = c(mintempo,maxtempo),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "Total_Time_04AM")
  
  df_free <- df_free %>% 
    mutate(medida = "Fluxo livre")
  
  geometria <- aopdata::read_grid(city=cidade) %>% dplyr::select(id_hex,geom)
  
  df_geom <- dplyr::left_join(df_free, geometria, by=c("id"="id_hex")) %>% filter(!is.na("T001"), T001> 0)
  df_geom <- st_as_sf(df_geom)%>%
    st_transform(3857) 
  
  #### peak #####
  
  setwd("C:/Users/b35143921880/Downloads/peak")
  
  peak <- fread(paste0("OD_TI_",cidade,"_final.csv")) %>%
    dplyr::select(origin_hex, destination_hex, median_morning_peak) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  df_peak <- cumulative_interval(travel_matrix = peak,
                                 land_use_data = landuse,
                                 interval = c(mintempo,maxtempo),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "median_morning_peak")
  
  
  df_peak <- df_peak %>% 
    mutate(medida = "Pico")
  
  df_peak <- left_join(df_free[,1], df_peak, by="id")
  
  df_geom_peak <- dplyr::left_join(df_peak, geometria, by=c("id"="id_hex")) %>% filter(!is.na("T001"), T001> 0)
  df_geom_peak <- st_as_sf(df_geom_peak)%>%
    st_transform(3857) 
  
  
  df <- rbind(df_geom, df_geom_peak)
  
  df <- df%>%
      mutate(T001_1000 = T001/10000)
  
  # if(cidade != "spo"){
  #   df <- df %>%
  #     mutate(T001_1000 = case_when(T001_1000>40 ~ 40,
  #                                  T001_1000 <40 ~ T001_1000))
  # }
  
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=df, aes(fill=T001_1000), color=NA, alpha=1) +
    facet_grid(~medida)+
    viridis::scale_fill_viridis(
      direction = 1
      , option = "cividis"
      #, limits = c(1, 80)
      #, breaks = c(1,150000,300000,450000,600000)
      #, labels = c("1","150","300","450","600")
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "Acessibilidade",
         fill = "Empregos\n(x10.000)") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom", strip.text.x = element_blank())
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }
}

####### Mapa Acessibilidade cumulativa intervalo - hora pico ######

CMA_razao <- function(cidade, legenda, mintempo,maxtempo){
 
  setwd("//storage6/usuarios/Proj_acess_oport/git_diego/congestionamento/04AM")
  
  landuse <- aopdata::read_landuse(city=cidade) %>% dplyr::select(id_hex, T001) %>% rename("id" = id_hex)
  
  free <- fread(paste0("OD_TI_", cidade, ".csv")) %>%
    dplyr::select(origin_hex, destination_hex, Total_Time_04AM) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  df_free <- cumulative_interval(travel_matrix = free,
                                 land_use_data = landuse,
                                 interval = c(mintempo,maxtempo),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "Total_Time_04AM")
  
  df_free <- df_free %>% 
    mutate(medida = "Fluxo livre")
  
  geometria <- aopdata::read_grid(city=cidade) %>% dplyr::select(id_hex,geom)
  
  #### peak #####
  
  setwd("C:/Users/b35143921880/Downloads/peak")
  
  peak <- fread(paste0("OD_TI_",cidade,"_final.csv")) %>%
    dplyr::select(origin_hex, destination_hex, median_morning_peak) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  df_peak <- cumulative_interval(travel_matrix = peak,
                                 land_use_data = landuse,
                                 interval = c(mintempo,maxtempo),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "median_morning_peak")
  
  
  df_peak <- df_peak %>% 
    mutate(medida = "Pico")
  
  df_peak <- left_join(df_free[,1], df_peak, by="id")
  
  df_razao <- dplyr::left_join(df_peak, df_free, by="id") %>%
    mutate(T001 = 1-(T001.x/T001.y),
           medida = "razão") %>%
    select("id", "T001", "medida")

  df_razao <- df_razao %>%
    mutate(T001 = case_when(T001 >0.5 ~ 0.5,
                            T001 <0.5 ~ T001))
  
  df_geom_razao <- dplyr::left_join(df_razao, geometria, by=c("id"="id_hex")) %>% filter(!is.na("T001"), T001> 0)
  df_geom_razao <- st_as_sf(df_geom_razao)%>%
    st_transform(3857)
  
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=df_geom_razao, aes(fill=T001), color=NA, alpha=1) +
    facet_grid(~medida)+
    viridis::scale_fill_viridis(
      direction = 1
      , option = "inferno"
      , limits = c(0, 0.5)
      , breaks = c(0, 0.25, 0.5)
      , labels = c("0%", "25%",">50%")
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "Acessibilidade",
         fill = "Percentual") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom", strip.text.x = element_blank())
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }
   
}

######## Rodando mapas
cma_for <- CMA("for","sim",15,45)
razao_for <- CMA_razao("for","sim",15,45)


cma_cur <- CMA("cur","sim",15,45)
razao_cur <- CMA_razao("cur","sim",15,45)


cma_spo <- CMA("spo","sim",15,45)
razao_spo <- CMA_razao("spo","sim",15,45)



ggdraw(xlim = c(0,135), ylim = c(0,165))+
  draw_plot(cma_cur, x=0, y=100, width = 90, height = 60)+
  draw_plot(razao_cur, x=54.6, y=100, width = 45, height = 60)+
  draw_plot(cma_for, x=16, y=60, width = 55, height = 40)+
  draw_plot(razao_for, x=66.3, y=60, width = 27, height = 40)+
  draw_plot(cma_spo, x=0, y=0, width = 90, height = 60)+
  draw_plot(razao_spo, x=57, y=0, width = 45, height = 60)+
  draw_label("Curitiba", size = 18, fontface = "bold", x=97, y=140,angle = 270,fontfamily = "serif")+
  draw_label("Fortaleza", size = 18, fontface = "bold", x=97, y=85,angle = 270,fontfamily = "serif")+
  draw_label("São Paulo", size = 18, fontface = "bold", x=97, y=35,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=33.5, y=162,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55.5, y=162,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=77, y=162,fontfamily = "serif")

ggsave2(filename="Fig4.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")

