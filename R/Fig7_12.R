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
  
  df <- df %>%
    sf::st_transform(3857)
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  if(cidade == "cgr"){
    
    w=-54.9
    s=-20.8
    e=-54.2
    n=-20.2
    
    test <- bbox2sf(n,s,e,w)
    
    test <- st_transform(test, 3857)
    
    lon = st_coordinates(test)[,1]
    lat = st_coordinates(test)[,2]
   
  }else if(cidade == "man"){
    
    w=-60.2
    s=-3.2
    e=-59.8
    n=-2.85
    
    test <- bbox2sf(n,s,e,w)
    
    test <- st_transform(test, 3857)
    
    lon = st_coordinates(test)[,1]
    lat = st_coordinates(test)[,2]
  }
  
  
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
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA)+
    labs(title = "Acessibilidade",
         fill = "Empregos\n(x10.000)") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom", strip.text.x = element_blank())
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none", strip.text.x = element_blank())
    }
  
  if(cidade %in% c("cgr", "man")){
    mapa_renda <- mapa_renda + coord_sf(xlim = c(min(lon), max(lon)), ylim=c(min(lat), max(lat)), expand = FALSE)}
  
  return(mapa_renda)
  
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
  
  
  if(cidade == "cgr"){
    
    w=-54.9
    s=-20.8
    e=-54.2
    n=-20.2
    
    test <- bbox2sf(n,s,e,w)
    
    test <- st_transform(test, 3857)
    
    lon = st_coordinates(test)[,1]
    lat = st_coordinates(test)[,2]
    
    }else if(cidade == "man"){
    
    w=-60.2
    s=-3.2
    e=-59.8
    n=-2.85
    
    test <- bbox2sf(n,s,e,w)
    
    test <- st_transform(test, 3857)
    
    lon = st_coordinates(test)[,1]
    lat = st_coordinates(test)[,2]
  }
  
  
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
  
  if(cidade %in% c("cgr", "man")){
    mapa_renda <- mapa_renda + coord_sf(xlim = c(min(lon), max(lon)), ylim=c(min(lat), max(lat)), expand = FALSE)}
  
  return(mapa_renda)
  
}

######## Rodando mapas
cma_bel <- CMA("bel","sim",15,45)
cma_bho <- CMA("bho","sim",15,45)
cma_bsb <- CMA("bsb","sim",15,45)
cma_cam <- CMA("cam","sim",15,45)
cma_cgr <- CMA("cgr","sim",15,45)
cma_duq <- CMA("duq","sim",15,45)
cma_goi <- CMA("goi","sim",15,45)
cma_gua <- CMA("gua","sim",15,45)
cma_mac <- CMA("mac","sim",15,45)
cma_man <- CMA("man","sim",15,45)
cma_nat <- CMA("nat","sim",15,45)
cma_poa <- CMA("poa","sim",15,45)
cma_rec <- CMA("rec","sim",15,45)
cma_rio <- CMA("rio","sim",15,45)
cma_sal <- CMA("sal","sim",15,45)
cma_sgo <- CMA("sgo","sim",15,45)
cma_slz <- CMA("slz","sim",15,45)


razao_bel <- CMA_razao("bel","sim",15,45)
razao_bho <- CMA_razao("bho","sim",15,45)
razao_bsb <- CMA_razao("bsb","sim",15,45)
razao_cam <- CMA_razao("cam","sim",15,45)
razao_cgr <- CMA_razao("cgr","sim",15,45)
razao_duq <- CMA_razao("duq","sim",15,45)
razao_goi <- CMA_razao("goi","sim",15,45)
razao_gua <- CMA_razao("gua","sim",15,45)
razao_mac <- CMA_razao("mac","sim",15,45)
razao_man <- CMA_razao("man","sim",15,45)
razao_nat <- CMA_razao("nat","sim",15,45)
razao_poa <- CMA_razao("poa","sim",15,45)
razao_rec <- CMA_razao("rec","sim",15,45)
razao_rio <- CMA_razao("rio","sim",15,45)
razao_sal <- CMA_razao("sal","sim",15,45)
razao_sgo <- CMA_razao("sgo","sim",15,45)
razao_slz <- CMA_razao("slz","sim",15,45)



ggdraw(xlim = c(0,135), ylim = c(0,185))+
  draw_plot(cma_bel, x=0, y=100, width = 90, height = 60)+
  draw_plot(razao_bel, x=53, y=100, width = 45, height = 60)+
  draw_plot(cma_bho, x=0, y=40, width = 90, height = 60)+
  draw_plot(razao_bho, x=53, y=40, width = 45, height = 60)+
  draw_plot(cma_bsb, x=20, y=0, width = 50, height = 33)+
  draw_plot(razao_bsb, x=51, y=0, width = 50, height = 33)+
  draw_label("Belém", size = 18, fontface = "bold", x=90, y=139,angle = 270,fontfamily = "serif")+
  draw_label("Belo Horizonte", size = 18, fontface = "bold", x=90, y=77,angle = 270,fontfamily = "serif")+
  draw_label("Brasília", size = 18, fontface = "bold", x=90, y=22,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=34.7, y=162,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55.3, y=162,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=75.4, y=162,fontfamily = "serif")

ggsave2(filename="Fig4_pt1.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")



ggdraw(xlim = c(0,135), ylim = c(0,185))+
  draw_plot(cma_cam, x=10, y=120, width = 70, height = 46)+
  draw_plot(razao_cam, x=63, y=120, width = 34, height = 46)+
  draw_plot(cma_cgr, x=10, y=75, width = 70, height = 46)+
  draw_plot(razao_cgr, x=62, y=75, width = 35, height = 46)+
  draw_plot(cma_duq, x=-2, y=10, width = 100, height = 67)+
  draw_plot(razao_duq, x=55, y=10, width = 50, height = 67)+
  draw_label("Campinas", size = 18, fontface = "bold", x=97, y=148,angle = 270,fontfamily = "serif")+
  draw_label("Campo Grande", size = 18, fontface = "bold", x=97, y=102,angle = 270,fontfamily = "serif")+
  draw_label("Duque de Caxias", size = 18, fontface = "bold", x=97, y=46,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=33.5, y=170,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55.5, y=170,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=82, y=170,fontfamily = "serif")

ggsave2(filename="Fig4_pt2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")
  
  
  
ggdraw(xlim = c(0,135), ylim = c(0,175))+
  draw_plot(cma_goi, x=8, y=115, width = 70, height = 46)+
  draw_plot(razao_goi, x=65, y=115, width = 35, height = 46)+
  draw_plot(cma_gua, x=3, y=60, width = 80, height = 53)+
  draw_plot(razao_gua, x=60, y=60, width = 40, height = 53)+
  draw_plot(cma_mac, x=0, y=0, width = 90, height = 60)+
  draw_plot(razao_mac, x=57, y=0, width = 45, height = 60)+
  draw_label("Goiânia", size = 18, fontface = "bold", x=97, y=146,angle = 270,fontfamily = "serif")+
  draw_label("Guarulhos", size = 18, fontface = "bold", x=97, y=88,angle = 270,fontfamily = "serif")+
  draw_label("Maceió", size = 18, fontface = "bold", x=97, y=32,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=30, y=165,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=54, y=165,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=82, y=165,fontfamily = "serif")

ggsave2(filename="Fig4_pt3.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")
  
  
  
ggdraw(xlim = c(0,135), ylim = c(0,185))+
  draw_plot(cma_man, x=10, y=120, width = 70, height = 46)+
  draw_plot(razao_man, x=62, y=120, width = 35, height = 46)+
  draw_plot(cma_nat, x=1, y=60, width = 90, height = 60)+
  draw_plot(razao_nat, x=57, y=60, width = 45, height = 60)+
  draw_plot(cma_poa, x=1, y=0, width = 90, height = 60)+
  draw_plot(razao_poa, x=57, y=0, width = 45, height = 60)+
  draw_label("Manaus", size = 18, fontface = "bold", x=97, y=150,angle = 270,fontfamily = "serif")+
  draw_label("Natal", size = 18, fontface = "bold", x=97, y=95,angle = 270,fontfamily = "serif")+
  draw_label("Porto Alegre", size = 18, fontface = "bold", x=97, y=35,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=33.5, y=180,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55.5, y=180,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=77, y=180,fontfamily = "serif")

ggsave2(filename="Fig4_pt4.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")
  
  
  
  
  
ggdraw(xlim = c(0,135), ylim = c(0,135))+
  draw_plot(cma_rec, x=0, y=65, width = 90, height = 60)+
  draw_plot(razao_rec, x=67, y=65, width = 45, height = 60)+
  draw_plot(cma_rio, x=15, y=30, width = 60, height = 40)+
  draw_plot(razao_rio, x=75, y=30, width = 30, height = 40)+
  draw_plot(cma_sal, x=15, y=0, width = 60, height = 40)+
  draw_plot(razao_sal, x=75, y=0, width = 30, height = 40)+
  draw_label("Recife", size = 18, fontface = "bold", x=110, y=95,angle = 270,fontfamily = "serif")+
  draw_label("Rio de Janeiro", size = 18, fontface = "bold", x=110, y=54,angle = 270,fontfamily = "serif")+
  draw_label("Salvador", size = 18, fontface = "bold", x=110, y=23,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=33.5, y=130,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=58, y=130,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=88, y=130,fontfamily = "serif")

ggsave2(filename="Fig4_pt5.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")
  
  
  
  
ggdraw(xlim = c(0,135), ylim = c(0,135))+
  draw_plot(cma_sgo, x=11, y=60, width = 70, height = 46.6)+
  draw_plot(razao_sgo, x=80, y=60, width = 35, height = 46.6)+
  draw_plot(cma_slz, x=1, y=0, width = 90, height = 60)+
  draw_plot(razao_slz, x=75, y=0, width = 45, height = 60)+
  draw_label("São Gonçalo", size = 18, fontface = "bold", x=120, y=85,angle = 270,fontfamily = "serif")+
  draw_label("São Luís", size = 18, fontface = "bold", x=120, y=30,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=27, y=110,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=61, y=110,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=96, y=110,fontfamily = "serif")

ggsave2(filename="Fig4_pt6.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")

