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
library(pgirmess)


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



ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_bel, x=0, y=60, width = 90, height = 60)+
  draw_plot(razao_bel, x=52, y=60, width = 45, height = 60)+
  draw_plot(cma_bho, x=3, y=0, width = 85, height = 56)+
  draw_plot(razao_bho, x=53, y=0, width = 42.5, height = 56)+
  draw_label("Belém", size = 18, fontface = "bold", x=87, y=90,angle = 270,fontfamily = "serif")+
  draw_label("Belo Horizonte", size = 18, fontface = "bold", x=87, y=30,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=34.7, y=120,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55.3, y=120,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=75.4, y=120,fontfamily = "serif")

ggsave2(filename="Fig6_pt1_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")



ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_bsb, x=7.5, y=45, width = 57, height = 37.6)+
  draw_plot(razao_bsb, x=63.5, y=45, width = 28.5, height = 37.6)+
  draw_plot(cma_cam, x=-1.5, y=0, width = 75, height = 49)+
  draw_plot(razao_cam, x=59, y=0, width = 37.5, height = 49)+
  draw_label("Brasília", size = 18, fontface = "bold", x=95, y=66,angle = 270,fontfamily = "serif")+
  draw_label("Campinas", size = 18, fontface = "bold", x=95, y=25,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=22.5, y=83.5,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=49, y=83.5,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=77.5, y=83.5,fontfamily = "serif")

ggsave2(filename="Fig7_pt2_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")



ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_cgr, x=10, y=70, width = 70, height = 46)+
  draw_plot(razao_cgr, x=63, y=70, width = 35, height = 46)+
  draw_plot(cma_duq, x=-10, y=0, width = 110, height = 73.7)+
  draw_plot(razao_duq, x=53, y=0, width = 55, height = 73.7)+
  draw_label("Campo Grande", size = 18, fontface = "bold", x=95, y=98,angle = 270,fontfamily = "serif")+
  draw_label("Duque de Caxias", size = 18, fontface = "bold", x=95, y=36,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=34.7, y=120,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=57, y=120,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=81, y=120,fontfamily = "serif")

ggsave2(filename="Fig8_pt3_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")




ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_goi, x=8, y=52, width = 70, height = 46)+
  draw_plot(razao_goi, x=63, y=52, width = 35, height = 46)+
  draw_plot(cma_gua, x=1.5, y=0, width = 83, height = 55)+
  draw_plot(razao_gua, x=60, y=0, width = 41.5, height = 55)+
  draw_label("Goiânia", size = 18, fontface = "bold", x=94, y=80,angle = 270,fontfamily = "serif")+
  draw_label("Guarulhos", size = 18, fontface = "bold", x=94, y=30,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=30, y=100,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=55, y=100,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=80, y=100,fontfamily = "serif")

ggsave2(filename="Fig9_pt4_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")





ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_mac, x=-5.5, y=46, width = 100, height = 66)+
  draw_plot(razao_mac, x=57.7, y=46, width = 50, height = 66)+
  draw_plot(cma_man, x=10, y=0, width = 70, height = 46)+
  draw_plot(razao_man, x=64.5, y=0, width = 35, height = 46)+
  draw_label("Maceió", size = 18, fontface = "bold", x=97, y=82,angle = 270,fontfamily = "serif")+
  draw_label("Manaus", size = 18, fontface = "bold", x=97, y=27,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=31.5, y=113,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=56.5, y=113,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=82, y=113,fontfamily = "serif")

ggsave2(filename="Fig10_pt5_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")





ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_nat, x=-1, y=58, width = 94, height = 62.6)+
  draw_plot(razao_nat, x=56.5, y=58, width = 47, height = 62.6)+
  draw_plot(cma_poa, x=1, y=0, width = 90, height = 60)+
  draw_plot(razao_poa, x=57.5, y=0, width = 45, height = 60)+
  draw_label("Natal", size = 18, fontface = "bold", x=93, y=92,angle = 270,fontfamily = "serif")+
  draw_label("Porto Alegre", size = 18, fontface = "bold", x=93, y=32,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=35, y=121,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=56, y=121,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=79, y=121,fontfamily = "serif")

ggsave2(filename="Fig11_pt6_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")





ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_rec, x=-17.5, y=37.5, width = 125, height = 83.3)+
  draw_plot(razao_rec, x=58.5, y=37.5, width = 62.5, height = 83.3)+
  draw_plot(cma_rio, x=15, y=0, width = 60, height = 40)+
  draw_plot(razao_rio, x=74.5, y=0, width = 30, height = 40)+
  draw_label("Recife", size = 18, fontface = "bold", x=106, y=75,angle = 270,fontfamily = "serif")+
  draw_label("Rio de Janeiro", size = 18, fontface = "bold", x=106, y=24.5,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=30, y=120,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=59, y=120,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=88, y=120,fontfamily = "serif")

ggsave2(filename="Fig12_pt7_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")




ggdraw(xlim = c(0,135), ylim = c(0,125))+
  draw_plot(cma_sal, x=0, y=53, width = 75, height = 50)+
  draw_plot(razao_sal, x=67.5, y=53, width = 37.5, height = 50)+
  draw_plot(cma_sgo, x=-4, y=0, width = 83, height = 55.3)+
  draw_plot(razao_sgo, x=65.5, y=0, width = 41.5, height = 55.3)+
  draw_label("Salvador", size = 18, fontface = "bold", x=103.5, y=80,angle = 270,fontfamily = "serif")+
  draw_label("São Gonçalo", size = 18, fontface = "bold", x=103.5, y=29,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=22, y=105,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=54, y=105,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=85.5, y=105,fontfamily = "serif")

ggsave2(filename="Fig13_pt8_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")






ggdraw(xlim = c(0,135), ylim = c(0,65))+
  draw_plot(cma_slz, x=0, y=0, width = 80, height = 53.3)+
  draw_plot(razao_slz, x=79, y=0, width = 40, height = 53.3)+
  draw_label("São Luís", size = 18, fontface = "bold", x=122, y=30,angle = 270,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Fluxo Livre)", size = 18, fontface = "bold", x=21, y=52,fontfamily = "serif")+
  draw_label("Acessibilidade\n(Pico)", size = 18, fontface = "bold", x=60, y=52,fontfamily = "serif")+
  draw_label("Impacto do\ncongestionamento", size = 18, fontface = "bold", x=98, y=52,fontfamily = "serif")

ggsave2(filename="Fig14_pt9_v2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 55, height = 30, units = "cm")

