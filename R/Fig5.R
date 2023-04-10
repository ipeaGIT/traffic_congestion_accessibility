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


############ Distribuição da População #############


pop <- function(cidade, legenda){
  landuse <- aopdata::read_landuse(city=cidade, geometry = T) %>% 
    filter(P001>0)%>%
    mutate(P001 = P001/1000) %>%
    dplyr::select(id_hex, P001, geometry)%>%
    st_transform(3857)
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_pop <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=landuse, aes(fill=P001), color=NA, alpha=1) +
    viridis::scale_fill_viridis(
      direction = 1
      , option = "inferno"
      #, limits = c(0, 8000)
      #, breaks = c(0, 4000, 8000)
      #, labels = c("0", "4000","8000")
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "População",
         fill = "Habitantes\n(x1.000)") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }
}


############### pop por quintil ####################

pop_quintil <- function(cidade, legenda){
  landuse <- aopdata::read_landuse(city=cidade, geometry = T) %>% 
    filter(P001>0)%>%
    mutate(quintil = as.character(R002)) %>%
    dplyr::select(id_hex, quintil, geometry)%>%
    st_transform(3857)
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_pop <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=landuse, aes(fill=quintil), color=NA, alpha=1) +
    scale_fill_viridis_d(
      breaks = c("1","2","3","4","5")
      , direction = 1
      , option = "viridis"
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "População",
         fill = "Quintil") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }
}




############ Distribuição dos Empregos #############

emp <- function(cidade, legenda){
  landuse <- aopdata::read_landuse(city=cidade, geometry = T) %>% 
    filter(T001>0)%>%
    mutate(T001 = T001/1000)%>%
    dplyr::select(id_hex, T001, geometry)%>%
    st_transform(3857)
  
  if(cidade!="spo"){
    landuse <- landuse %>%
      mutate(T001 = case_when(T001>4 ~ 4,
                              T001 <4 ~ T001))
  }else{landuse <- landuse %>%
    mutate(T001 = case_when(T001>7 ~ 7,
                            T001 <7 ~ T001))}
  
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_emp <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=landuse, aes(fill=T001), color=NA, alpha=1) +
    viridis::scale_fill_viridis(
      direction = 1
      , option = "viridis"
      #, limits = c(0, 8000)
      #, breaks = c(0, 2000, 4000, 6000, 8000)
      #, labels = c("0", "2", "4","6", "+8")
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "Empregos",
         fill = "Empregos\n(x1.000)") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }
}


######## Rodando mapas

pop_cur <- pop("cur","sim")
pop_quintil_cur <- pop_quintil("cur","sim")
emp_cur <- emp("cur","sim")


pop_for <- pop("for","sim")
pop_quintil_for <- pop_quintil("for","sim")
emp_for <- emp("for","sim")


pop_spo <- pop("spo","sim")
pop_quintil_spo <- pop_quintil("spo","sim")
emp_spo <- emp("spo","sim")


ggdraw(xlim = c(0,135), ylim = c(0,165))+
  draw_plot(pop_cur, x=11, y=100, width = 45, height = 60)+
  draw_plot(pop_quintil_cur, x=38.5, y=100, width = 45, height = 60)+
  draw_plot(emp_cur, x=65.5, y=100, width = 45, height = 60)+
  
  draw_plot(pop_for, x=11, y=60, width = 45, height = 40)+
  draw_plot(pop_quintil_for, x=38.5, y=60, width = 45, height = 40)+
  draw_plot(emp_for, x=65.5, y=60, width = 45, height = 40)+
  
  draw_plot(pop_spo, x=11, y=0, width = 45, height = 60)+
  draw_plot(pop_quintil_spo, x=38.5, y=0, width = 45, height = 60)+
  draw_plot(emp_spo, x=65.5, y=0, width = 45, height = 60)+
  
  draw_label("Curitiba", size = 18, fontface = "bold", x=105, y=140,angle = 270,fontfamily = "serif")+
  draw_label("Fortaleza", size = 18, fontface = "bold", x=105, y=85,angle = 270,fontfamily = "serif")+
  draw_label("São Paulo", size = 18, fontface = "bold", x=105, y=35,angle = 270,fontfamily = "serif")+
  draw_label("População", size = 18, fontface = "bold", x=35, y=162,fontfamily = "serif")+
  draw_label("Renda", size = 18, fontface = "bold", x=60.5, y=162,fontfamily = "serif")+
  draw_label("Empregos", size = 18, fontface = "bold", x=87.5, y=162,fontfamily = "serif")


ggsave2(filename="Fig5_congestionamento_v3.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")

