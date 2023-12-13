

library(accessibility)
library(dplyr)
library(data.table)
library(aopdata)
library(tidyverse)
library(mapview)
library(ggplot2)
library(scales)

setDTthreads(percent = 100)

cidades <- c("bel", "bho","bsb","cam","cgr","cur","duq","for","goi","gua","mac","man","nat","poa","rec","rio","sal","sgo","slz","spo")

acesso <- function(cidade){
  
  setwd("//storage6/usuarios/Proj_acess_oport/git_diego/congestionamento/04AM")
  
  free <- fread(paste0("OD_TI_", cidade, ".csv")) %>%
    dplyr::select(origin_hex, destination_hex, Total_Time_04AM) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  setwd("C:/Users/b35143921880/Downloads/peak")
  
  peak <- fread(paste0("OD_TI_",cidade,"_final.csv")) %>%
    filter(origin_hex %in% free$from_id,
           destination_hex %in% free$to_id) %>%
    dplyr::select(origin_hex, destination_hex, median_morning_peak) %>%
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)
  
  landuse <- aopdata::read_landuse(city=cidade) %>% dplyr::select(id_hex, T001) %>% rename("id" = id_hex)
  
  df_04AM <- cumulative_interval(travel_matrix = free,
                                 land_use_data = landuse,
                                 interval = c(15,45),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "Total_Time_04AM")
  
  df_04AM <- df_04AM %>% rename("T001_04AM"= T001)
  
    
  df_peak <- cumulative_interval(travel_matrix = peak,
                                 land_use_data = landuse,
                                 interval = c(15,45),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "median_morning_peak")

  df_peak <- df_peak %>% rename("T001_peak"= T001)
  
  
  df <- dplyr::left_join(df_04AM, df_peak, by="id")
  
  pop <- aopdata::read_population(city = cidade) %>% filter(P001 >0) %>% dplyr::select(id_hex,P001)
  
  
  acc <- dplyr::left_join(pop, df, by=c("id_hex"="id"))
  
  acc[is.na(acc)] <- 0
  
  acc <- acc %>%
    group_by() %>%
    summarise(mean_CMATT_free = weighted.mean(T001_04AM, P001/1000),
              mean_CMATT_peak = weighted.mean(T001_peak, P001/1000))
  
  acc_cast <- as.data.frame(t(acc)) 
  acc_cast <-acc_cast %>%
    mutate(medida = row.names(acc_cast),
           cid = cidade) %>%
    rename("Jobs" = V1)
  
  row.names(acc_cast) <- c(seq(1:nrow(acc_cast)))
  
  return(acc_cast)
  
  }


for(i in (1:length(cidades))){
  resultado <- acesso(cidades[i])
  if(i==1){resultado_final <- resultado}
  else(resultado_final <- rbind(resultado_final,resultado))
}

rm(resultado)

resultado_final_v2 <- resultado_final 

resultado_final_v2 <-resultado_final_v2 %>%
  mutate(grupo = paste0(substring(medida, 12, nchar(medida))))
# 
# resultado_final_v2 <- resultado_final_v2 %>%
#   mutate(concat = paste0(cid,"_", limite))

resultado_final_v2_peak <- resultado_final_v2 %>%
  filter(grupo == "peak")

resultado_final_v2_free <- resultado_final_v2 %>%
  filter(grupo == "free")

resultado_final_v3 <- dplyr::left_join(resultado_final_v2_peak, resultado_final_v2_free, by="cid") %>%
  mutate(razao = 1-(Jobs.x/Jobs.y))

resultado_final_v3 <- resultado_final_v3 %>%
  dplyr::select(medida.x,cid, razao)

resultado_final_v3 <- resultado_final_v3 %>%
  rename("medida" = medida.x)

resultado_final_v3$cid <- recode_factor(resultado_final_v3$cid, "bel"="Belém",
                                                                    "bho"="Belo Horizonte",
                                                                    "bsb"="Brasília",
                                                                    "cam"="Campinas",
                                                                    "cgr"="Campo Grande",
                                                                    "cur"="Curitiba",
                                                                    "duq"="Duque de Caxias",
                                                                    "for"="Fortaleza",
                                                                    "goi"="Goiânia",
                                                                    "gua"="Guarulhos",
                                                                    "mac"="Maceió",
                                                                    "man"="Manaus",
                                                                    "nat"="Natal",
                                                                    "poa"="Porto Alegre",
                                                                    "rec"="Recife",
                                                                    "rio"="Rio de Janeiro",
                                                                    "sal"="Salvador",
                                                                    "sgo"="São Gonçalo",
                                                                    "slz"="São Luís",
                                                                    "spo"="São Paulo")


options(scipen = 999)

p <- ggplot(resultado_final_v3, aes(y=reorder(cid, razao), x=razao))+
  geom_line(aes(group=cid), size=2, color="grey")+
  geom_point(aes(color= "black"), size=4, shape=16, stroke=2,show.legend = F)


p + scale_color_manual(name="",
                       values = "black",
                       labels="")+
  scale_y_discrete(expand = c(.02,0))+
  scale_x_continuous(labels = scales::percent)+
  #facet_wrap(~limite)+
  #labs(title = "Ratio of reachable job opportunities (peak hour/free flow)",
  #     subtitle = "Interval accessibility measure (15-45 minutes)")+
  theme_minimal() +
  xlab("Percentage")+
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        axis.title.x = element_text(),
        text = element_text(family = "sans", face = "bold", size = 14),
        plot.title = element_text(size = 16, margin = margin(b=10)),
        plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))


ggsave(filename="Fig2.png", plot=ggplot2::last_plot(),
       dpi = 300, width = 25, height = 20, units = "cm")
