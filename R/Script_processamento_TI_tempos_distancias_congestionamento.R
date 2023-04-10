library(data.table)
library(R.utils)
library(sf)
library(dplyr)
library(matrixStats)

setDTthreads(percent = 100)
setwd("~/Processamentos")

cidades <- c("bel", "bho","bsb","cam","cgr","cur","duq","for_","goi","gua","mac","man","nat","poa","rec","rio","sal","sgo","slz","spo")
pastas <- c("04AM")

for (i in (1:length(cidades))){
  for (j in (1:length(pastas))){
    print(cidades[i])
    print(pastas[j])
    
    OD_TI <- fread(gunzip(paste0("Matriz_TI/",cidades[i],"/wkday", pastas[j],"/",pastas[j], ".csv.gzip"), temporary=T, remove = F, overwrite=T))
    
    setnames(OD_TI, "Total_Time", paste0("Total_Time_", pastas[j]))
    setnames(OD_TI, "Total_Distance", paste0("Total_Distance_", pastas[j]))
    OD_TI[, concat := paste0(OriginName,'-', DestinationName)]
    OD_TI[,c("OriginName","DestinationName"):=NULL]
    
    gc()
  
    }
    
  OD_TI[, c("origin", "destination"):= tstrsplit(concat, "-", fixed=TRUE)]
  
  shp <- st_read(dsn="shapes_saida/hexagonos.gdb", layer=paste0(cidades[i])) 
  shp <- shp %>%
    st_drop_geometry()%>%
    mutate("id_num" = rownames(shp)) %>%
    select("id_hex","id_num")%>%
    rename(origin = id_num)
  
  shp <- as.data.table(shp)  
  
  OD_TI <- merge(OD_TI, shp, by="origin") %>%
    rename(origin_hex = id_hex)
  
  shp <- shp %>%
    rename(destination = "origin")
  
  OD_TI <- merge(OD_TI, shp, by="destination") %>%
    rename(destination_hex = id_hex)
  

  OD_TI <- OD_TI[, c("origin_hex","destination_hex","Total_Time_04AM","Total_Distance_04AM")]
  
  fwrite(OD_TI, paste0("OD_TI_", cidades[i],".csv"))
  rm(OD_TI)
  rm(shp)
  gc()
}
