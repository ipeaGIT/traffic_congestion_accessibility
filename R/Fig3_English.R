library(aopdata)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(accessibility)

setDTthreads(percent = 100)

cidades <- c("bel", "bho","bsb","cam","cgr","cur","duq","for","goi","gua","mac","man","nat","poa","rec","rio","sal","sgo","slz","spo")

get_acess_by_quitile <- function(cidade){

  message(cidade)

  # read free-flow ttm
  free <- fread(paste0("../../data/congestionamento/04AM/OD_TI_", cidade, ".csv")) |>
    dplyr::select(origin_hex, destination_hex, Total_Time_04AM) |>
    rename("from_id" = origin_hex,
           "to_id" = destination_hex)


  # read peak ttm

  ## get all files
  ttm_files <- list.files("../../data/acesso_oport/ttmatrix/ttmatrix_fixed/car/", full.names = T, pattern = '.csv')
  ttm_files <- ttm_files[ttm_files %like% cidade]

  peak <- lapply(X=ttm_files, data.table::fread) |> data.table::rbindlist(fill = T)

  colnames <- colnames(peak)


  if (all(c("from_id", "to_id", "median_morning_peak") %in% colnames)) {

      peak <- peak |>
        filter(from_id %in% unique(free$from_id),
               to_id %in% unique(free$to_id)) |>
        dplyr::select(from_id, to_id, median_morning_peak)

    } else {

    peak <- peak |>
      filter(origin %in% unique(free$from_id),
             destination %in% unique(free$to_id)) |>
      dplyr::select(origin, destination, median_morning_peak) |>
      rename("from_id" = origin,
             "to_id" = destination)
  }


  # land use data
  landuse <- aopdata::read_landuse(city=cidade, showProgress = FALSE) |>
    dplyr::select(id_hex, T001) |>
    rename("id" = id_hex)

  # calculate access
  df_04AM <- accessibility::cumulative_interval(travel_matrix = free,
                                 land_use_data = landuse,
                                 interval = c(15,45),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "Total_Time_04AM")



  df_peak <- accessibility::cumulative_interval(travel_matrix = peak,
                                 land_use_data = landuse,
                                 interval = c(15,45),
                                 interval_increment = 1,
                                 opportunity = "T001",
                                 travel_cost = "median_morning_peak")

  # rename access columns
  df_04AM <- df_04AM |> rename("T001_04AM"= T001)
  df_peak <- df_peak |> rename("T001_peak"= T001)


  # join access estimates
  df <- dplyr::left_join(df_04AM, df_peak, by="id")


  pop <- aopdata::read_population(city = cidade, showProgress = FALSE) |>
    filter(P001 >0) |>
    dplyr::select(id_hex, P001, R002)

  acc <- dplyr::left_join(pop, df, by=c("id_hex"="id")) |>
    filter(R002 >0)

  acc[is.na(acc)] <- 0


  acc <- acc |>
    group_by(R002) |>
    summarise(mean_CMATT_free = weighted.mean(T001_04AM, P001/1000),
              mean_CMATT_peak = weighted.mean(T001_peak, P001/1000)
              )

  acc <- acc |>
    rename("quintile" = R002)

  acc <- acc |>
    mutate(cid = cidade,
           quintile = as.character(quintile))

  gc(T)
  return(acc)
}



resultado_final <- pbapply::pblapply(
  X=cidades,
  FUN = get_acess_by_quitile
  )|>
  data.table::rbindlist()


resultado_final$quintile <- factor(resultado_final$quintile, levels = c("1","2","3","4","5"))
resultado_final$ratio <- 1-(resultado_final$mean_CMATT_peak/resultado_final$mean_CMATT_free)
resultado_final$Quintil <- resultado_final$quintile

resultado_final$cid <- recode_factor(resultado_final$cid, "bel"="Belém",
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


# resultado_final <- resultado_final |>
#   mutate(Quintil = case_when(Quintil == "1" ~ "1 - pobre",
#                              Quintil == "5" ~ "5 - rico",
#                              Quintil %in% c("2", "3","4") ~ Quintil))

resultado_final <- resultado_final |>
  mutate(Quintil = case_when(Quintil == "1" ~ "1\nPoorer",
                             Quintil == "5" ~ "5\nRicher",
                             Quintil %in% c("2", "3","4") ~ Quintil))

media_resultado <- resultado_final |>
  group_by(cid) |>
  summarise(media_cong = mean(ratio))

p <- ggplot(resultado_final, aes(y=reorder(cid, ratio), x=ratio))+
  geom_point(data = media_resultado, aes(y=reorder(cid, media_cong), x=media_cong, group=cid), size=4, shape=3, stroke=2,show.legend = F, colour="grey")+
  geom_line(aes(group=cid), size=2, color="grey")+
  geom_point(aes(color= Quintil), size=4, shape=1, stroke=2)


# p1 <- ggplot(media_resultado) +
#   geom_point(aes(x=media_cong, y=cid ,group=cid), size=4, shape=3, stroke=2)

p +scale_color_viridis_d()+
  scale_y_discrete(expand = c(.02,0))+
  scale_x_continuous(labels = scales::percent)+
  #labs(title = "Impact of congestion on accessibility to jobs by car",
   #    subtitle = "Interval accessibility measure (15-45 minutes)")+
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


ggsave(filename="C:/Users/b35143921880/Fig3_English.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 25, height = 20, units = "cm")
