library(aopdata)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(accessibility)
library(h3jsr)
library(geobr)

setDTthreads(percent = 100)

cidades <- c("bel", "bho","bsb","cam","cgr","cur","duq","for","goi","gua","mac","man","nat","poa","rec","rio","sal","sgo","slz","spo")

# get hex in goiania muni
goi_poly <- geobr::read_municipality(year=2010) |>
  filter(code_muni == '5208707')

goi_hexes <- h3jsr::polygon_to_cells(
  geometry = sf::st_geometry(goi_poly),
  res = 9
  )

goi_hexes <- goi_hexes |> unlist() |> as.vector()

get_acess_by_quitile <- function(cidade){

  message(cidade)

  # land use data
  landuse <- aopdata::read_landuse(city=cidade, showProgress = FALSE) |>
    dplyr::select(id_hex, P001, R002, T001) |>
    rename("id" = id_hex)

  # filter goi to main muni, and not the metro
  if(cidade == 'goi'){
    landuse <- filter(landuse, id %in% goi_hexes)
  }

  hex_with_pop <- landuse[P001 > 0]$id
  hex_with_jobs <- landuse[T001 > 0]$id


  # read free-flow ttm
  free <- arrow::open_csv_dataset(paste0("../../data/congestionamento/04AM/OD_TI_", cidade, ".csv")) |>
    dplyr::select(origin_hex, destination_hex, Total_Time_04AM) |>
    rename("from_id" = origin_hex,
           "to_id" = destination_hex) |>
    filter(from_id %in% hex_with_pop,
           to_id %in% hex_with_jobs) |>
    collect()


  # read peak ttm

  peak <- arrow::open_csv_dataset(paste0("../../data/acesso_oport/output_ttmatrix_ti/OD_TI_", cidade, ".csv"))
  colnames <- names(peak)

  if (all(c("origin_hex", "destination_hex", "median_morning_peak") %in% colnames)) {

    peak <- peak |>
      dplyr::rename("from_id" = origin_hex,
                    "to_id" = destination_hex) |>
      dplyr::filter(from_id %in% hex_with_pop,
                    to_id %in% hex_with_jobs) |>
      dplyr::select(from_id, to_id, median_morning_peak) |>
      dplyr::collect()

  } else {

    peak <- peak |>
      dplyr::rename("from_id" = origin,
                    "to_id" = destination) |>
      dplyr::filter(from_id %in% hex_with_pop,
                    to_id %in% hex_with_jobs) |>
      dplyr::select(from_id, to_id, median_morning_peak) |>
      dplyr::collect()
  }

  gc(T)


  # calculate access
  df_04AM <- accessibility::cumulative_interval(
    travel_matrix = free,
    land_use_data = landuse,
    interval = c(15,45),
    interval_increment = 1,
    opportunity = "T001",
    travel_cost = "Total_Time_04AM"
  )


  df_peak <- accessibility::cumulative_interval(
    travel_matrix = peak,
    land_use_data = landuse,
    interval = c(15,45),
    interval_increment = 1,
    opportunity = "T001",
    travel_cost = "median_morning_peak"
  )

  # rename access columns
  df_04AM <- df_04AM |> rename("T001_04AM"= T001)
  df_peak <- df_peak |> rename("T001_peak"= T001)


  # join access estimates
  df <- dplyr::left_join(df_04AM, df_peak, by="id")


  pop <- landuse |>
    filter(P001 >0) |>
    dplyr::select(id, P001, R002)

  acc <- dplyr::left_join(pop, df, by= "id") |>
    filter(R002 >0)

  acc[is.na(acc)] <- 0


  acc <- acc |>
    group_by(R002) |>
    summarise(pop = sum(P001),
              mean_CMATT_free = weighted.mean(T001_04AM, P001/1000),
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
  X = cidades,
  FUN = get_acess_by_quitile)|>
  data.table::rbindlist()

table(resultado_final$cid)

saveRDS(resultado_final, './data/table_acess_by_quitile.rds')
