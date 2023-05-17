
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
google_key <- ""

sp <- aopdata::read_grid("spo") %>%
 group_by(abbrev_muni) %>%
 summarise()

sp <- sp %>%
 st_transform(4326)

r <- gt_make_raster_from_polygon(polygon = sp,
                                zoom = 12,
                                google_key = google_key,)

r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
names(r_df) <- c("value", "x","y")

ggplot() +
  geom_raster(data = r_df, 
              aes(x = x, y = y, 
                  fill = as.factor(value))) +
  labs(fill = "Traffic\nLevel") +
  scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
  coord_quickmap() + 
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color="white"))

