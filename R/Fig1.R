
library(tidyverse)
library(ggbump)
library(patchwork)
library(data.table)

# Colors

color_palette <- viridisLite::mako(11)
description_color <- 'grey40'
cities_colors <- c(
  'S?o Paulo' = color_palette[2],
  'Fortaleza' = color_palette[7],
  'Recife' = color_palette[8],
  'Salvador' = color_palette[5],
  'Belo Horizonte' = color_palette[6],
  'Rio de Janeiro' = color_palette[3],
  'Curitiba' = color_palette[10],
  'Porto Alegre' = color_palette[9],
  'Bras?lia' = color_palette[4]
)

cities_rank_acessibilidade <- fread("./Proj_acess_oport/git_diego/traffic_congestion_accessibility/data/resultado_acessibilidade.csv")%>%
  mutate(ordem=1)

cities_rank_censo <- fread("./Proj_acess_oport/git_diego/traffic_congestion_accessibility/data/resultado_censo2010.csv") %>%
  mutate(rank = row_number(desc(media_tempo)),
         ordem = 2,
         cidade = case_when(COD_MUN == 2304400 ~ "Fortaleza",
                            COD_MUN == 2611606 ~ "Recife",
                            COD_MUN == 2927408 ~ "Salvador",
                            COD_MUN == 3106200 ~ "Belo Horizonte",
                            COD_MUN == 3304557 ~ "Rio de Janeiro",
                            COD_MUN == 3550308 ~ "S?o Paulo",
                            COD_MUN == 4106902 ~ "Curitiba",
                            COD_MUN == 4314902 ~ "Porto Alegre",
                            COD_MUN == 5300108 ~ "Bras?lia"))

cities_rank_censo <- cities_rank_censo %>%
  select("rank","cidade","ordem")

cities_rank_tomtom <- fread("./Proj_acess_oport/git_diego/traffic_congestion_accessibility/data//resultado_tomtom.csv") %>%
  mutate(ordem=3)

cities_rank_geral <- rbind(cities_rank_acessibilidade,cities_rank_censo,cities_rank_tomtom) #%>%

bump_chart_basic <- cities_rank_geral %>%
  ggplot(aes(ordem, rank, col = cidade)) +
  geom_point(shape = '|', stroke = 6) +
  geom_bump(size = 1.5) +
  geom_text(
    data = cities_rank_geral %>% filter(ordem == 1),
    aes(label = cidade),
    hjust = 1,
    nudge_x = -0.1,
    fontface = 'bold'
  ) +
  geom_text(
    data = cities_rank_geral %>% filter(ordem==3),
    aes(label = rank),
    hjust = 0,
    nudge_x = 0.1,
    size = 5,
    fontface = 'bold'
  ) +
  annotate(
    'text',
    x = c(1, 2, 3),
    y = c(0.25, 0.25, 0.25),
    label = c("Acesssibilidade", "Tempo de viagem","Velocidade do tr?fego"),
    hjust = c(0, 0.5, 1),
    vjust = 1,
    size = 4,
    fontface = 'bold',
    color = description_color
  ) +
  scale_y_reverse(position = 'right', breaks = seq(16, 2, -2)) +
  scale_color_manual(values = cities_colors) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(10, 0), expand = F) +
  theme_void() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(
      margin = margin(t = 3, b = 2, unit = 'mm')
    ),
    plot.title = element_text(
      face = 'bold',
      size = 20
    )
  ) +
  labs(
    title = 'Ranking de congestionamento',
    subtitle = 'Congestionamento medido atrav?s de diferentes m?todos'
  )

ggsave(filename="Fig1.png", plot=ggplot2::last_plot(),
       dpi = 300, width = 25, height = 15, units = "cm")
