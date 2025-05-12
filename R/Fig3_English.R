library(aopdata)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(accessibility)


# figure 3 --------------------------------------------------------------

resultado_final <- readRDS('./data/table_acess_by_quitile.rds')


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

# diferenca de impacto nos pobres e ricos
resultado_final[, .(gap = ratio[which(Quintil==1)] / ratio[which(Quintil==5)]),
                by=cid][order(gap)]

# resultado_final <- resultado_final |>
#   mutate(Quintil = case_when(Quintil == "1" ~ "1 - pobre",
#                              Quintil == "5" ~ "5 - rico",
#                              Quintil %in% c("2", "3","4") ~ Quintil))

resultado_final <- resultado_final |>
  mutate(Quintil = case_when(Quintil == "1" ~ "1\nPoorer",
                             Quintil == "5" ~ "5\nRicher",
                             Quintil %in% c("2", "3","4") ~ Quintil))


# resultado medio por grupo de renda
resultado_final[, .(avg_congest = mean(ratio) *100), by = Quintil]

media_resultado <- resultado_final |>
  group_by(cid) |>
  summarise(media_cong = mean(ratio))


p <- ggplot(resultado_final, aes(y=reorder(cid, ratio), x=ratio))+
  geom_point(data = media_resultado, aes(y=reorder(cid, media_cong), x=media_cong, group=cid), size=4, shape=3, stroke=2,show.legend = F, colour="grey")+
  geom_line(aes(group=cid), size=2, color="grey")+
  geom_point(aes(color= Quintil), size=4, shape=1, stroke=2)


# p1 <- ggplot(media_resultado) +
#   geom_point(aes(x=media_cong, y=cid ,group=cid), size=4, shape=3, stroke=2)

fig3 <- p + scale_color_viridis_d()+
  scale_y_discrete(expand = c(.02,0))+
  scale_x_continuous(labels = scales::percent)+
  #labs(title = "Impact of congestion on accessibility to jobs by car",
   #    subtitle = "Interval accessibility measure (15-45 minutes)")+
  theme_minimal() +
  labs(color= 'Quintile') +
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

fig3

ggsave(fig3, filename="./figures/Fig3_English.png",
        dpi = 300, width = 25, height = 20, units = "cm")




