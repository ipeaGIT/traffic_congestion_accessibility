library(aopdata)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(accessibility)

options(scipen = 999)


# figure 2 --------------------------------------------------------------

resultado_final <- readRDS('./data/table_acess_by_quitile.rds')

resultado_final$ratio <- 1-(resultado_final$mean_CMATT_peak/resultado_final$mean_CMATT_free)

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


# resultado medio por cidade
# figura 2 e tabela 1
resultado_cidade <- resultado_final[
  ,
  .(avg_congest = mean(ratio),
    avg_peak = mean(mean_CMATT_peak),
    avg_free = mean(mean_CMATT_free)
    ),
  by = cid][order(avg_congest),]

resultado_cidade


p <- ggplot(resultado_cidade, aes(y=reorder(cid, avg_congest), x=avg_congest))+
  geom_line(aes(group=cid), size=2, color="grey")+
  geom_point(aes(color= "black"), size=4, shape=16, stroke=2,show.legend = F)

figure2 <- p + scale_color_manual(name="",
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

figure2
ggsave(figure2, filename="./figures/Fig2_English.png",
       dpi = 300, width = 25, height = 20, units = "cm")
