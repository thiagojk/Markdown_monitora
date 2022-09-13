
library(tidyverse, xts)


# Casos COVID-19

don_casos <- monitoramento |> 
  mutate(dia_mes = format(as.Date(datanot, "%m/%d/%y"), "%m/%d"),
         ano = format(datanot, "%Y")) |> 
  group_by(dia_mes, ano) |> 
  summarise(count = n()) |> 
  spread(key = ano, value = count) |> 
  mutate(datanot = as.Date(dia_mes, format = "%d/%m")) |> 
  filter(datanot >= max(`2022`)-60)


casos_2020 <- xts(don_casos$`2020`, order.by = don_casos$datanot)
casos_2021 <- xts(don_casos$`2021`, order.by = don_casos$datanot)
casos_2022 <- xts(don_casos$`2022`, order.by = don_casos$datanot)

casos <- cbind(casos_2020, casos_2021, casos_2022)

dygraph(casos) |> 
  dySeries("casos_2020", label = "Casos 2020") |> 
  dySeries("casos_2021", label = "Casos 2021") |> 
  dySeries("casos_2022", label = "Casos 2022") |> 
  dyLegend(width = 400,)



##


casos_totais <- monitoramento |>  
  group_by(datanot) |> 
  summarise(count = n()) 

obitos_totais <- monitoramento |> 
  filter(obito == "Sim") |> 
  group_by(dataobt) |> 
  summarise(obitos = n()) |> 
  rename(datanot = dataobt)

casos_obitos <- full_join(casos_totais, obitos_totais, by = "datanot") |> 
  rename(casos = count) |> 
  drop_na(datanot)

casos1 <- xts(casos_obitos$casos, order.by = casos_obitos$datanot)
obitos1 <- xts(casos_obitos$obitos, order.by = casos_obitos$datanot)

plot1 <- cbind(casos1, obitos1)


dygraph(plot1) |> 
  dyBarSeries('obitos1', color = "red") |> 
  dyEvent(x = "2020-12-31", "2020", labelLoc = 'top') |> 
  dyEvent(x = "2021-12-31", "2021", labelLoc = 'top') |> 
  dySeries(name = "casos1", label = "Casos") |> 
  dySeries(name = "obitos1", label = "Óbitos")



###

# Casos por faixa etaria e ano
pacman::p_load(ggpol)

monitoramento |> 
  group_by(Faixa_Etaria, datanot, sexo) |> 
  summarise(count = n()) |> 
  drop_na(Faixa_Etaria) |> 
  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) |> 
  mutate(count = ifelse(sexo == "Masculino",-count,count)) |> 
  ggplot(aes(x = Faixa_Etaria, y = count, fill = sexo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = count)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  coord_flip() +
  facet_share(~sexo, scales = "free", reverse_num = TRUE) 


# plot 2
monitoramento |> 
  group_by(Faixa_Etaria, datanot, sexo) |> 
  summarise(count = n()) |> 
  drop_na(Faixa_Etaria) |> 
  ggplot(aes(x = Faixa_Etaria, y = count, fill = sexo)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  coord_flip()


# casos por mês -------------------------------------------------------------
library(lubridate)

casos_mes <- monitoramento |>
  group_by(Mes, year) |>
  summarise(count = n()) 

casos_mes$mes1 <- seq(ymd("2020-02-01"), 
                      ymd("2022-08-01"), by = "months")

casos_mes1 <- xts(x = casos_mes$count, order.by = casos_mes$mes1)

dygraph(casos_mes1) |> 
  dyBarChart()
















