setwd("/home/talita/covidProject/scripts")
getwd()

# Instalando pacotes
install.packages("ggpubr")

# Carregando os pacotes
library(dplyr)
library(ggplot2)
library(ggpubr)

# Resumo dos arquivos
# Dados ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)
read.csv("COVID-19-worldwide.csv")

# Dados Kaggle Brasil (https://www.kaggle.com/unanimad/corona-virus-brazil)
read.csv("brazil_covid19.csv")

# Dados Kaggle Mundo (https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/data#covid_19_data.csv)
read.csv("covid_19_data.csv")

# Organizando o shape dos dados
#world <- data.frame(read.csv("COVID-19-worldwide.csv"))
#world <- world %>% 
#  mutate(dateRep = as.Date(dateRep, format='%d/%m/%Y'))
#View(world)

brazilRegions <- data.frame(read.csv("brazil_covid19.csv"))
brazilRegions <- brazilRegions %>%
  mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
  filter(cases != 0)
View(brazilRegions)

world2 <- data.frame(read.csv("covid_19_data.csv"))
world2 <- world2 %>% 
  mutate(ObservationDate = as.Date(ObservationDate, format='%m/%d/%Y')) %>%
  filter(Province.State == "") %>%
  filter(cases != 0)
colnames(world2) <- c("row", "date", "province", "state", "update", "cases", "deaths", "recovered")
View(world2)

# Comparando Países
#targetCountries <- world %>% 
#  filter(countriesAndTerritories == "Brazil" | countriesAndTerritories == "Italy" | countriesAndTerritories == "United_States_of_America" | countriesAndTerritories == "Spain" | countriesAndTerritories == "Iran" | countriesAndTerritories == "China" | countriesAndTerritories == "France") %>%
#  filter(cases != 0)

#targetCountries %>% 
#  ggplot(aes(x = dateRep, y = cases, group = countriesAndTerritories, color = countriesAndTerritories)) + 
#  geom_line() +  
#  geom_point() +
#  ggtitle("Casos de COVID-19 por Região do Brasil")

# Totalizando casos
#brazil <- world %>% filter(countriesAndTerritories == "Brazil" & cases != 0)
#View(brazil)

brazil2 <- world2 %>% filter(state == "Brazil" & cases != 0)
View(brazil2)


# Contabilizando até 01/04/2020
brazil$total_cases <- c(5717,4579,4256,3904,3417,2915,2433,2201,1891,1546,1128,904,621,428,291,234,200,121,98,77,52,34,25,13,8,3,2,1)
View(brazil)

total_cases_brazil <- sum(brazil$cases)
total_cases_brazil

total_cases_by_region <- brazilRegions %>%
  filter(brazilRegions$date == Sys.Date() - 1)
View(total_cases_by_region)

total_cases_brazil2 <- sum(total_cases_by_region$cases)
total_cases_brazil2

# Gerando gráfico de casos por dia no Brasil
ggplot(brazil, aes(dateRep, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  ggtitle("Casos de COVID-19 no Brasil por dia")

# Gerando gráfico do total de casos no Brasil
ggplot(brazil, aes(dateRep, total_cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  ggtitle("Casos de COVID-19 no Brasil")

# Gerando gráfico do total de casos por Região no Brasil
total_cases_by_region %>% 
  ggplot(aes(x = region, y = cases, group = region, color = region, fill = region)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=cases), vjust=0) + 
  ggtitle("Casos de COVID-19 por Região do Brasil")

# Casos nos Estados Foco

# DF
casosDF <- brazilRegions %>%
  filter(brazilRegions$state == "Distrito Federal") %>%
  select(state, date, cases, deaths)
casosDF$id <- c(1:nrow(casosDF))
View(casosDF)

# Gerando gráfico de crescimento de casos no DF
plotDF <- ggplot(casosDF, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no DF")
plotDF

# SP
casosSP <- brazilRegions %>%
  filter(brazilRegions$state == "São Paulo") %>%
  select(state, date, cases, deaths)
casosSP$id <- c(1:nrow(casosSP))
View(casosSP)

# Gerando gráfico de crescimento de casos em SP
plotSP <- ggplot(casosSP, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 em SP")
plotSP

# RJ
casosRJ <- brazilRegions %>%
  filter(brazilRegions$state == "Rio de Janeiro") %>%
  select(state, date, cases, deaths)
casosRJ$id <- c(1:nrow(casosRJ))
View(casosRJ)

# Gerando gráfico de crescimento de casos no RJ
plotRJ <- ggplot(casosRJ, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no RJ")
plotRJ

# CE
casosCE <- brazilRegions %>%
  filter(brazilRegions$state == "Ceará") %>%
  select(state, date, cases, deaths)
casosCE$id <- c(1:nrow(casosCE))
View(casosCE)

# Gerando gráfico de crescimento de casos no CE
plotCE <- ggplot(casosCE, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no CE")
plotCE

# Combinando gráficos dos Estados Foco
estadosFoco <- rbind(casosDF, casosSP, casosRJ, casosCE)
plotEstadosFoco <- ggplot(estadosFoco, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 nos Estados Foco")
plotEstadosFoco

# Comparando dados dos Estados do Brasil com a Itália

# Itália
casosItalia <- world2 %>%
  filter(world2$state == "Italy" & world2$cases != 0) %>%
  select(state, date, cases, deaths)
casosItalia$id <- c(1:nrow(casosItalia))
View(casosItalia)
casosItaliaFilter <- casosItalia %>%
  filter(casosItalia$id <= 40)

# Mortes Itália
mortesItalia <- world2 %>%
  filter(world2$state == "Italy" & world2$deaths != 0) %>%
  select(state, date, cases, deaths)
mortesItalia$id <- c(1:nrow(mortesItalia))
View(mortesItalia)
mortesItaliaFilter <- mortesItalia %>%
  filter(mortesItalia$id <=20)
View(mortesItaliaFilter)

# Gerando gráfico de crescimento de casos na Itália
plotItalia <- ggplot(casosItalia, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  ggtitle("Casos de COVID-19 na Itália")
plotItalia

# Combinando gráficos dos Estados Foco com a Itália
estadosMaisItalia <- rbind(casosDF, casosSP, casosRJ, casosCE, casosItalia)
plotEstadosMaisItalia <- ggplot(estadosMaisItalia, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 nos Estados Foco e Itália")
plotEstadosMaisItalia

# Combinando gráficos dos Estados Foco com a Itália (até o dia atual da doença no Brasil)
estadosMaisItaliaFilter <- rbind(casosDF, casosSP, casosRJ, casosCE, casosItaliaFilter)
plotEstadosMaisItaliaFilter <- ggplot(estadosMaisItaliaFilter, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 nos Estados Foco e Itália")
plotEstadosMaisItaliaFilter


# Comparando Brasil e Itália

# Brasil
casosBrasil <- brazil2 %>%
  select(state, date, cases, deaths)
casosBrasil$id <- c(1:nrow(casosBrasil))
View(casosBrasil)

# Mortes Brasil
mortesBrasil <- world2 %>%
  filter(world2$state == "Brazil" & world2$deaths != 0) %>%
  select(state, date, cases, deaths)
mortesBrasil$id <- c(1:nrow(mortesBrasil))
View(mortesBrasil)

# Gerando gráfico de crescimento de casos no Brasil
plotBrazil <- ggplot(casosBrasil, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no Brazil")
plotBrazil

# Comparando casos Brasil e Itália
brazilItalia <- rbind(casosItalia, casosBrasil)
plotBrazilItalia <- ggplot(brazilItalia, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 Brasil e Itália")
plotBrazilItalia

# Comparando dados Brasil e Itália (até o dia atual da doença no Brasil)
brazilItaliaFilter <- rbind(casosItaliaFilter, casosBrasil)
plotBrazilItaliaFilter <- ggplot(brazilItaliaFilter, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 Brasil e Itália")
plotBrazilItaliaFilter

# Comparando mortes Brasil e Itália
brasilItaliaMortes <- rbind(mortesItaliaFilter, mortesBrasil)
plotBrasilItaliaMortes <- ggplot(brasilItaliaMortes, aes(x=id, y=deaths, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=deaths), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 Brasil e Itália")
plotBrasilItaliaMortes


