setwd("/home/talita/covidProject/scripts")
getwd()

# Instalando pacotes
#install.packages("ggpubr")

# Carregando os pacotes
library(dplyr)
library(ggplot2)
library(ggpubr)

# Resumo dos arquivos

# Dados Kaggle Mundo (https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/)
read.csv("../datasets/covid_19_data.csv")

# Dados Kaggle Brasil (https://www.kaggle.com/unanimad/corona-virus-brazil)
read.csv("../datasets/brazil_covid19.csv")

# Carregando dados
world <- data.frame(read.csv("../datasets/covid_19_data.csv"))
brazilRegions <- data.frame(read.csv("../datasets/brazil_covid19.csv"))
#View(world)
#View(brazilRegions)

# Organizando o shape dos dados

# Mundo
world <- world %>% 
  mutate(ObservationDate = as.Date(ObservationDate, format='%m/%d/%Y')) %>%
  filter(Province.State == "") %>%
  filter(Confirmed != 0)
colnames(world) <- c("row", "date", "province", "state", "update", "cases", "deaths", "recovered")

# Brasil
brazilRegions <- brazilRegions %>%
  mutate(date = as.Date(date, format='%Y-%m-%d')) %>%
  filter(cases != 0)

brazil <- world %>% filter(state == "Brazil" & cases != 0)
#View(brazil)

# Totalizando dados

# Total de casos e mortes no Brasil por Região
totalBrazilByRegion <- brazilRegions %>%
  filter(brazilRegions$date == Sys.Date() - 1) %>%
  group_by(region) %>%
  summarise_at(vars(cases, deaths), funs(sum))
#View(totalBrazilByRegion)

# Total de Casos no Brasil
totalCasesBrazil <- sum(totalBrazilByRegion$cases)
totalCasesBrazil

# Total de Mortes no Brasil
totalDeathsBrazil <- sum(totalBrazilByRegion$deaths)
totalDeathsBrazil

# Estados Foco no Brasil

# DF
casesDF <- brazilRegions %>%
  filter(brazilRegions$state == "Distrito Federal") %>%
  select(state, date, cases, deaths)
casesDF$id <- c(1:nrow(casesDF))
#View(casesDF)

# Gráfico de crescimento de casos no DF
plotDF <- ggplot(casesDF, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no DF")
plotDF

# SP
casesSP <- brazilRegions %>%
  filter(brazilRegions$state == "São Paulo") %>%
  select(state, date, cases, deaths)
casesSP$id <- c(1:nrow(casesSP))
#View(casesSP)

# Gráfico de crescimento de casos em SP
plotSP <- ggplot(casesSP, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 em SP")
plotSP

# RJ
casesRJ <- brazilRegions %>%
  filter(brazilRegions$state == "Rio de Janeiro") %>%
  select(state, date, cases, deaths)
casesRJ$id <- c(1:nrow(casesRJ))
#View(casesRJ)

# Gráfico de crescimento de casos no RJ
plotRJ <- ggplot(casesRJ, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no RJ")
plotRJ

# CE
casesCE <- brazilRegions %>%
  filter(brazilRegions$state == "Ceará") %>%
  select(state, date, cases, deaths)
casesCE$id <- c(1:nrow(casesCE))
#View(casesCE)

# Gráfico de crescimento de casos no CE
plotCE <- ggplot(casesCE, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no CE")
plotCE

# AM
casesAM <- brazilRegions %>%
  filter(brazilRegions$state == "Amazonas") %>%
  select(state, date, cases, deaths)
casesAM$id <- c(1:nrow(casesAM))
#View(casesAM)

# Gráfico de crescimento de casos no AM
plotAM <- ggplot(casesAM, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no AM")
plotAM

# Combinando gráficos dos Estados Foco
focusStates <- rbind(casesDF, casesSP, casesRJ, casesCE, casesAM)
plotfocusStates <- ggplot(focusStates, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 nos Estados Foco")
plotfocusStates


# GRÁFICOS BRASIL

# Gráfico do total de casos por Região no Brasil
totalBrazilByRegion %>%
  ggplot(aes(x = reorder(region, -cases), y = cases, color = region, fill = region)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=cases), vjust=0) + 
  ggtitle("Casos de COVID-19 por Região do Brasil")

# Gráfico do total de mortes por Região no Brasil
totalBrazilByRegion %>%
  ggplot(aes(x = reorder(region, -deaths), y = deaths, color = region, fill = region)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=deaths), vjust=0) + 
  ggtitle("Mortes por COVID-19 por Região do Brasil")

# Totais Brasil

# Casos Brasil
casesBrazil <- brazil %>%
  select(state, date, cases, deaths)
casesBrazil$id <- c(1:nrow(casesBrazil))
#View(casesBrazil)

# Mortes Brasil
deathsBrazil <- world %>%
  filter(world$state == "Brazil" & world$deaths != 0) %>%
  select(state, date, cases, deaths)
deathsBrazil$id <- c(1:nrow(deathsBrazil))
#View(deathsBrazil)

# Totais Mundo

# Itália

# Casos Itália
casesItaly <- world %>%
  filter(world$state == "Italy" & world$cases != 0) %>%
  select(state, date, cases, deaths)
casesItaly$id <- c(1:nrow(casesItaly))
#View(casesItaly)
casesItalyFilter <- casesItaly %>%
  filter(casesItaly$id <= 70)

# Mortes Itália
deathsItaly <- world %>%
  filter(world$state == "Italy" & world$deaths != 0) %>%
  select(state, date, cases, deaths)
deathsItaly$id <- c(1:nrow(deathsItaly))
#View(deathsItaly)
deathsItalyFilter <- deathsItaly %>%
  filter(deathsItaly$id <=50)
#View(deathsItalyFilter)

# Gráficos Brasil e Mundo

# Gráfico de crescimento de casos no Brasil por dia
plotBrazil <- ggplot(casesBrazil, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 no Brazil por dia")
plotBrazil

# Gráfico de crescimento de casos na Itália por dia
plotItaly <- ggplot(casesItaly, aes(date, cases, colours=cases)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Casos de COVID-19 na Itália por dia")
plotItaly

# Comparando Brasil e Mundo

# Brasil e Itália

# Casos Brasil e Itália

# Comparando dados Brasil e Itália
brazilItaly <- rbind(casesItaly, casesBrazil)
plotbrazilItaly <- ggplot(brazilItaly, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 Brasil e Itália")
plotbrazilItaly

# Comparando dados Brasil e Itália (até o dia atual da doença no Brasil)
brazilItalyFilter <- rbind(casesItalyFilter, casesBrazil)
plotbrazilItalyFilter <- ggplot(brazilItalyFilter, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 Brasil e Itália")
plotbrazilItalyFilter

# Comparando mortes Brasil e Itália
brazilItalyDeaths <- rbind(deathsItalyFilter, deathsBrazil)
plotbrazilItalyDeaths <- ggplot(brazilItalyDeaths, aes(x=id, y=deaths, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=deaths), vjust=1) +
  ggtitle("Comparação de Mortes de COVID-19 Brasil e Itália")
plotbrazilItalyDeaths

# Comparando dados dos Estados do Brasil e Mundo

# Combinando gráficos dos Estados Foco com a Itália
statesAndItaly <- rbind(focusStates, casesItaly)
plotstatesAndItaly <- ggplot(statesAndItaly, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 nos Estados Foco e Itália")
plotstatesAndItaly

# Combinando gráficos dos Estados Foco com a Itália (até o dia atual da doença no Brasil)
statesAndItalyFilter <- rbind(focusStates, casesItalyFilter)
plotstatesAndItalyFilter <- ggplot(statesAndItalyFilter, aes(x=id, y=cases, col=state, fill=state)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label=cases), vjust=1) +
  ggtitle("Comparação de Casos de COVID-19 nos Estados Foco e Itália")
plotstatesAndItalyFilter



