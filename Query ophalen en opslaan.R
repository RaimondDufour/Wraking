library(dplyr)
# Uit database halen en opslaan: 
rm(list = ls())
setwd("D:/Uitwisselmap/Projecten/Ecli/Rstudio")
source ("D://Uitwisselmap/Projecten/Rstudio/connecties.R")
library(RMySQL)
library(tm)

# uitkomst en redenen: 
WrakingsRedenenQuery <- paste(
  "SELECT onderzoek.ecli, tekst1 as wrakingszaak, tekst2 as uitkomst, categorie1 as reden, inhoudsindicatie, uitspraak
FROM onderzoek
LEFT JOIN uitspraaktekst ON uitspraaktekst.ecli = onderzoek.ecli
WHERE onderzoek.ecli NOT LIKE 'ECLI:NL:RBGEL:2018:4813';
  ")
Wrakingsredenen <- dbGetQuery(conThuis, WrakingsRedenenQuery)
Wrakingsredenen <- Wrakingsredenen %>% 
  mutate(wrakingszaak = as.factor(wrakingszaak)) %>% 
  mutate(uitkomst = as.factor(uitkomst)) %>% 
  mutate(reden = as.factor(reden)) %>% 
  mutate(uitspraak = gsub(pattern = "<(.*?)>", " ", uitspraak)) %>% 
  mutate(uitspraak = stripWhitespace(uitspraak)) %>% 
  mutate(uitspraak = trimws(uitspraak)) %>% 
  mutate(inhoudsindicatie = gsub(pattern = "<(.*?)>", " ", inhoudsindicatie)) %>% 
  mutate(inhoudsindicatie = stripWhitespace(inhoudsindicatie)) %>% 
  mutate(inhoudsindicatie = trimws(inhoudsindicatie)) %>% 
  as_tibble()

saveRDS(Wrakingsredenen, file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking\\WrakingAlles.rds")
Wrakingsredenen2 <- Wrakingsredenen %>% 
  filter(wrakingszaak=='wrakingszaak') %>% 
  select(-wrakingszaak)
saveRDS(Wrakingsredenen2, file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking\\Wrakingszaken.rds")

RechtsgebiedQuery <- paste(
  "SELECT onderzoek.ecli, rechtsgebied
FROM onderzoek 
left join eclis ON eclis.ecli = onderzoek.ecli 
WHERE tekst1 = 'wrakingszaak'
  AND onderzoek.ecli NOT LIKE '%:2018:4813';")
Rechtsgebieden <- dbGetQuery(conThuis, RechtsgebiedQuery) %>% 
  mutate(rechtsgebied = as.factor(rechtsgebied))

saveRDS(Rechtsgebieden, file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking\\Wraking_Rechtsgebieden.rds")
Dataset <- readRDS(file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking_Rechtsgebieden.rds") 


# Eventueel: Zoeken op tekenreeks wraking:
WrakingQuery <- paste(
  "SELECT uitspraaktekst.ecli, inhoudsindicatie, uitspraak
FROM uitspraaktekst
LEFT JOIN eclis ON eclis.ecli = uitspraaktekst.ecli
WHERE uitspraakdatum > '2015-12-31'
AND uitspraakdatum < '2021-01-01'
AND (uitspraak LIKE '%wraking%' OR inhoudsindicatie LIKE '%wraking%')
AND uitspraaktekst.ecli NOT LIKE '%NL:T%'
AND uitspraaktekst.ecli NOT LIKE '%NL:PHR%';"
)
Dataset <- dbGetQuery(conThuis, WrakingQuery)
saveRDS(Dataset, file = "Wraking_Trefwoord.rds")
Dataset <- readRDS(file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking_Trefwoord.rds") 

  