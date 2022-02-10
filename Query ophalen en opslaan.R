library(dplyr)
# Uit database halen en opslaan: 
rm(list = ls())
setwd("D:/Uitwisselmap/Projecten/Ecli/Rstudio")
source ("D://Uitwisselmap/Projecten/Rstudio/connecties.R")
library(RMySQL)
library(tm)
library(stringr)
library(tidyr)
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



# met veel andere variabelen:
WrakingVeelVariabelenQuery <- paste(
  "SELECT onderzoek.ecli, tekst1 as wrakingszaak, tekst2 as uitkomst, categorie1 as reden, inhoudsindicatie, uitspraak, feitEnRechtPerAlinea,
  meta.rechtsgebied, oordeel, aantalwoorden, positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust
FROM onderzoek
LEFT JOIN uitspraaktekst ON uitspraaktekst.ecli = onderzoek.ecli
LEFT JOIN meta ON meta.ecli = onderzoek.ecli
WHERE onderzoek.ecli NOT LIKE 'ECLI:NL:RBGEL:2018:4813'
AND tekst1 = 'wrakingszaak';
  ")
Multivariabelen <- dbGetQuery(conThuis, WrakingVeelVariabelenQuery)
Multivariabelen <- Multivariabelen %>% 
  select(-wrakingszaak) %>% 
  mutate(uitkomst = as.factor(uitkomst)) %>% 
  mutate(reden = as.factor(reden)) %>% 
  mutate(uitspraak = gsub(pattern = "<(.*?)>", " ", uitspraak)) %>% 
  mutate(uitspraak = stripWhitespace(uitspraak)) %>% 
  mutate(uitspraak = trimws(uitspraak)) %>% 
  mutate(inhoudsindicatie = gsub(pattern = "<(.*?)>", " ", inhoudsindicatie)) %>% 
  mutate(inhoudsindicatie = stripWhitespace(inhoudsindicatie)) %>% 
  mutate(inhoudsindicatie = trimws(inhoudsindicatie)) %>% 
  separate(feitEnRechtPerAlinea, c("a", "b", "c"), sep=" \\| ") %>% 
  separate(a, c("a", "feiten"), sep=" ") %>% 
  separate(b, c("b", "recht"), sep=" ") %>%
  separate(c, c("c", "c2", "totaal"), sep=" ") %>%
  mutate(feiten=as.numeric(feiten)) %>% 
  mutate(recht=as.numeric(recht)) %>%
  mutate(totaal=as.numeric(totaal)) %>%
  select(-a, -b, -c, -c2) %>% 
  mutate(positive = positive/aantalwoorden) %>% 
  mutate(negative = negative/aantalwoorden) %>% 
  mutate(anger = anger/aantalwoorden) %>% 
  mutate(anticipation = anticipation/aantalwoorden) %>% 
  mutate(disgust = disgust/aantalwoorden) %>% 
  mutate(fear = fear/aantalwoorden) %>% 
  mutate(joy = joy/aantalwoorden) %>% 
  mutate(sadness = sadness/aantalwoorden) %>% 
  mutate(surprise = surprise/aantalwoorden) %>% 
  mutate(trust = trust/aantalwoorden) %>% 
  mutate(lengteUitspraak=nchar(uitspraak)) %>%
  mutate(rechtsgebied = ifelse(grepl("Bestuurs", rechtsgebied)==TRUE, "bestuursrecht", rechtsgebied)) %>% 
  mutate(rechtsgebied = ifelse(grepl("Civiel", rechtsgebied)==TRUE, "civielRecht", rechtsgebied)) %>% 
  mutate(rechtsgebied = ifelse(grepl("Straf", rechtsgebied)==TRUE, "strafrecht", rechtsgebied)) %>%
  mutate(rechtsgebied = ifelse(is.na(rechtsgebied), "onbekend", rechtsgebied)) %>% 
  mutate(rechtsgebied = as.factor(rechtsgebied)) %>% 
  mutate(instantie = ifelse(grepl(":RB", ecli)==TRUE, "rechtbank", "")) %>%   
  mutate(instantie = ifelse(grepl(":GH", ecli)==TRUE, "hof", instantie)) %>%   
  mutate(instantie = ifelse(grepl(":HR", ecli)==TRUE, "hogeRaad", instantie)) %>%   
  mutate(instantie = ifelse(grepl(":RVS", ecli)==TRUE, "raadVanState", instantie)) %>%   
  mutate(instantie = ifelse(instantie=="", "overig", instantie)) %>%   
  mutate(instantie = as.factor(instantie)) %>% 
  # mutate(instantie = ifelse(grepl(":Rb", ecli)==TRUE, "rechtbank", "rechtsgebied")) %>%   
  as_tibble()

saveRDS(Multivariabelen, file = "Wraking\\Multivariabelen.rds")
# Dataset <- readRDS(file = "D:\\Uitwisselmap\\Projecten\\Ecli\\Rstudio\\Wraking\\Multivariabelen.rds") 


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