library(jsonlite)
library(tidyverse)
library(janitor)

##data

gc_2018_id_raw <- fromJSON("https://www.ge.ch/elections/20180415/GC/datas/2327/GC_infosCandidat.json")

gc_2018_res_raw <- fromJSON("https://www.ge.ch/elections/20180415/GC/datas/2327/GC_Resultats_Canton.json") #resultats canton

gc_2018_part_raw <- fromJSON("https://www.ge.ch/elections/20180415/GC/datas/2327/GC_infosListes.json")

#wrangling

gc_2018_part <- gc_2018_part_raw %>%
  clean_names() %>%
  rename(parti = nom,
         num_parti = no_depot) %>%
  select(parti, id)


gc_2018_res <- pluck(gc_2018_res_raw, "repartitionDesSieges") 

gc_2018_res_list <-  gc_2018_res$liste 

gc_2018 <- gc_2018_res_list %>%
  tibble(gc_2018_res) %>%
  clean_names() %>%
  select(id,suffrage_pourcent, nb_sieges) %>%
  left_join(gc_2018_part, by = c("id" = "id")) %>%
  rename("Pourcentages des voix obtenus" = suffrage_pourcent,
         "Répartition des sièges" = nb_sieges) #quorum à 7%, fct_collapse?

gc_2018$parti <- gsub("DÉMOCRATE-CHRÉTIEN","LE CENTRE",gc_2018$parti)

write_csv(gc_2018, "gc_2018.csv")




###Regierungsrat 2023
regierungsrat_data <- read.csv("https://abstimmungen.bl.ch/election/regierungsratswahlen-2019/data-csv",fileEncoding = "UTF-8")

###Landrat 2023
#Parteien und Sitze
landrat_data <- read.csv("https://abstimmungen.bl.ch/elections/landratswahlen-2019/data-parties-csv",fileEncoding = "UTF-8")

landrat_data$name <- gsub("MitteBL","Mitte",landrat_data$name)
landrat_data$name <- gsub("DieMitte","Mitte",landrat_data$name)
landrat_data$name <- gsub("CVP","Mitte",landrat_data$name)
landrat_data$name <- gsub("BDP","Mitte",landrat_data$name)
landrat_data$name <- gsub("glp","GLP",landrat_data$name)
landrat_data$name <- gsub("sp","SP",landrat_data$name)
landrat_data$name <- gsub("svp","SVP",landrat_data$name)

landrat_parteien <- landrat_data %>%
  filter(domain == "canton") %>%
  group_by(name) %>%
  mutate(voters_count_percentage = sum(voters_count_percentage),
         mandates = sum(mandates)) %>%
  select(year,
         id,
         name,
         voters_count_percentage,
         mandates) %>%
  distinct(name,.keep_all = TRUE)

landrat_sitze_superregionen_2023 <- landrat_data %>% 
  filter(domain == "superregion") %>%
  group_by(domain_segment) %>%
  mutate(mandates = sum(mandates)) %>%
  select(domain_segment,
         mandates) %>%
  distinct(domain_segment,.keep_all = TRUE)

#Personen
landrat_personen <- read.csv("https://abstimmungen.bl.ch/elections/landratswahlen-2019/data-csv",fileEncoding = "UTF-8")

landrat_personen$candidate_party <- gsub("DieMitteBL","Mitte",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("MitteBL","Mitte",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("DieMitte","Mitte",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("CVP","Mitte",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("BDP","Mitte",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("glp","GLP",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("sp","SP",landrat_personen$candidate_party)
landrat_personen$candidate_party <- gsub("svp","SVP",landrat_personen$candidate_party)

#Wie viele Gemeinden sind ausgezaehlt?
counted_gemeinden <- landrat_personen %>%
  filter(entity_counted == "True") %>%
  distinct(entity_id)

#Wahlkreise
links_wahlkreise <- c("1-1-allschwil","1-2-binningen","1-3-oberwil",
                      "2-4-reinach","2-5-muenchenstein","2-6-muttenz","2-7-laufen",
                      "3-8-pratteln","3-9-liestal",
                      "4-10-sissach","4-11-gelterkinden","4-12-waldenburg")
landrat_wahlkreise <- read.csv(paste0("https://abstimmungen.bl.ch/election/landratswahlen-2019-region-",links_wahlkreise[1],
                                      "/data-parties-csv"),fileEncoding = "UTF-8")

landrat_wahlkreise$name <- gsub("DieMitteBL","Mitte",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("MitteBL","Mitte",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("DieMitte","Mitte",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("CVP","Mitte",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("BDP","Mitte",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("glp","GLP",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("sp","SP",landrat_wahlkreise$name)
landrat_wahlkreise$name <- gsub("svp","SVP",landrat_wahlkreise$name)

landrat_wahlkreise <- landrat_wahlkreise %>% 
  group_by(name) %>%
  mutate(votes = sum(votes),
         mandates = sum(mandates)
  ) %>%
  select(domain_segment,
         year,
         id,
         name,
         mandates,
         votes,
         total_votes)  %>%
  distinct(name,.keep_all = TRUE)

for (w in 2:12) {
  landrat_wahlkreise_new <- read.csv(paste0("https://abstimmungen.bl.ch/election/landratswahlen-2019-region-",links_wahlkreise[w],
                                            "/data-parties-csv"),fileEncoding = "UTF-8")
  
  landrat_wahlkreise_new$name <- gsub("MitteBL","Mitte",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("DieMitte","Mitte",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("CVP","Mitte",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("BDP","Mitte",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("glp","GLP",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("sp","SP",landrat_wahlkreise_new$name)
  landrat_wahlkreise_new$name <- gsub("svp","SVP",landrat_wahlkreise_new$name)
  
  landrat_wahlkreise_new <- landrat_wahlkreise_new %>% 
    group_by(name) %>%
    mutate(votes = sum(votes),
           mandates = sum(mandates)
    ) %>%
    select(domain_segment,
           year,
           id,
           name,
           mandates,
           votes,
           total_votes)  %>%
    distinct(name,.keep_all = TRUE)
  
  landrat_wahlkreise <- rbind(landrat_wahlkreise,landrat_wahlkreise_new)  
} 