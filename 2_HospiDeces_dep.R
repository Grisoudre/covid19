library(questionr)
library(tidyverse)
library(sf)
library(biscale)
library(cowplot)

# Imports -----------

max <- read_csv2("data/SP_Hospitalisations/donnees-hospitalieres-covid19-2020-04-25-19h00.csv")
tests <- read_csv2("data/SP_tests/donnees-tests-covid19-labo-quotidien-2020-04-25-19h00.csv")
exo <- read_csv2("data/EP/Departement et covid_19042020_AM.csv")
sp_dep <- st_read("data/shapefiles/departements-20140306-50m.shp")

# Mise en forme ------------

exo$Code <- ifelse(nchar(exo$Code)==1, paste0("0",exo$Code),
                   exo$Code)

# Ajout des tests --------
tests <- tests %>% 
  filter(clage_covid =="0")
tests$nb_tests_cum <- tests$nb_test
tests$nb_pos_cum <- tests$nb_pos
for (i in 2:nrow(tests)){
  if(tests$dep[i]==tests$dep[i-1]){
    tests$nb_tests_cum[i] <- tests$nb_tests_cum[i-1] + tests$nb_tests_cum[i] 
    tests$nb_pos_cum[i] <- tests$nb_pos_cum[i-1] + tests$nb_pos_cum[i] 
  }
}
tests <- tests %>% 
  filter(jour == max(jour)) %>% 
  select(dep,jour, nb_tests_cum, nb_pos_cum)  %>% 
  mutate(part_pos = round(nb_pos_cum/nb_tests_cum*100,1))

names(tests)[names(tests)=="nb_tests_cum"] <- paste0("nb_tests_cum_",unique(tests$jour))
names(tests)[names(tests)=="nb_pos_cum"] <- paste0("nb_pos_cum_",unique(tests$jour))
names(tests)[names(tests)=="part_pos"] <- paste0("part_pos_",unique(tests$jour))


exo <- merge(exo, 
             tests %>% select(-jour),
             by.x="Code", by.y="dep", all.x=T)

tests <- read_csv2("data/SP_tests/donnees-tests-covid19-labo-quotidien-2020-04-25-19h00.csv")

tests <- tests %>% 
  filter(clage_covid =="0" & jour <= '2020-04-13')
tests$nb_tests_cum <- tests$nb_test
tests$nb_pos_cum <- tests$nb_pos
for (i in 2:nrow(tests)){
  if(tests$dep[i]==tests$dep[i-1]){
    tests$nb_tests_cum[i] <- tests$nb_tests_cum[i-1] + tests$nb_tests_cum[i] 
    tests$nb_pos_cum[i] <- tests$nb_pos_cum[i-1] + tests$nb_pos_cum[i] 
  }
}
tests <- tests %>% 
  filter(jour == max(jour)) %>% 
  select(dep,jour, nb_tests_cum, nb_pos_cum)  %>% 
  mutate(part_pos = round(nb_pos_cum/nb_tests_cum*100,1))

names(tests)[names(tests)=="nb_tests_cum"] <- paste0("nb_tests_cum_",unique(tests$jour))
names(tests)[names(tests)=="nb_pos_cum"] <- paste0("nb_pos_cum_",unique(tests$jour))
names(tests)[names(tests)=="part_pos"] <- paste0("part_pos_",unique(tests$jour))


exo <- merge(exo, 
             tests %>% select(-jour),
             by.x="Code", by.y="dep", all.x=T)


# Ajout du nb max d'hospitalisation------

max <- max %>% group_by(dep) %>% summarise(hosp = max(hosp))

max <- max %>% rename("MaxHosp"="hosp")

nouveau_dep$reahosp <- round(nouveau_dep$incid_rea / nouveau_dep$incid_hosp*100,1)

# Ajout du nb de lits ------

exo <- exo %>% rename("NbLitsHopPubl"="Nombre de lits d'hopital (medecine secteur public)",
                      "NbLitsHopPublCentMille"="Nbr de lits /100000 hab")
nouveau_dep <- merge(nouveau_dep,
                     exo %>% select(Code, Pop),
                     by.x="dep",
                     by.y="Code",
                     all.x=T)
nouveau_dep <- merge(nouveau_dep,
                     exo %>% select(Code, NbLitsHopPubl,NbLitsHopPublCentMille),
                     by.x="dep",
                     by.y="Code",
                     all.x=T)

# Ajout des hospi et décès / 100 000 hab -------

nouveau_dep$hospCentmille <- round(nouveau_dep$incid_hosp/
  nouveau_dep$Pop*100000,0)
nouveau_dep$dcCentmille <-round(nouveau_dep$incid_dc/
  nouveau_dep$Pop*100000,0)


# Carte bi-variée ---------
sp_dep$code_insee <- as.character(sp_dep$code_insee)
sp_dep <- sp_dep %>% filter(nchar(code_insee)==2)

sp_dep <- merge(sp_dep,
                nouveau_dep,
                by.x="code_insee",
                by.y="dep",all.x=T)

sp_dep <- merge(sp_dep,
                max,
                by.x="code_insee",
                by.y="dep",all.x=T)


data <- bi_class(sp_dep, x =hospDixmille ,
                 y = NbLitsHopPublCentMille, style = "fisher", dim = 3)




pal <- "DkViolet"
map<-ggplot()+ 
  # annotation_map_tile(zoom=9,
  #                     cachedir = system.file("rosm.cache",
  #                                            package = "ggspatial"),
  #                     type="cartolight")+
  geom_sf(data=data,
          aes(fill=bi_class),
          color="snow2",
          size=.1,
          show.legend =F) +
    bi_scale_fill(pal = pal, dim = 3)+
  labs(title = "", 
       caption="Source : Santé Publique France, package : slu-openGIS/biscale - style = fisher, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=18))+
  coord_sf( datum = NA)
legend <- bi_legend(pal = pal,
                    dim = 3,
                    xlab = "max hospi",
                    ylab = "nb de lits",
                    size = 14)

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, .1, .1, 0.25, 0.25)


tapply(data$dcCentmille,substr(data$bi_class,1,1),  summary)
tapply(data$hospCentmille,substr(data$bi_class,3,3),  summary)

# Exports -------
ggsave("2_LitsMaxHospi_dep.png", height = 30, width = 30, units="cm")

write_csv2(exo, paste0("output/Departements_Covid_", Sys.Date(),".csv"))
