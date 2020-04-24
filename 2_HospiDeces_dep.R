library(questionr)
library(tidyverse)
library(sf)
library(biscale)
library(cowplot)

# Imports -----------

max <- read_csv2("data/donnees-hospitalieres-covid19-2020-04-23-19h00.csv")
max <- max %>% group_by(dep) %>% summarise(hosp = max(hosp))
head(max)
max <- max %>% rename("MaxHosp"="hosp")

nouveau_dep$reahosp <- round(nouveau_dep$incid_rea / nouveau_dep$incid_hosp*100,1)

exo <- read_csv2("Departement et covid_19042020_AM.csv")

# Mise en forme données ---------
exo$Code <- ifelse(nchar(exo$Code)==1, paste0("0",exo$Code),
                   exo$Code)
head(exo$`Nombre de lits d'hopital (medecine secteur public)`)
head(exo$`Nbr de lits /100000 hab`)
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

nouveau_dep$hospCentmille <- round(nouveau_dep$incid_hosp/
  nouveau_dep$Pop*100000,0)
nouveau_dep$dcCentmille <-round(nouveau_dep$incid_dc/
  nouveau_dep$Pop*100000,0)


sp_dep <- st_read("data/shapefiles/departements-20140306-50m.shp")
head(sp_dep$code_insee)
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

# reahosp et hospDixmille
names(nouveau_dep)

data <- bi_class(sp_dep, x =dcDixmille ,
                 y = hospDixmille, style = "fisher", dim = 3)





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
                    xlab = "décès /100M hab",
                    ylab = "hospi /100M hab",
                    size = 14)

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, .1, .1, 0.25, 0.25)

tapply(data$dcCentmille,substr(data$bi_class,1,1),  summary)
tapply(data$hospCentmille,substr(data$bi_class,3,3),  summary)
ggsave("2_HospiDeces_dep.png", height = 30, width = 30, units="cm")
