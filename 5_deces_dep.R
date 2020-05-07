# Décès départements =========

# packages -------
library(questionr)
library(tidyverse)
library(sf)
library(patchwork)

# import ---------

dc_can <- read_csv2("data/INSEE_deces/DecesJ_dep.csv")
# load("data/BC/dc_jour_departement_hugree.Rdata")
# dc_can <- dc_jour_departement
popdep <- read_csv2("data/EP/popdep.csv")
dc_cov <- read_csv2("data/INSEE_deces/DC_jan2018-avr2020_det.csv")

# changer les noms : dc_can, dc_cov

# Mise en forme des données ------

# Population des départements :

popdep <- popdep %>% 
  gather(key="annee", value="pop", -dep, -dep_lib)

popdep$dep <- ifelse(nchar(popdep$dep)==1,
                     paste0("0",popdep$dep),
                     popdep$dep)
# dc_cov :
dc_cov$DATED <- as.Date(paste(dc_cov$ADEC,
                               dc_cov$MDEC,
                               dc_cov$JDEC, sep="-"))
dc_cov <- dc_cov %>% filter((DATED >= as.Date("2018-03-01") &
                                 DATED < as.Date("2018-04-21"))|
                                (DATED >= as.Date("2019-03-01") &
                                   DATED < as.Date("2019-04-21"))|
                                (DATED >= as.Date("2020-03-01") &
                                   DATED < as.Date("2020-04-21"))
)

dc_cov$DATEX <- as.Date(paste0("2000-",substr(dc_cov$DATED,6,10)))

dc_cov$DEP <- ifelse(substr(dc_cov$COMDEC,1,2)=="97",
                      substr(dc_cov$COMDEC,1,3),
                      substr(dc_cov$COMDEC,1,2))


dc_cov <- dc_cov %>% 
  group_by(DEP, ADEC, DATED, DATEX) %>% 
  summarise(n=n())

dc_cov <- merge(dc_cov,
                   popdep %>%
                     select(dep, annee, pop),
                by.x = c("DEP","ADEC"),
                   by.y=c("dep","annee"),
                   all.x=T)



dc_cov$npop <- dc_cov$n/dc_cov$pop*100000

dc_cov <- merge(dc_cov,
                   popdep %>% select(dep, dep_lib) %>% unique(),
                   by.y="dep",by.x="DEP", all.x=T)


# dc_can

# dc_can$DEP <- dc_can$departement
# dc_can$ADEC <- substr(dc_can$date_deces,1,4)
# dc_can$DATED <- dc_can$date_deces
# dc_can$DATEX <- as.Date(paste0("2000-",substr(dc_can$DATED,6,10)))
# dc_can$n <- dc_can$N
dc_can$DEP <- dc_can$dep
dc_can$ADEC <- substr(dc_can$dateD,1,4)
dc_can$DATED <- dc_can$dateD
dc_can$DATEX <- as.Date(paste0("2000-",substr(dc_can$DATED,6,10)))



# dc_candep <- dc_can
# dc_candep <- dc_can %>% 
#   group_by(DEP, ADEC, DATED, DATEX) %>% 
#   summarise(n=n())
dc_candep <- merge(dc_can,
                    popdep %>% 
                      select(dep, annee, pop),
                    by.x=c("DEP","ADEC"),
                    by.y=c("dep","annee"),all.x=T)

dc_candep$npop <- dc_candep$n/dc_candep$pop*100000


dc_candep <- merge(dc_candep,
                   popdep %>% select(dep, dep_lib) %>% unique(),
                   by.x="DEP",
                   by.y = "dep",all.x=T)


# Génération des graphes ----------

dc_candep$dep <- dc_candep$DEP
dc_candep$annee<- dc_candep$ADEC
dc_candep$datex <- dc_candep$DATEX

c <- unique(dc_cov$DEP)

for (j in 1:length(c)){
  i <- c[j]

can <- dc_candep %>% 
  filter(dep==i) %>% 
  filter(annee == "2001" |
           annee == "2002" |
           annee == "2003" ) %>% 
  filter(datex < as.Date("2000-09-01") &
           datex>=as.Date("2000-07-11") ) %>% 
  ggplot() +
  aes(x=datex, y=npop, group=annee, col = annee)+
  geom_line(stat="identity")  +
  scale_x_date(breaks="1 week", date_labels  = "%d-%b")+
  labs(title = paste0("Nombres de décès quotidiens - ",
                      unique(dc_candep[dc_candep$dep==i,"dep"]),
                      " - ",
                      unique(dc_candep[dc_candep$dep==i,"dep_lib"])),
       y ="Pour 100.000 hab.",x="date", col="Année")+
  viridis::scale_color_viridis(discrete=T, end=.8)+
  theme_bw()


cov <- dc_cov %>% 
  filter(DEP==i) %>% 
  filter(ADEC == "2018" |
           ADEC == "2019" |
           ADEC == "2020" ) %>% 
  ggplot() +
  aes(x=DATEX, y=npop, group=as.character(ADEC), col = as.character(ADEC))+
  geom_line(stat="identity")+
  scale_x_date(breaks="1 week", date_labels  = "%d-%b")+
  labs(
       y ="Pour 100.000 hab.",x="date", col="Année",
       caption = "Source : Insee, état civil")+
  viridis::scale_color_viridis(discrete=T, end=.8)+
  theme_bw()

can/cov
ggsave(filename = paste0("graphes_canicule_covid/",
                         unique(dc_cov[dc_cov$DEP==i,"DEP"]),
                         "_",
                         unique(dc_cov[dc_cov$DEP==i,"dep_lib"]),
                         ".png"),
       width = 20, height = 15, units ="cm")

}

# France ---------
# Mise en forme ----
popfr <- popdep %>% filter(dep=="France métropolitaine et DOM")
dc_canfr <- dc_candep %>% 
  group_by(ADEC, DATED, DATEX) %>% 
  summarise(n = sum(n))
dc_canfr <- merge(dc_canfr,
                  popfr %>% select(annee,
                                   pop), by.x="ADEC",by.y="annee",all.x=T)

dc_canfr$npop <- dc_canfr$n / dc_canfr$pop * 100000
dc_covfr <- dc_cov %>% 
  group_by(ADEC,DATED,DATEX) %>% 
  summarise(n = sum(n))
dc_covfr <- merge(dc_covfr,
                  popfr %>% select(annee,
                                   pop), by.x="ADEC",by.y="annee",all.x=T)


dc_covfr$npop <- dc_covfr$n / dc_covfr$pop * 100000

# Graphe ----------
can <- dc_canfr %>% 
  filter(ADEC == "2001" |
           ADEC == "2002" |
           ADEC == "2003" ) %>% 
  filter(DATEX < as.Date("2000-09-01") &
           DATEX>=as.Date("2000-07-11") ) %>% 
  ggplot() +
  aes(x=DATEX, y=npop, group=ADEC, col = ADEC)+
  geom_line(stat="identity")  +
  scale_x_date(breaks="1 week", date_labels  = "%d-%b")+
  labs(title ="Nombres de décès quotidiens - France",
       y ="Pour 100.000 hab.",x="date", col="Année")+
  viridis::scale_color_viridis(discrete=T, end=.8)+
  theme_bw()

cov <- dc_covfr %>% 
  filter(ADEC == "2018" |
           ADEC == "2019" |
           ADEC == "2020" ) %>% 
  ggplot() +
  aes(x=DATEX, y=npop, group=as.character(ADEC), col = as.character(ADEC))+
  geom_line(stat="identity")+
  scale_x_date(breaks="1 week", date_labels  = "%d-%b")+
  labs(
    y ="Pour 100.000 hab.",x="date", col="Année",
    caption = "Source : Insee, état civil")+
  viridis::scale_color_viridis(discrete=T, end=.8)+
  theme_bw()

can/cov
ggsave(filename = "graphes_canicule_covid//99_France.png",
       width = 20, height = 15, units ="cm")

