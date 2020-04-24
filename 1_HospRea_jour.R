# COVID19 - Personnes en réanimation et hospitalisées par jour =========

# packages ----
library(tidyverse)
library(magick)

# import ----------
encours <- read_csv2("donnees-hospitalieres-covid19-2020-04-23-19h00.csv")

# Mise en forme -------
encours$hosp <- encours$hosp - encours$rea
encours <- encours %>% 
  filter(sexe ==0) %>% 
  group_by(jour) %>% 
  summarise(hosp=sum(hosp),
            rea = sum(rea))
encours <-gather(encours, Hospitalisation, nb,-jour)

max <- max(encours %>% group_by(jour) %>% summarise(nb=sum(nb)) %>% select(nb))
min <- min(encours %>% group_by(jour) %>% summarise(nb=sum(nb)) %>% select(nb))

# Graphes --------

c <-seq(as.integer(as.Date(min(encours$jour))),as.integer(as.Date(max(encours$jour))))

for (i in c)
{
  encoursi <- encours %>% filter(jour == as.integer(i))
encoursi$fraction <- encoursi$nb/sum(encoursi$nb)
 encoursi$ymax <-  cumsum(encoursi$fraction)
  encoursi$ymin  <-  c(0, head(encoursi$ymax, n=-1))
  
  xmin <- 6 - sum(encoursi$nb) * 4 / (max-min)
  xmax <- 6 + sum(encoursi$nb) * 4 / (max-min)
  
  encoursi$Hospitalisation <- fct_recode(encoursi$Hospitalisation,
                                              "Hors réanimation"="hosp",
                                              "En réanimation"="rea")
  
  ggplot(encoursi)+ 
    aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=Hospitalisation) +
    geom_rect(col = "black", size=.5)+
    coord_polar(theta="y") +
    xlim(c(0, 12))+
    viridis::scale_fill_viridis(discrete=T)+
    labs(title = paste0("Hospitalisations en cours en raison de\nla covid19 en France par jour\n\n",
                        as.character(as.Date(i,origin="1970-01-01")),
                        " : ",sum(encoursi$nb)),
         fill="",
         caption = "Source : Santé Publique France, 2020\n")+
    theme_void()+ 
    theme(plot.title =element_text(hjust=.5) )
  ggsave(filename = paste0(as.character(as.Date(i,origin="1970-01-01")),".png"),
         width = 17, height = 20, units ="cm")
  
}

# gif -----------

list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("TT1_covid.gif") # write to current dir

