library(tidyverse)
library(rjson)

# charge le fichier des prénoms italiens masculins
# source : https://data.world/axtscz/italian-first-names
prénoms_italiens_garçons <- fromJSON(file = "ITGivenMale.json") %>%
  map(1) %>%
  unlist() %>%
  toupper()

# charge le fichier des prénoms italiens féminins
# source : https://data.world/axtscz/italian-first-names
prénoms_italiens_filles <- fromJSON(file = "ITGivenFemale.json") %>%
  map(1) %>%
  unlist() %>%
  toupper()

# crée un vecteur unique de prénoms italiens
prénoms_italiens <- c(prénoms_italiens_garçons, prénoms_italiens_filles)

# charge le fichier des prénoms de l'INSEE, ajoute une variable identifiant les prénoms italiens,
# calcule la proportion des nouveau-nés avec un de ces prénoms et fait les transformations en vue
# de la création d'un graphique avec ggplot
# source : https://www.insee.fr/fr/statistiques/2540004
prénoms_insee <- read_delim("nat2017.txt", delim = "\t") %>%
  mutate(italien = preusuel %in% prénoms_italiens) %>%
  group_by(annais, italien) %>%
  summarize(n = sum(nombre)) %>%
  mutate(proportion = n / sum(n)) %>%
  filter(annais != "XXXX" & italien == TRUE) %>%
  select(année = annais, proportion)

# crée un graphique qui montre l'évolution de la proportion des nouveau-nés ayant reçu un prénom italien
ggplot(prénoms_insee, aes(x = année, y = proportion, group = 1)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Proportion des nouveau-nés ayant reçu un prénom italien en France entre 1900 et 2017") +
  xlab("Année") +
  ylab("Proportion") +
  scale_x_discrete(breaks = seq(1900, 2017, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))
