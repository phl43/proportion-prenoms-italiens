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

# charge le fichier de prénoms français
# source : https://github.com/MatthiasWinkelmann/firstname-database
prénoms_français <- read_delim("firstnames.csv", delim = ";") %>%
  filter(!is.na(France)) %>%
  mutate(name = toupper(str_replace_all(iconv(name, to = "ASCII//TRANSLIT"), "['`\\^\"]", "")))

# crée un vecteur unique de prénoms italiens en prenant l'union des prénoms italiens de garçons
# et de filles moins les prénoms français qui sont inclus dans le fichier des prénoms italiens
prénoms_italiens <- setdiff(c(prénoms_italiens_garçons, prénoms_italiens_filles), prénoms_français$name)

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

# charge le fichier des nouveau-nés selon le pays de naissance de la mère
# source : https://www.insee.fr/fr/statistiques/3576462?sommaire=3576483
# (note : j'ai modifié le fichier pour qu'il soit plus simple à lire)
naissances_insee <- read_csv("naissances.csv")

# calcule la proportion de nouveau-nés dont la mère est née en Espagne ou en Italie
# note : l'INSEE ne publie pas les chiffres pour l'Italie séparément sur son site
naissances_espagne_italie <- naissances_insee %>%
  mutate(proportion = `Espagne ou Italie` / Ensemble) %>%
  select(année = Année, proportion)

# crée un graphique qui montre l'évolution de la proportion des nouveau-nés dont la mère est née en Espagne
ggplot(naissances_espagne_italie, aes(x = année, y = proportion, group = 1)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Proportion des nouveau-nés en France métropolitaine entre 1977 et 2017 dont la mère est née en Espagne") +
  xlab("Année") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))
