# 03_fusion.R
# Fusionne les corpus AN et Sénat, retire les faux positifs relus à la main, harmonise les départements (COG 2025) et ajoute la typologie de densité Insee.
# Sortie : data/processed/corpus.csv

library(here)
library(stringr)
library(readxl)
source(here("scripts", "regles_corpus.R"))

# Fichiers Insee : liste des départements (COG 2025) et grille de densité 2025
insee <- c(insee_departements_2025.csv = "https://www.insee.fr/fr/statistiques/fichier/8377162/v_departement_2025.csv", insee_grille_densite_2025.xlsx = "https://www.insee.fr/fr/statistiques/fichier/8571524/fichier_diffusion_2025.xlsx")
for (f in names(insee)) {
  if (!file.exists(here("data", "raw", f))) curl::curl_download(insee[[f]], here("data", "raw", f))
}

an <- read.csv(here("data", "processed", "corpus_AN.csv"), colClasses = c(code_dep = "character"))
senat <- read.csv(here("data", "processed", "corpus_Senat.csv"), colClasses = c(code_dep = "character"))
corpus <- rbind(an, senat)

# Faux positifs relus à la main : l'espèce est citée mais n'est pas le sujet
faux_positifs <- c(
  QANR5L15QE19089 = "rats de laboratoire (dioxyde de titane)",
  QANR5L16QE9825 = "rats de laboratoire (dioxane)",
  SEQ171001497 = "rats de laboratoire (nanoparticules)",
  QANR5L16QE4126 = "rat cité comme proie du putois"
)
stopifnot(all(names(faux_positifs) %in% corpus$id))
corpus <- corpus[!corpus$id %in% names(faux_positifs), ]

# Chaque question porte sur une seule espèce : on en fait une colonne
stopifnot(all(rowSums(corpus[names(especes)]) == 1))
corpus$espece <- names(especes)[apply(corpus[names(especes)], 1, which)]

# Départements : le Sénat ne donne que le nom, on retrouve le code dans le COG, puis on reprend les noms officiels ("Réunion" à l'AN, "La Réunion" au Sénat)
cog <- read.csv(here("data", "raw", "insee_departements_2025.csv"), colClasses = "character")
sans_code <- is.na(corpus$code_dep)
corpus$code_dep[sans_code] <- cog$DEP[match(corpus$departement[sans_code], cog$LIBELLE)]
corpus$departement <- cog$LIBELLE[match(corpus$code_dep, cog$DEP)]
stopifnot(!anyNA(corpus$code_dep), !anyNA(corpus$departement))

# Typologie de densité des départements (Insee, grille 2025, onglet départemental)
densite <- read_excel(here("data", "raw", "insee_grille_densite_2025.xlsx"), sheet = "Maille départementale", skip = 4)
i <- match(corpus$code_dep, densite$DEP)
corpus$densite <- densite$LIBDENS[i]
corpus$part_rural <- round(densite$P_RURAL[i], 1)
stopifnot(!anyNA(corpus$densite))

corpus$annee <- as.integer(substr(corpus$date, 1, 4))
corpus <- corpus[, c("id", "chambre", "date", "annee", "espece", "departement", "code_dep", "densite", "part_rural", "acteur", "groupe", "rubrique", "titre", "texte")]

write.csv(corpus, here("data", "processed", "corpus.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("Questions :", nrow(corpus), "\n\n")
print(table(corpus$espece, corpus$densite))
