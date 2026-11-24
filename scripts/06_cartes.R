# 06_cartes.R
# Géographie des questions : taux par département (questions pour 1 million
# d'habitants), registre dominant, et comparaison selon le type de département.
# Sorties : figures/fig4_cartes.png, figures/fig5_densite.png,
#           data/processed/taux_par_densite.csv

library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)

corpus <- read.csv(here("data", "processed", "corpus_types.csv"), colClasses = c(code_dep = "character"))

# Contours des départements métropolitains (simplifiés, d'après IGN / OSM)
geo <- here("data", "raw", "departements.geojson")
if (!file.exists(geo)) {
  curl::curl_download("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson", geo)
}
departements <- st_read(geo, quiet = TRUE) |> st_transform(2154)  # Lambert-93

# Population des départements : somme des communes de la grille Insee (pop. 2022).
# Code département = 2 premiers caractères du code commune, 3 pour l'outre-mer.
communes <- read_excel(here("data", "raw", "insee_grille_densite_2025.xlsx"),
                       sheet = "Maille communale", skip = 4)
communes$code_dep <- ifelse(startsWith(communes$CODGEO, "97"),
                            substr(communes$CODGEO, 1, 3), substr(communes$CODGEO, 1, 2))
population <- communes |> group_by(code_dep) |> summarise(pop = sum(PMUN22))

densite <- read_excel(here("data", "raw", "insee_grille_densite_2025.xlsx"),
                      sheet = "Maille départementale", skip = 4) |>
  select(code_dep = DEP, densite = LIBDENS)

# Indicateurs par département ---------------------------------------------------
# Registre dominant : calculé seulement s'il y a au moins 3 questions classées
# (hors "mixte" et "aucun"), sinon trop fragile.

registre_dep <- corpus |>
  filter(!type %in% c("mixte", "aucun")) |>
  count(code_dep, type) |>
  group_by(code_dep) |>
  filter(sum(n) >= 3) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  select(code_dep, registre = type)

indicateurs <- population |>
  left_join(count(corpus, code_dep, name = "questions"), by = "code_dep") |>
  mutate(questions = coalesce(questions, 0L), taux = questions / pop * 1e6) |>
  left_join(registre_dep, by = "code_dep") |>
  left_join(densite, by = "code_dep")

carte <- departements |> left_join(indicateurs, by = c("code" = "code_dep"))
stopifnot(!anyNA(carte$pop))

# Figure 4 : deux cartes côte à côte -------------------------------------------

noms_registres <- c(sante = "Santé", nuisance = "Nuisance", biodiversite = "Biodiversité",
                    economie = "Économie", action_publique = "Action publique")

carte_taux <- ggplot(carte) +
  geom_sf(aes(fill = taux), colour = "white", linewidth = 0.1) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, name = "Questions\npar million\nd'habitants") +
  labs(title = "Volume de questions") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))

carte_registre <- ggplot(carte) +
  geom_sf(aes(fill = registre), colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = c(sante = "#66c2a5", nuisance = "#fc8d62", biodiversite = "#8da0cb",
                               economie = "#e78ac3", action_publique = "#a6d854"),
                    labels = noms_registres, na.value = "grey90",
                    name = "Registre dominant\n(gris : moins de\n3 questions)") +
  labs(title = "Registre dominant") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))

png(here("figures", "fig4_cartes.png"), width = 10, height = 5, units = "in", res = 300)
gridExtra::grid.arrange(carte_taux, carte_registre, ncol = 2,
                        bottom = "Questions écrites AN + Sénat, juin 2017 - nov. 2026. France métropolitaine.")
dev.off()

# Figure 5 : taux selon le type de département et l'espèce ----------------------

pop_densite <- indicateurs |> group_by(densite) |> summarise(pop = sum(pop))

taux_densite <- corpus |>
  count(densite, espece) |>
  left_join(pop_densite, by = "densite") |>
  mutate(taux = n / pop * 1e6)

write.csv(taux_densite, here("data", "processed", "taux_par_densite.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(taux_densite)

taux_densite$type_dep <- factor(taux_densite$densite,
  levels = c("Département à prédominance rurale", "Département intermédiaire",
             "Département à prédominance urbaine"),
  labels = c("Rural", "Intermédiaire", "Urbain"))
libelles <- as_labeller(c(
  moustique_tigre = "Moustique tigre", frelon_asiatique = "Frelon asiatique",
  rats = "Rats", punaises_de_lit = "Punaises de lit",
  chenille_processionnaire = "Chenille processionnaire"
))

ggplot(taux_densite, aes(taux, type_dep)) +
  geom_col(fill = "grey40") +
  facet_wrap(~ espece, scales = "free_x", labeller = libelles) +
  labs(x = "Questions pour 1 million d'habitants", y = NULL,
       title = "Questions écrites selon le type de département (grille de densité Insee)") +
  theme_minimal(base_family = "sans")
ggsave(here("figures", "fig5_densite.png"), width = 10, height = 5, dpi = 300)
