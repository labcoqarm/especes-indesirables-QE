# 05_typologie.R
# Typologie des discours par dictionnaire : pour chaque question, on compte les termes de cinq registres et on retient le registre dominant.
# Sorties : data/processed/corpus_types.csv, figures/fig3_types_discours.png

library(here)
library(stringr)
library(dplyr)
library(ggplot2)

corpus <- read.csv(here("data", "processed", "corpus.csv"), colClasses = c(code_dep = "character"))

# Dictionnaire : \\w* = n'importe quelle fin de mot (allerg\\w* -> allergie, allergique...)
registres <- list(
  sante = c("santé", "sanitaires?", "maladies?", "dengue", "chikungunya", "zika", "virus",
            "piqûres?", "allerg\\w*", "respiratoires?", "urtica\\w*", "leptospirose",
            "infect\\w*", "épidém\\w*", "vecteurs?", "médec\\w*", "patients?"),
  nuisance = c("nuisances?", "gêne", "désagréments?", "qualité de vie", "sommeil",
               "insalubr\\w*", "hygiène", "détresse", "psycholog\\w*", "logements?",
               "infest\\w*", "inquiét\\w*"),
  biodiversite = c("biodiversité", "invasi\\w*", "envahissant\\w*", "exotiques?",
                   "écosystèmes?", "pollinis\\w*", "faune", "flore", "prédat\\w*",
                   "environnement\\w*", "espèces? protégées?"),
  economie = c("coûts?", "coûteu\\w*", "financ\\w*", "rembours\\w*", "prise en charge",
               "filières?", "apicult\\w*", "économi\\w*", "pertes?", "productions?",
               "indemnis\\w*", "euros?", "ruches?"),
  action_publique = c("plan national", "stratégie", "réglementation", "décrets?", "arrêtés?",
                      "législati\\w*", "classement", "compétences?", "collectivités?",
                      "maires?", "préfe\\w*", "dispositifs?", "moyens", "obligat\\w*")
)

texte <- str_to_lower(paste(corpus$titre, corpus$texte))
for (r in names(registres)) {
  motif <- regex(paste0("\\b(", paste(registres[[r]], collapse = "|"), ")\\b"))
  corpus[[r]] <- str_count(texte, motif)
}

# Registre dominant : celui qui a le plus d'occurrences. Égalité -> "mixte".
scores <- as.matrix(corpus[names(registres)])
maxi <- apply(scores, 1, max)
n_max <- rowSums(scores == maxi)
corpus$type <- ifelse(maxi == 0, "aucun",
               ifelse(n_max > 1, "mixte", names(registres)[max.col(scores, ties.method = "first")]))

print(table(corpus$espece, corpus$type))

write.csv(corpus, here("data", "processed", "corpus_types.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Contrôle à l'œil : 2 titres tirés au hasard par registre
set.seed(1)
corpus |> group_by(type) |> slice_sample(n = 2) |> select(type, espece, titre) |> print(n = 20)

# Figure 3 : part de chaque registre dominant, par espèce ---------------------

libelles_especes <- c(moustique_tigre = "Moustique tigre", frelon_asiatique = "Frelon asiatique",
                      rats = "Rats", punaises_de_lit = "Punaises de lit",
                      chenille_processionnaire = "Chenille processionnaire")

parts <- corpus |>
  count(espece, type) |>
  group_by(espece) |>
  mutate(part = n / sum(n), effectif = sum(n)) |>
  ungroup() |>
  mutate(espece = paste0(libelles_especes[espece], " (n = ", effectif, ")"))

couleurs <- c(sante = "#66c2a5", nuisance = "#fc8d62", biodiversite = "#8da0cb",
              economie = "#e78ac3", action_publique = "#a6d854", mixte = "grey70", aucun = "grey90")
noms_registres <- c(sante = "Santé", nuisance = "Nuisance", biodiversite = "Biodiversité",
                    economie = "Économie", action_publique = "Action publique",
                    mixte = "Mixte (égalité)", aucun = "Aucun terme")
parts$type <- factor(parts$type, levels = names(couleurs))

ggplot(parts, aes(part, espece, fill = type)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = couleurs, labels = noms_registres) +
  labs(x = "Part des questions", y = NULL, fill = "Registre dominant",
       title = "Registre dominant des questions écrites, par espèce") +
  theme_minimal(base_family = "sans")
ggsave(here("figures", "fig3_types_discours.png"), width = 9, height = 4, dpi = 300)
