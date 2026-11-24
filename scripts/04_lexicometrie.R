# 04_lexicometrie.R
# Analyse lexicale du corpus : évolution du nombre de questions, mots les plus spécifiques de chaque espèce (tf-idf) et cooccurrences de mots (bigrammes).
# Sorties : figures/fig1_evolution.png, figures/fig2_mots_specifiques.png, data/processed/bigrammes.csv

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)

corpus <- read.csv(here("data", "processed", "corpus.csv"))
corpus$texte <- gsub("['’]", " ", corpus$texte) # "l'attention" -> "l attention"

# Mots vides : liste standard du français, formules propres aux questions écrites, et noms des espèces (sinon ils sortent en tête partout)
mots_vides <- c(
  get_stopwords("fr")$word,
  "attire", "appelle", "attention", "interroge", "alerte", "ministre", "ministère",
  "gouvernement", "souhaite", "savoir", "connaître", "entend", "intentions",
  "madame", "monsieur", "mme", "état", "secrétaire", "ainsi", "afin", "cette",
  "plus", "être", "dont", "très", "aussi", "comme", "notamment", "également",
  "contre", "demande", "déjà", "aujourd", "hui",
  "moustique", "moustiques", "tigre", "tigres", "frelon", "frelons", "asiatique",
  "asiatiques", "punaise", "punaises", "lit", "lits", "chenille", "chenilles",
  "processionnaire", "processionnaires", "rat", "rats"
)

libelles <- as_labeller(c(
  moustique_tigre = "Moustique tigre", frelon_asiatique = "Frelon asiatique",
  rats = "Rats", punaises_de_lit = "Punaises de lit",
  chenille_processionnaire = "Chenille processionnaire"
))

# Figure 1 : nombre de questions par an et par espèce -------------------------

par_an <- count(corpus, annee, espece)

ggplot(par_an, aes(annee, n)) +
  geom_col(fill = "grey40") +
  facet_wrap(~ espece, scales = "free_y", labeller = libelles) +
  labs(x = NULL, y = "Questions écrites",
       title = "Questions écrites par espèce (AN + Sénat)",
       caption = "2017 : à partir du 21 juin. 2026 : jusqu'à novembre.") +
  theme_minimal(base_family = "sans")
ggsave(here("figures", "fig1_evolution.png"), width = 9, height = 5, dpi = 300)

# Figure 2 : mots spécifiques de chaque espèce (tf-idf) -----------------------
# On compte dans combien de questions chaque mot apparaît, par espèce. Le tf-idf fait ressortir les mots fréquents pour une espèce et rares ailleurs. Un mot doit apparaître dans au moins 2 questions pour éviter les cas isolés.

mots <- corpus |>
  unnest_tokens(mot, texte) |>
  filter(!mot %in% mots_vides, !grepl("[0-9]", mot), nchar(mot) > 2) |>
  distinct(id, espece, mot)

specifiques <- mots |>
  count(espece, mot) |>
  filter(n >= 2) |>
  bind_tf_idf(mot, espece, n) |>
  group_by(espece) |>
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  ungroup()

ggplot(specifiques, aes(tf_idf, reorder_within(mot, tf_idf, espece))) +
  geom_col(fill = "grey40") +
  facet_wrap(~ espece, scales = "free", labeller = libelles) +
  scale_y_reordered() +
  labs(x = "tf-idf", y = NULL, title = "Mots les plus spécifiques de chaque espèce") +
  theme_minimal(base_family = "sans")
ggsave(here("figures", "fig2_mots_specifiques.png"), width = 10, height = 6, dpi = 300)

# Cooccurrences : paires de mots consécutifs (bigrammes) -----------------------

bigrammes <- corpus |>
  unnest_tokens(bigramme, texte, token = "ngrams", n = 2) |>
  separate(bigramme, c("mot1", "mot2"), sep = " ") |>
  filter(!mot1 %in% mots_vides, !mot2 %in% mots_vides,
         !grepl("[0-9]", mot1), !grepl("[0-9]", mot2)) |>
  count(espece, mot1, mot2, sort = TRUE)

write.csv(bigrammes, here("data", "processed", "bigrammes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

print(bigrammes |> group_by(espece) |> slice_head(n = 5), n = 25)

# Questions quasi identiques : deux questions dont plus de 60 % des suites de 5 mots sont communes (rapporté au texte le plus court)
cinq_mots <- corpus |> unnest_tokens(g, texte, token = "ngrams", n = 5) |> distinct(id, g)
cinq_mots <- split(cinq_mots$g, cinq_mots$id)
paires <- combn(names(cinq_mots), 2)
recouvrement <- apply(paires, 2, function(p) {
  a <- cinq_mots[[p[1]]]
  b <- cinq_mots[[p[2]]]
  length(intersect(a, b)) / min(length(a), length(b))
})
doublons <- unique(c(paires[, recouvrement > 0.6]))
cat("\nQuestions ayant au moins un quasi-doublon :", length(doublons), "sur", nrow(corpus), "\n")
print(table(corpus$espece[corpus$id %in% doublons]))
