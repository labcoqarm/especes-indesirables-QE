# 02_corpus_Senat.R
# Corpus des questions écrites du Sénat (depuis le 21/06/2017, comme l'AN)
# portant sur une des espèces étudiées, avec les mêmes règles que le script 01.
# Source : dump PostgreSQL de data.senat.fr, lu comme un fichier texte.
# Sortie : data/processed/corpus_Senat.csv

library(here)
library(stringr)
source(here("scripts", "regles_corpus.R"))

zip_senat <- here("data", "raw", "senat_questions.zip")
sql       <- here("data", "raw", "questions.sql")
if (!file.exists(zip_senat)) {
  curl::curl_download("https://data.senat.fr/data/questions/questions.zip", zip_senat)
}
if (!file.exists(sql)) unzip(zip_senat, files = "questions.sql", exdir = here("data", "raw"))

# Lecture de la table tam_questions -------------------------------------------
# Dans le dump, ses lignes sont entre "COPY tam_questions (colonnes) FROM stdin;"
# et "\." : une question par ligne, colonnes séparées par des tabulations,
# "\N" pour une valeur vide. Le fichier fait 1 Go, on le lit par paquets.

motif_toutes <- regex(paste(especes, collapse = "|"), ignore_case = TRUE)
con <- file(sql, open = "r", encoding = "UTF-8")
colonnes <- NULL
lignes   <- character(0)

repeat {
  paquet <- readLines(con, n = 50000)
  if (length(paquet) == 0) stop("tam_questions introuvable ou incomplète dans le dump")

  if (is.null(colonnes)) {
    debut <- which(startsWith(paquet, "COPY tam_questions "))
    if (length(debut) == 0) next
    colonnes <- strsplit(str_match(paquet[debut], "\\((.*)\\)")[, 2], ", ")[[1]]
    paquet   <- paquet[-(1:debut)]
  }

  fin <- which(paquet == "\\.")
  if (length(fin) > 0) paquet <- paquet[seq_len(fin[1] - 1)]
  lignes <- c(lignes, paquet[str_detect(paquet, motif_toutes)])
  if (length(fin) > 0) break
}
close(con)

stopifnot(all(str_count(lignes, fixed("\t")) == length(colonnes) - 1))
senat <- as.data.frame(str_split_fixed(lignes, fixed("\t"), length(colonnes)))
names(senat) <- colonnes
senat[senat == "\\N"] <- NA

senat$date <- as.Date(substr(senat$datejodepot, 1, 10))
senat <- senat[senat$natquecod == "QE" & senat$date >= as.Date("2017-06-21"), ]
stopifnot(!anyNA(senat$date), !anyNA(senat$circonscription))

# Même format que le corpus AN ------------------------------------------------

sans_echappements <- function(x) str_replace_all(x, "\\\\[nrt]", " ")  # \n, \r, \t du dump

corpus <- data.frame(
  id          = senat$reference,
  chambre     = "Senat",
  type        = senat$natquecod,
  date        = as.character(senat$date),
  acteur      = senat$matricule,
  mandat      = NA,
  groupe      = senat$groupe,
  rubrique    = senat$rubrique,
  titre       = nettoyer(sans_echappements(senat$titre)),
  texte       = nettoyer(sans_echappements(senat$txtque)),
  departement = senat$circonscription,  # nom seulement, le code est ajouté au script 03
  code_dep    = NA
)
stopifnot(!anyNA(corpus$texte))

for (e in names(especes)) {
  corpus[[e]] <- porte_sur(corpus$titre, corpus$texte, especes[[e]])
}
corpus <- corpus[rowSums(corpus[names(especes)]) > 0, ]

write.csv(corpus, here("data", "processed", "corpus_Senat.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\nQuestions retenues :", nrow(corpus), "\n")
print(colSums(corpus[names(especes)]))
