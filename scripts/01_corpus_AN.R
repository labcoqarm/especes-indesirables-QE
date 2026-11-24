# 01_corpus_AN.R
# Corpus des questions écrites de l'AN (juin 2017 à 2026) portant sur une des espèces étudiées. On cherche dans la question seulement, pas dans la réponse du ministre. Sortie : data/processed/corpus_AN.csv

library(here)
library(stringr)
library(jsonlite)
source(here("scripts", "regles_corpus.R"))

# Pré-filtre sur le texte brut (rapide), affiné plus bas sur la question seule
dossiers <- here("data", "raw", paste0("AN_QE_leg", 15:17))
fichiers <- list.files(dossiers, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
motif_toutes <- regex(paste(especes, collapse = "|"), ignore_case = TRUE)

candidat <- vapply(fichiers, function(f) {
  txt <- readChar(f, file.size(f), useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  str_detect(txt, motif_toutes)
}, logical(1), USE.NAMES = FALSE)

fichiers <- fichiers[candidat]
cat(length(fichiers), "fichiers candidats sur", length(candidat), "\n")

# Lecture des questions -------------------------------------------------------

ou_na <- function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = " ; ")

lire_question <- function(f) {
  q <- fromJSON(f, simplifyVector = FALSE)$question
  idx <- q$indexationAN
  titre <- if (!is.null(idx$analyses)) idx$analyses$analyse else idx$ANALYSE$ANA # varie selon la législature
  tq <- q$textesQuestion$texteQuestion
  data.frame(
    id = q$uid,
    chambre = "AN",
    type = ou_na(q$type),
    date = ou_na(tq$infoJO$dateJO),
    acteur = ou_na(q$auteur$identite$acteurRef),
    mandat = ou_na(q$auteur$identite$mandatRef),
    groupe = ou_na(q$auteur$groupe$abrege),
    rubrique = ou_na(idx$rubrique),
    titre = ou_na(titre),
    texte = ou_na(tq$texte)
  )
}

corpus <- do.call(rbind, lapply(fichiers, lire_question))
corpus <- corpus[corpus$type == "QE", ]

# Dates au format "19/09/2017" en 15e législature, "2025-10-07" ensuite
date_iso <- as.Date(corpus$date, format = "%Y-%m-%d")
date_fr <- as.Date(corpus$date, format = "%d/%m/%Y")
corpus$date <- ifelse(is.na(date_iso), as.character(date_fr), as.character(date_iso))
stopifnot(!anyNA(corpus$date), !anyNA(corpus$texte), !anyNA(corpus$mandat))

corpus$titre <- nettoyer(corpus$titre)
corpus$texte <- nettoyer(corpus$texte)

for (e in names(especes)) {
  corpus[[e]] <- porte_sur(corpus$titre, corpus$texte, especes[[e]])
}
corpus <- corpus[rowSums(corpus[names(especes)]) > 0, ]

# Département de l'auteur -----------------------------------------------------
# Les questions ne donnent que l'identifiant du mandat ; le département est dans le fichier des mandats (un JSON par député). On n'extrait que ceux utiles.

zip_amo <- here("data", "raw", "AN_acteurs_mandats.zip")
dossier_amo <- here("data", "raw", "AN_acteurs_mandats")
if (!file.exists(zip_amo)) {
  curl::curl_download(paste0("https://data.assemblee-nationale.fr/static/openData/repository/17/amo/", "tous_acteurs_mandats_organes_xi_legislature/", "AMO30_tous_acteurs_tous_mandats_tous_organes_historique.json.zip"), zip_amo)
}
unzip(zip_amo, files = paste0("json/acteur/", unique(corpus$acteur), ".json"), exdir = dossier_amo, overwrite = TRUE)

departement_du_mandat <- function(acteur, mandat) {
  f <- file.path(dossier_amo, "json", "acteur", paste0(acteur, ".json"))
  mandats <- fromJSON(f, simplifyVector = FALSE)$acteur$mandats$mandat
  if (!is.null(mandats$uid)) mandats <- list(mandats) # un seul mandat : objet et non liste
  m <- Filter(function(x) x$uid == mandat, mandats)
  stopifnot(length(m) == 1)
  lieu <- m[[1]]$election$lieu
  c(departement = lieu$departement, code_dep = lieu$numDepartement)
}

deps <- mapply(departement_du_mandat, corpus$acteur, corpus$mandat)
corpus$departement <- deps["departement", ]
corpus$code_dep <- deps["code_dep", ]
stopifnot(!anyNA(corpus$departement))

write.csv(corpus, here("data", "processed", "corpus_AN.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\nQuestions retenues :", nrow(corpus), "\n")
print(colSums(corpus[names(especes)]))
