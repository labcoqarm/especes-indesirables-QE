# 00_verif_donnees.R
# Étape 0 : vérifier que les questions écrites de l'Assemblée nationale
# sont téléchargeables, et compter celles qui parlent du moustique tigre.
# On ne nettoie rien ici : on veut juste savoir si le corpus est assez gros.

library(stringr)   # str_detect() : chercher un motif dans du texte
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)      # here() : chemins construits depuis la racine du projet (.Rproj)
if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")

# 1. Paramètres -----------------------------------------------------------

legislatures <- 15:17   # juin 2017 -> 2026. La 14e (2012-2017) est exclue :
                        # le serveur de l'AN coupe son zip (128 Mo) après 1 à 4 Mo,
                        # depuis R comme depuis un navigateur (essais du 24/11/2026).

# Nom exact du zip pour chaque législature (l'archive de la 15e a un suffixe)
noms_zip <- c("15" = "Questions_ecrites_XV.json.zip",
              "16" = "Questions_ecrites.json.zip",
              "17" = "Questions_ecrites.json.zip")

# Motif recherché : "moustique tigre", "moustiques-tigres", "Aedes albopictus"...
# Entre les deux mots : un ou plusieurs espaces (\\s, y compris l'espace insécable),
# tirets ou espaces insécables écrits en HTML (&nbsp; ou &#160;). s? = "s" facultatif.
motif <- regex("moustiques?(\\s|-|&nbsp;|&#160;)+tigres?|albopictus", ignore_case = TRUE)

# 2. Fonction qui traite une législature ----------------------------------

compter_legislature <- function(leg) {

  url <- paste0("https://data.assemblee-nationale.fr/static/openData/repository/",
                leg, "/questions/questions_ecrites/", noms_zip[as.character(leg)])
  zip_local <- here("data", "raw", paste0("AN_QE_leg", leg, ".zip"))
  dossier   <- here("data", "raw", paste0("AN_QE_leg", leg))

  # Télécharger le zip s'il n'est pas déjà sur le disque.
  # curl_download() plutôt que download.file() : le serveur de l'AN coupe les
  # gros fichiers en HTTP/2 ("HTTP2 framing layer"). http_version = 2 force
  # HTTP/1.1 (dans libcurl, la valeur 2 veut dire "version 1.1").
  # En cas d'échec, curl_download() s'arrête avec une erreur et ne laisse
  # aucun fichier à moitié téléchargé (il écrit d'abord dans un fichier temporaire).
  if (!file.exists(zip_local)) {
    curl::curl_download(url, zip_local, handle = curl::new_handle(http_version = 2))
  }

  # Lire la table des matières du zip sans l'extraire.
  # Si le zip est abîmé, unzip() s'arrête ici avec une erreur.
  n_attendu <- sum(grepl("\\.json$", unzip(zip_local, list = TRUE)$Name))

  # Dézipper si le dossier manque ou s'il contient moins de fichiers que le zip
  n_present <- length(list.files(dossier, pattern = "\\.json$", recursive = TRUE))
  if (n_present < n_attendu) unzip(zip_local, exdir = dossier, overwrite = TRUE)

  # Un fichier JSON = une question
  fichiers <- list.files(dossier, pattern = "\\.json$",
                         recursive = TRUE, full.names = TRUE)

  # Garde-fou : on doit avoir exactement autant de fichiers que dans le zip.
  # Sinon le script s'arrête (dézippage incomplet, disque plein...).
  stopifnot(length(fichiers) == n_attendu)

  # On lit chaque fichier comme un bloc de texte brut et on teste le motif.
  # vapply() renvoie un vecteur TRUE/FALSE de même longueur que 'fichiers'.
  trouve <- vapply(fichiers, function(f) {
    txt <- readChar(f, file.size(f), useBytes = TRUE)
    Encoding(txt) <- "UTF-8"   # les JSON de l'AN sont en UTF-8 : on le dit à R
    str_detect(txt, motif)
  }, logical(1), USE.NAMES = FALSE)

  # On garde la liste des fichiers retenus pour l'étape 1
  write.csv(data.frame(fichier = fichiers[trouve]),
            here("data", "processed", paste0("AN_leg", leg, "_fichiers_moustique.csv")),
            row.names = FALSE)

  data.frame(legislature = leg, n_questions = length(fichiers),
             n_moustique = sum(trouve))
}

# 3. Lancer sur les 3 législatures et afficher le bilan -------------------

bilan <- do.call(rbind, lapply(legislatures, compter_legislature))
print(bilan)
cat("\nTotal Assemblée nationale :", sum(bilan$n_moustique), "questions\n")
