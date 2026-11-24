# 00_verif_donnees.R
# Télécharge les questions écrites de l'AN (15e à 17e législature) et compte celles qui citent le moustique tigre, pour juger si le corpus est exploitable. La 14e (2012-2017) est exclue : le serveur coupe son zip après quelques Mo, depuis R comme depuis un navigateur (essais du 24/11/2026).

library(stringr)
library(here)

legislatures <- 15:17
noms_zip <- c("15" = "Questions_ecrites_XV.json.zip",
              "16" = "Questions_ecrites.json.zip",
              "17" = "Questions_ecrites.json.zip")

motif <- regex("moustiques?(\\s|-|&nbsp;|&#160;)+tigres?|albopictus", ignore_case = TRUE)

compter_legislature <- function(leg) {
  url <- paste0("https://data.assemblee-nationale.fr/static/openData/repository/",
                leg, "/questions/questions_ecrites/", noms_zip[as.character(leg)])
  zip_local <- here("data", "raw", paste0("AN_QE_leg", leg, ".zip"))
  dossier <- here("data", "raw", paste0("AN_QE_leg", leg))

  # http_version = 2 force HTTP/1.1 : en HTTP/2, le serveur de l'AN coupe les gros fichiers
  if (!file.exists(zip_local)) {
    curl::curl_download(url, zip_local, handle = curl::new_handle(http_version = 2))
  }

  # unzip() ne plante pas si l'extraction est incomplète : on compare donc le nombre de fichiers extraits au contenu du zip
  n_attendu <- sum(grepl("\\.json$", unzip(zip_local, list = TRUE)$Name))
  n_present <- length(list.files(dossier, pattern = "\\.json$", recursive = TRUE))
  if (n_present < n_attendu) unzip(zip_local, exdir = dossier, overwrite = TRUE)

  fichiers <- list.files(dossier, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(fichiers) == n_attendu)

  trouve <- vapply(fichiers, function(f) {
    txt <- readChar(f, file.size(f), useBytes = TRUE)
    Encoding(txt) <- "UTF-8"
    str_detect(txt, motif)
  }, logical(1), USE.NAMES = FALSE)

  write.csv(data.frame(fichier = fichiers[trouve]),
            here("data", "processed", paste0("AN_leg", leg, "_fichiers_moustique.csv")),
            row.names = FALSE)

  data.frame(legislature = leg, n_questions = length(fichiers), n_moustique = sum(trouve))
}

bilan <- do.call(rbind, lapply(legislatures, compter_legislature))
print(bilan)
cat("\nTotal Assemblée nationale :", sum(bilan$n_moustique), "questions\n")
