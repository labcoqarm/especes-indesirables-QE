# regles_corpus.R
# Règles communes à l'AN (script 01) et au Sénat (script 02) : motifs des espèces, règle de sélection, nettoyage du HTML. Nécessite stringr.

sep <- "(\\s|-|&nbsp;|&#160;)+" # espace(s), tiret ou espace insécable HTML

especes <- c(
  moustique_tigre = paste0("moustiques?", sep, "tigres?|albopictus"),
  frelon_asiatique = paste0("frelons?", sep, "asiatiques?|vespa", sep, "velutina"),
  # on exclut le rat taupier (un campagnol) et le rat musqué
  rats = paste0("\\brats?\\b(?!", sep, "(taupiers?|musqu[ée]s?))|\\bsurmulots?\\b"),
  punaises_de_lit = paste0("punaises?", sep, "de", sep, "lits?"),
  chenille_processionnaire = paste0("chenilles?", sep, "processionnaires?")
)

# Une question porte sur une espèce si celle-ci est dans le titre ou citée au moins deux fois. Une mention isolée (rats de laboratoire...) ne suffit pas.
porte_sur <- function(titre, texte, motif) {
  m <- regex(motif, ignore_case = TRUE)
  str_detect(titre, m) | str_count(paste(titre, texte), m) >= 2
}

nettoyer <- function(x) {
  x <- str_replace_all(x, "<[^>]+>", " ")
  x <- str_replace_all(x, "&nbsp;|&#160;", " ")
  x <- str_replace_all(x, "&rsquo;|&#8217;", "'")
  x <- str_replace_all(x, "&amp;", "&")
  x <- str_replace_all(x, "&[a-zA-Z0-9#]+;", " ")
  str_squish(x)
}
