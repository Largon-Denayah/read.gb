DNA.treatment <- function(y){
  Seq <- y$ORIGIN
  Seq <- gsub("\\s", "", Seq, perl = TRUE)
  Seq <- gsub("[[:digit:]]", "", Seq, perl = TRUE)

  SeqF <- paste(Seq[1:length(Seq)], collapse = "")
}
