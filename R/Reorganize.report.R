Reorganize.report <- function(Temp){
  #### Separating the informations from a unique string ####
  LOCUS <- gsub(".*LOCUS([^.]+)\nDEFINITION.*", "\\1", Temp)
  LOCUS <- gsub("^ *|(?<= ) | *$", "", LOCUS, perl = TRUE)
  LOCUS <- gsub("\n", "", LOCUS)
  LOCUS <- gsub("\r", "", LOCUS)

  Db <- gregexpr("DEFINITION", Temp)[[1]][1]
  De <- gregexpr("\nACCESSION", Temp)[[1]][1]
  DEFINITION <- substr(Temp, Db+10, De-1)
  DEFINITION <- gsub("^ *|(?<= ) | *$", "", DEFINITION, perl = TRUE)
  DEFINITION <- gsub("\n", "", DEFINITION)
  DEFINITION <- gsub("\r", "", DEFINITION)

  ACCESSION <- gsub(".*ACCESSION([^.]+\n).*", "\\1", Temp)
  ACCESSION <- gsub("^ *|(?<= ) | *$", "", ACCESSION, perl = TRUE)
  ACCESSION <- gsub("\n", "", ACCESSION)
  ACCESSION <- gsub("\r", "", ACCESSION)

  VERSION <- gsub(".*VERSION([^.]+)|\n.*", "\\1", Temp)
  VERSION <- gsub("^ *|(?<= ) | *$", "", VERSION, perl = TRUE)
  VERSION <- gsub("\n", "", VERSION)
  VERSION <- gsub("\r", "", VERSION)

  Db <- gregexpr("DBLINK", Temp)[[1]][1]
  De <- gregexpr("nKEYWORDS.", Temp)[[1]][1]
  if(Db > 0){
    DBLINKa <- substr(Temp, Db+6, De-1)
    DBLINKa <- gsub("^ |(?<= ) | *$", "", DBLINKa, perl = TRUE)
    DBLINKa <- gsub("\r", "", DBLINKa)
    DBLINKa <- unlist(strsplit(DBLINKa, "\\n "))
    DBLINK <- data.frame(stringsAsFactors = F)
    for(i in 1:length(DBLINKa)){
      DB <- data.frame("Name" = unlist(strsplit(DBLINKa[i], " "))[1], "Detail" = unlist(strsplit(DBLINKa[i], " "))[2], stringsAsFactors = F)
      DBLINK <- rbind(DBLINK, DB)
    }
  }

  KEYWORDS <- gsub(".*KEYWORDS([^.]+)|\n.*", "\\1", Temp)
  KEYWORDS <- gsub("^ *|(?<= ) | *$", "", KEYWORDS, perl = TRUE)
  KEYWORDS <- gsub("\n", "", KEYWORDS)
  KEYWORDS <- gsub("\r", "", KEYWORDS)

  Sb <- gregexpr("SOURCE", Temp)[[1]][1]
  Se <- gregexpr("\n  ORGANISM", Temp)[[1]][1]
  SOURCE <- substr(Temp, Sb + 6, Se-1)
  SOURCE <- gsub("^ *|(?<= ) | *$", "", SOURCE, perl = TRUE)
  SOURCE <- gsub("\n", "", SOURCE)
  SOURCE <- gsub("\r", "", SOURCE)

  ORGANISM <- gsub(".*ORGANISM([^.]+)|\nREFERENCE.*", "\\1", Temp)
  ORGANISM <- gsub("^ *|(?<= ) | *$", "", ORGANISM, perl = TRUE)
  ORGANISM <- gsub("\n", "", ORGANISM)
  ORGANISM <- gsub("\r", "", ORGANISM)

  ## Position of REFERENCE:
  Rb <- gregexpr("REFERENCE", Temp)[[1]][1]
  Rem <- c(gregexpr("FEATURES", Temp)[[1]][1], gregexpr("COMMENT", Temp)[[1]][1])
  Re <- min(Rem[Rem > 0])
  REFERENCE <- substr(Temp, Rb, Re-1)

  ## Position of COMMENT:
  Cb <- gregexpr("COMMENT", Temp)[[1]][1]
  Ce <- gregexpr("FEATURES", Temp)[[1]][1]
  if(Cb > 0){
    COMMENT <- substr(Temp, Cb+7, Ce-1)
    COMMENT <- gsub("\r", "", COMMENT)
    COMMENT <- as.data.frame(strsplit(COMMENT, split = "\\n", perl = T)[[1]], stringsAsFactors = F)
    COMMENT <- apply(COMMENT, 1 , function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  }

  ## Position of FEATURES:
  Fb <- gregexpr("FEATURES", Temp)[[1]][1]
  Fe <- gregexpr("ORIGIN", Temp, ignore.case = F)[[1]][1]
  if(Fe == -1){
    Fe <- gregexpr("CONTIG", Temp, ignore.case = F)[[1]][1]
  }
  FEATURES <- substr(Temp, Fb+8, Fe-1)
  FEATURES <- gsub("\r", "", FEATURES)
  FEATURES <- strsplit(FEATURES, split = "\n", fixed = T)

  Ob <- gregexpr("ORIGIN", Temp, useBytes = F)[[1]][1]
  Oe <- gregexpr("$", Temp, useBytes = F)[[1]][1]
  if(Ob != -1){
  ORIGIN <- substr(Temp, Ob+6, Oe-2)
  ORIGIN <- gsub("\r", "", ORIGIN)
  ORIGIN <- strsplit(ORIGIN, split = "\n", fixed = T)
  ORIGIN <- ORIGIN[[1]][-1]
  }

  #### Grouping into a list ####
  Order <- c("LOCUS", "DEFINITION", "ACCESSION", "VERSION", "DBLINK", "KEYWORDS", "SOURCE", "ORGANISM", "REFERENCE", "COMMENT", "FEATURES", "ORIGIN")
  if(exists("COMMENT") == F){Order <- Order[-which(gregexpr("COMMENT", Order) != -1)]}
  if(exists("DBLINK") == F){Order <- Order[-which(gregexpr("DBLINK", Order) != -1)]}
  if(exists("ORIGIN") == F){Order <- Order[-which(gregexpr("ORIGIN", Order) != -1)]}

  y <- list()
  for(i in 1:length(Order)){
    y[[length(y)+1]] <- eval(parse(text = Order[i]))
    names(y)[[i]] <- Order[i]
  }
  return(y)
}
