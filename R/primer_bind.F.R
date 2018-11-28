primer_bind.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/PCR_conditions=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "PCR_conditions", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  primer_bind <- data.frame("Location" = "primer_bind", "Qualifier" = gsub(".*primer_bind +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        primer_bind <- rbind(primer_bind, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            primer_bind[dim(primer_bind)[1],2] <- paste(primer_bind[dim(primer_bind)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            primer_bind[dim(primer_bind)[1],2] <- paste(primer_bind[dim(primer_bind)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        primer_bind <- rbind(primer_bind, c(SQuali[k], SQualiN[k]))
      }
    }
    if(length(grep("PCR_primers=", Feat[i])) == 1){
      s <- gsub(".*=\\\\\\\"([^.]+)\"*", "\\1", Feat[i])
      k <-i+1
      while(length(grep("\\\\\"$", Feat[k])) != 1){
        s <- paste(s, Feat[k], collapse = "")
        k <- k+1
      }
      s <- paste(s, Feat[k], collapse = "")
      primer_bind <- rbind(primer_bind, c("PCR_primers",  s))
      primer_bind[length(primer_bind[,1]),2] <- gsub("\\\"", "", primer_bind[length(primer_bind[,1]),2], perl = TRUE)
    }
  }
  primer_bind <- apply(primer_bind, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  primer_bind <- apply(primer_bind, 2, function(x){gsub("\"", "", x, fixed = T)})
  primer_bind <- apply(primer_bind, 2, function(x){gsub("\\", "", x, fixed = T)})
  primer_bind <- apply(primer_bind, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(primer_bind)
}
