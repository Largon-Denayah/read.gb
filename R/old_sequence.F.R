old_sequence.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/compare=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/replace=")
  ItemN <- c("allele", "citation", "compare", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "replace")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  old_sequence <- data.frame("Location" = "old_sequence", "Qualifier" = gsub(".*old_sequence +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        old_sequence <- rbind(old_sequence, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            old_sequence[dim(old_sequence)[1],2] <- paste(old_sequence[dim(old_sequence)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            old_sequence[dim(old_sequence)[1],2] <- paste(old_sequence[dim(old_sequence)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        old_sequence <- rbind(old_sequence, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  old_sequence <- apply(old_sequence, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  old_sequence <- apply(old_sequence, 2, function(x){gsub("\"", "", x, fixed = T)})
  old_sequence <- apply(old_sequence, 2, function(x){gsub("\\", "", x, fixed = T)})
  old_sequence <- apply(old_sequence, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(old_sequence)
}
