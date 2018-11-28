F_UTR.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  F_UTR <- data.frame("Location" = "F_UTR", "Qualifier" = gsub(".*F_UTR +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        F_UTR <- rbind(F_UTR, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            F_UTR[dim(F_UTR)[1],2] <- paste(F_UTR[dim(F_UTR)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            F_UTR[dim(F_UTR)[1],2] <- paste(F_UTR[dim(F_UTR)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        F_UTR <- rbind(F_UTR, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  F_UTR <- apply(F_UTR, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  F_UTR <- apply(F_UTR, 2, function(x){gsub("\"", "", x, fixed = T)})
  F_UTR <- apply(F_UTR, 2, function(x){gsub("\\", "", x, fixed = T)})
  F_UTR <- apply(F_UTR, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(F_UTR)
}
