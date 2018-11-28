iDNA.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/number=", "/old_locus_tag=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "number", "old_locus_tag", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  iDNA <- data.frame("Location" = "iDNA", "Qualifier" = gsub(".*iDNA +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        iDNA <- rbind(iDNA, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            iDNA[dim(iDNA)[1],2] <- paste(iDNA[dim(iDNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            iDNA[dim(iDNA)[1],2] <- paste(iDNA[dim(iDNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        iDNA <- rbind(iDNA, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  iDNA <- apply(iDNA, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  iDNA <- apply(iDNA, 2, function(x){gsub("\"", "", x, fixed = T)})
  iDNA <- apply(iDNA, 2, function(x){gsub("\\", "", x, fixed = T)})
  iDNA <- apply(iDNA, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(iDNA)
}
