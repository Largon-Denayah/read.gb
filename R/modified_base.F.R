modified_base.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/frequency=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/mod_base=", "/note=", "/old_locus_tag=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "frequency", "gene", "gene_synonym", "inference", "locus_tag", "map", "mod_base", "note", "old_locus_tag")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  modified_base <- data.frame("Location" = "modified_base", "Qualifier" = gsub(".*modified_base +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        modified_base <- rbind(modified_base, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            modified_base[dim(modified_base)[1],2] <- paste(modified_base[dim(modified_base)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            modified_base[dim(modified_base)[1],2] <- paste(modified_base[dim(modified_base)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        modified_base <- rbind(modified_base, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  modified_base <- apply(modified_base, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  modified_base <- apply(modified_base, 2, function(x){gsub("\"", "", x, fixed = T)})
  modified_base <- apply(modified_base, 2, function(x){gsub("\\", "", x, fixed = T)})
  modified_base <- apply(modified_base, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(modified_base)
}
