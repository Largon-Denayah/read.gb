misc_structure.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  misc_structure <- data.frame("Location" = "misc_structure", "Qualifier" = gsub(".*misc_structure +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        misc_structure <- rbind(misc_structure, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            misc_structure[dim(misc_structure)[1],2] <- paste(misc_structure[dim(misc_structure)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            misc_structure[dim(misc_structure)[1],2] <- paste(misc_structure[dim(misc_structure)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        misc_structure <- rbind(misc_structure, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  misc_structure <- apply(misc_structure, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  misc_structure <- apply(misc_structure, 2, function(x){gsub("\"", "", x, fixed = T)})
  misc_structure <- apply(misc_structure, 2, function(x){gsub("\\", "", x, fixed = T)})
  misc_structure <- apply(misc_structure, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(misc_structure)
}
