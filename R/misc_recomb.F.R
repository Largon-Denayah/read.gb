misc_recomb.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/recombination_class=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "recombination_class", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  misc_recomb <- data.frame("Location" = "misc_recomb", "Qualifier" = gsub(".*misc_recomb +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        misc_recomb <- rbind(misc_recomb, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            misc_recomb[dim(misc_recomb)[1],2] <- paste(misc_recomb[dim(misc_recomb)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            misc_recomb[dim(misc_recomb)[1],2] <- paste(misc_recomb[dim(misc_recomb)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        misc_recomb <- rbind(misc_recomb, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  misc_recomb <- apply(misc_recomb, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  misc_recomb <- apply(misc_recomb, 2, function(x){gsub("\"", "", x, fixed = T)})
  misc_recomb <- apply(misc_recomb, 2, function(x){gsub("\\", "", x, fixed = T)})
  misc_recomb <- apply(misc_recomb, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(misc_recomb)
}
