regulatory.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/bound_moiety=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/operon=", "/phenotype=", "/pseudogene=", "/regulatory_class=", "/standard_name=")
  ItemN <- c("allele", "bound_moiety", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "operon", "phenotype", "pseudogene", "regulatory_class", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  regulatory <- data.frame("Location" = "regulatory", "Qualifier" = gsub(".*regulatory +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        regulatory <- rbind(regulatory, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            regulatory[dim(regulatory)[1],2] <- paste(regulatory[dim(regulatory)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            regulatory[dim(regulatory)[1],2] <- paste(regulatory[dim(regulatory)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        regulatory <- rbind(regulatory, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  regulatory <- apply(regulatory, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  regulatory <- apply(regulatory, 2, function(x){gsub("\"", "", x, fixed = T)})
  regulatory <- apply(regulatory, 2, function(x){gsub("\\", "", x, fixed = T)})
  regulatory <- apply(regulatory, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(regulatory)
}
