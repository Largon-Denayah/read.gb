variation.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/compare=", "/db_xref=", "/experiment=", "/frequency=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/phenotype=", "/product=", "/replace=", "/standard_name=")
  ItemN <- c("allele", "citation", "compare", "db_xref", "experiment", "frequency", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "phenotype", "product", "replace", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  variation <- data.frame("Location" = "variation", "Qualifier" = gsub(".*variation +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        variation <- rbind(variation, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            variation[dim(variation)[1],2] <- paste(variation[dim(variation)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            variation[dim(variation)[1],2] <- paste(variation[dim(variation)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        variation <- rbind(variation, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  variation <- apply(variation, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  variation <- apply(variation, 2, function(x){gsub("\"", "", x, fixed = T)})
  variation <- apply(variation, 2, function(x){gsub("\\", "", x, fixed = T)})
  variation <- apply(variation, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(variation)
}
