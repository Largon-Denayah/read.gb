propeptide.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/product=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "product", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  propeptide <- data.frame("Location" = "propeptide", "Qualifier" = gsub(".*propeptide +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        propeptide <- rbind(propeptide, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            propeptide[dim(propeptide)[1],2] <- paste(propeptide[dim(propeptide)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            propeptide[dim(propeptide)[1],2] <- paste(propeptide[dim(propeptide)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        propeptide <- rbind(propeptide, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  propeptide <- apply(propeptide, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  propeptide <- apply(propeptide, 2, function(x){gsub("\"", "", x, fixed = T)})
  propeptide <- apply(propeptide, 2, function(x){gsub("\\", "", x, fixed = T)})
  propeptide <- apply(propeptide, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(propeptide)
}
