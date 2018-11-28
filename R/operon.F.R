operon.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/inference=", "/map=", "/note=", "/operon=", "/phenotype=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "inference", "map", "note", "operon", "phenotype", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  operon <- data.frame("Location" = "operon", "Qualifier" = gsub(".*operon +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        operon <- rbind(operon, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            operon[dim(operon)[1],2] <- paste(operon[dim(operon)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            operon[dim(operon)[1],2] <- paste(operon[dim(operon)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        operon <- rbind(operon, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  operon <- apply(operon, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  operon <- apply(operon, 2, function(x){gsub("\"", "", x, fixed = T)})
  operon <- apply(operon, 2, function(x){gsub("\\", "", x, fixed = T)})
  operon <- apply(operon, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(operon)
}
