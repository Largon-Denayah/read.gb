C_region.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/product=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "product", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  C_region <- data.frame("Location" = "C_region", "Qualifier" = gsub(".*C_region +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        C_region <- rbind(C_region, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            C_region[dim(C_region)[1],2] <- paste(C_region[dim(C_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            C_region[dim(C_region)[1],2] <- paste(C_region[dim(C_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        C_region <- rbind(C_region, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  C_region <- apply(C_region, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  C_region <- apply(C_region, 2, function(x){gsub("\"", "", x, fixed = T)})
  C_region <- apply(C_region, 2, function(x){gsub("\\", "", x, fixed = T)})
  C_region <- apply(C_region, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(C_region)
}
