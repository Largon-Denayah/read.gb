V_region.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/product=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "product", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  V_region <- data.frame("Location" = "V_region", "Qualifier" = gsub(".*V_region +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        V_region <- rbind(V_region, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            V_region[dim(V_region)[1],2] <- paste(V_region[dim(V_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            V_region[dim(V_region)[1],2] <- paste(V_region[dim(V_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        V_region <- rbind(V_region, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  V_region <- apply(V_region, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  V_region <- apply(V_region, 2, function(x){gsub("\"", "", x, fixed = T)})
  V_region <- apply(V_region, 2, function(x){gsub("\\", "", x, fixed = T)})
  V_region <- apply(V_region, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(V_region)
}
