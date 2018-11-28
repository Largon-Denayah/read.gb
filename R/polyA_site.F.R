polyA_site.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  polyA_site <- data.frame("Location" = "polyA_site", "Qualifier" = gsub(".*polyA_site +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        polyA_site <- rbind(polyA_site, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            polyA_site[dim(polyA_site)[1],2] <- paste(polyA_site[dim(polyA_site)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            polyA_site[dim(polyA_site)[1],2] <- paste(polyA_site[dim(polyA_site)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        polyA_site <- rbind(polyA_site, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  polyA_site <- apply(polyA_site, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  polyA_site <- apply(polyA_site, 2, function(x){gsub("\"", "", x, fixed = T)})
  polyA_site <- apply(polyA_site, 2, function(x){gsub("\\", "", x, fixed = T)})
  polyA_site <- apply(polyA_site, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(polyA_site)
}
