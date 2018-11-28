rep_origin.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/direction=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "direction", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  rep_origin <- data.frame("Location" = "rep_origin", "Qualifier" = gsub(".*rep_origin +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        rep_origin <- rbind(rep_origin, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            rep_origin[dim(rep_origin)[1],2] <- paste(rep_origin[dim(rep_origin)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            rep_origin[dim(rep_origin)[1],2] <- paste(rep_origin[dim(rep_origin)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        rep_origin <- rbind(rep_origin, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  rep_origin <- apply(rep_origin, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  rep_origin <- apply(rep_origin, 2, function(x){gsub("\"", "", x, fixed = T)})
  rep_origin <- apply(rep_origin, 2, function(x){gsub("\\", "", x, fixed = T)})
  rep_origin <- apply(rep_origin, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(rep_origin)
}
