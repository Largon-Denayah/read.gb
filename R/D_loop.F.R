D_loop.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  D_loop <- data.frame("Location" = "D-loop", "Qualifier" = gsub(".*D-loop +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        D_loop <- rbind(D_loop, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            D_loop[dim(D_loop)[1],2] <- paste(D_loop[dim(D_loop)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            D_loop[dim(D_loop)[1],2] <- paste(D_loop[dim(D_loop)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        D_loop <- rbind(D_loop, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  D_loop <- apply(D_loop, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  D_loop <- apply(D_loop, 2, function(x){gsub("\"", "", x, fixed = T)})
  D_loop <- apply(D_loop, 2, function(x){gsub("\\", "", x, fixed = T)})
  D_loop <- apply(D_loop, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(D_loop)
}
