tmRNA.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/product=", "/pseudogene=", "/standard_name=", "/tag_peptide=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "product", "pseudogene", "standard_name", "tag_peptide")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  tmRNA <- data.frame("Location" = "tmRNA", "Qualifier" = gsub(".*tmRNA +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        tmRNA <- rbind(tmRNA, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            tmRNA[dim(tmRNA)[1],2] <- paste(tmRNA[dim(tmRNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            tmRNA[dim(tmRNA)[1],2] <- paste(tmRNA[dim(tmRNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        tmRNA <- rbind(tmRNA, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  tmRNA <- apply(tmRNA, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  tmRNA <- apply(tmRNA, 2, function(x){gsub("\"", "", x, fixed = T)})
  tmRNA <- apply(tmRNA, 2, function(x){gsub("\\", "", x, fixed = T)})
  tmRNA <- apply(tmRNA, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(tmRNA)
}
