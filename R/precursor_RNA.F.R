precursor_RNA.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/operon=", "/product=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "operon", "product", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  precursor_RNA <- data.frame("Location" = "precursor_RNA", "Qualifier" = gsub(".*precursor_RNA +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        precursor_RNA <- rbind(precursor_RNA, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            precursor_RNA[dim(precursor_RNA)[1],2] <- paste(precursor_RNA[dim(precursor_RNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            precursor_RNA[dim(precursor_RNA)[1],2] <- paste(precursor_RNA[dim(precursor_RNA)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        precursor_RNA <- rbind(precursor_RNA, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  precursor_RNA <- apply(precursor_RNA, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  precursor_RNA <- apply(precursor_RNA, 2, function(x){gsub("\"", "", x, fixed = T)})
  precursor_RNA <- apply(precursor_RNA, 2, function(x){gsub("\\", "", x, fixed = T)})
  precursor_RNA <- apply(precursor_RNA, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(precursor_RNA)
}
