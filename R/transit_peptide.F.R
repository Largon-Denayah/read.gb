transit_peptide.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/product=", "/pseudogene=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "product", "pseudogene", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  transit_peptide <- data.frame("Location" = "transit_peptide", "Qualifier" = gsub(".*transit_peptide +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        transit_peptide <- rbind(transit_peptide, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            transit_peptide[dim(transit_peptide)[1],2] <- paste(transit_peptide[dim(transit_peptide)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            transit_peptide[dim(transit_peptide)[1],2] <- paste(transit_peptide[dim(transit_peptide)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        transit_peptide <- rbind(transit_peptide, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  transit_peptide <- apply(transit_peptide, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  transit_peptide <- apply(transit_peptide, 2, function(x){gsub("\"", "", x, fixed = T)})
  transit_peptide <- apply(transit_peptide, 2, function(x){gsub("\\", "", x, fixed = T)})
  transit_peptide <- apply(transit_peptide, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(transit_peptide)
}
