CDS.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/artificial_location=", "/citation=", "/codon_start=", "/db_xref=", "/EC_number=", "/exception=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/number=", "/old_locus_tag=", "/operon=", "/product=", "/protein_id=", "/pseudogene=", "/standard_name=", "/translation=", "/transl_except=", "/transl_table=")
  ItemN <- c("allele", "artificial_location", "citation", "codon_start", "db_xref", "EC_number", "exception", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note",  "number", "old_locus_tag", "operon", "product", "protein_id", "pseudogene", "standard_name", "translation", "transl_except", "transl_table")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  CDS <- data.frame("Location" = "CDS", "Qualifier" = gsub(".*CDS +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        CDS <- rbind(CDS, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            CDS[dim(CDS)[1],2] <- paste(CDS[dim(CDS)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            CDS[dim(CDS)[1],2] <- paste(CDS[dim(CDS)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        CDS <- rbind(CDS, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  CDS <- apply(CDS, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  CDS <- apply(CDS, 2, function(x){gsub("\"", "", x, fixed = T)})
  CDS <- apply(CDS, 2, function(x){gsub("\\", "", x, fixed = T)})
  CDS <- apply(CDS, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(CDS)
}
