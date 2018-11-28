repeat_region.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/note=", "/old_locus_tag=", "/rpt_family=", "/rpt_type=", "/rpt_unit_range=", "/rpt_unit_seq=", "/satellite=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "note", "old_locus_tag", "rpt_family", "rpt_type", "rpt_unit_range", "rpt_unit_seq", "satellite", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  repeat_region <- data.frame("Location" = "repeat_region", "Qualifier" = gsub(".*repeat_region +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        repeat_region <- rbind(repeat_region, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            repeat_region[dim(repeat_region)[1],2] <- paste(repeat_region[dim(repeat_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            repeat_region[dim(repeat_region)[1],2] <- paste(repeat_region[dim(repeat_region)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        repeat_region <- rbind(repeat_region, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  repeat_region <- apply(repeat_region, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  repeat_region <- apply(repeat_region, 2, function(x){gsub("\"", "", x, fixed = T)})
  repeat_region <- apply(repeat_region, 2, function(x){gsub("\\", "", x, fixed = T)})
  repeat_region <- apply(repeat_region, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(repeat_region)
}
