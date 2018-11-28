mobile_element.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/allele=", "/citation=", "/db_xref=", "/experiment=", "/function=", "/gene=", "/gene_synonym=", "/inference=", "/locus_tag=", "/map=", "/mobile_element_type=", "/note=", "/old_locus_tag=", "/rpt_family=", "/rpt_type=", "/standard_name=")
  ItemN <- c("allele", "citation", "db_xref", "experiment", "function", "gene", "gene_synonym", "inference", "locus_tag", "map", "mobile_element_type", "note", "old_locus_tag", "rpt_family", "rpt_type", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  mobile_element <- data.frame("Location" = "mobile_element", "Qualifier" = gsub(".*mobile_element +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        mobile_element <- rbind(mobile_element, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            mobile_element[dim(mobile_element)[1],2] <- paste(mobile_element[dim(mobile_element)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            mobile_element[dim(mobile_element)[1],2] <- paste(mobile_element[dim(mobile_element)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        mobile_element <- rbind(mobile_element, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  mobile_element <- apply(mobile_element, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  mobile_element <- apply(mobile_element, 2, function(x){gsub("\"", "", x, fixed = T)})
  mobile_element <- apply(mobile_element, 2, function(x){gsub("\\", "", x, fixed = T)})
  mobile_element <- apply(mobile_element, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(mobile_element)
}
