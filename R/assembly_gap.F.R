assembly_gap.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/estimated_length=", "/gap_type=", "/linkage_evidence=")
  ItemN <- c("estimated_length", "gap_type", "linkage_evidence")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  assembly_gap <- data.frame("Location" = "assembly_gap", "Qualifier" = gsub(".*assembly_gap +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        assembly_gap <- rbind(assembly_gap, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            assembly_gap[dim(assembly_gap)[1],2] <- paste(assembly_gap[dim(assembly_gap)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            assembly_gap[dim(assembly_gap)[1],2] <- paste(assembly_gap[dim(assembly_gap)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        assembly_gap <- rbind(assembly_gap, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  assembly_gap <- apply(assembly_gap, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  assembly_gap <- apply(assembly_gap, 2, function(x){gsub("\"", "", x, fixed = T)})
  assembly_gap <- apply(assembly_gap, 2, function(x){gsub("\\", "", x, fixed = T)})
  assembly_gap <- apply(assembly_gap, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(assembly_gap)
}
