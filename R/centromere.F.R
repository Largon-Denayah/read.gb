centromere.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/citation=", "/db_xref=", "/experiment=", "/inference=", "/note=", "/standard_name=")
  ItemN <- c("citation", "db_xref", "experiment", "inference", "note", "standard_name")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  centromere <- data.frame("Location" = "centromere", "Qualifier" = gsub(".*centromere +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        centromere <- rbind(centromere, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            centromere[dim(centromere)[1],2] <- paste(centromere[dim(centromere)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            centromere[dim(centromere)[1],2] <- paste(centromere[dim(centromere)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        centromere <- rbind(centromere, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  centromere <- apply(centromere, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  centromere <- apply(centromere, 2, function(x){gsub("\"", "", x, fixed = T)})
  centromere <- apply(centromere, 2, function(x){gsub("\\", "", x, fixed = T)})
  centromere <- apply(centromere, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(centromere)
}
