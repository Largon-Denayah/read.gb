gap.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/estimated_length=", "/experiment=", "/inference=", "/map=", "/note=")
  ItemN <- c("estimated_length", "experiment", "inference", "map", "note")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  gap <- data.frame("Location" = "gap", "Qualifier" = gsub(".*gap +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        gap <- rbind(gap, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            gap[dim(gap)[1],2] <- paste(gap[dim(gap)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            gap[dim(gap)[1],2] <- paste(gap[dim(gap)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        gap <- rbind(gap, c(SQuali[k], SQualiN[k]))
      }
    }
  }
  gap <- apply(gap, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  gap <- apply(gap, 2, function(x){gsub("\"", "", x, fixed = T)})
  gap <- apply(gap, 2, function(x){gsub("\\", "", x, fixed = T)})
  gap <- apply(gap, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(gap)
}
