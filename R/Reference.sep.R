Reference.sep <- function(y){
  TempR <- y$REFERENCE
  c <- sort(c(gregexpr("REFERENCE", TempR)[[1]], nchar(TempR)))
  Reference <- list()

  for(i in 2:length(c)){
    Ref <- substr(TempR, c[i-1], c[i]-1)
    Ref <- unlist(strsplit(Ref, "\\n", fixed = F))

    if(length(grep("REFERENCE", Ref[1])) == 1){
      reference <- data.frame("Name" = "REFERENCE", "Detail" = gsub(".*REFERENCE +([^.]+)\"*", "\\1", Ref[1], perl = T), stringsAsFactors = F)
      for(j in 2:length(Ref)){
        if(length(grep("AUTHORS", Ref[j])) == 1){
          reference <- rbind(reference, c("AUTHORS", gsub(".*AUTHORS([^.]+)\"*", "\\1", Ref[j])))
          t <- j+1
          while(length(grep("TITLE", Ref[t])) == 0){
            reference[dim(reference)[1],2] <- paste(reference[dim(reference)[1],2], gsub("\\s", " ", Ref[t]), sep = "")
            t <- t+1
          }
        }
        if(length(grep("TITLE", Ref[j])) == 1){
          reference <- rbind(reference, c("TITLE",  gsub(".*TITLE +([^.]+)\"*", "\\1", Ref[j])))
          t <- j+1
          while(length(grep("JOURNAL", Ref[t], ignore.case = F)) == 0){
            reference[dim(reference)[1],2] <- paste(reference[dim(reference)[1],2], gsub("\\s", " ", Ref[t]), sep = "")
            t <- t+1
          }
        }
        if(length(grep("JOURNAL", Ref[j])) == 1){
          reference <- rbind(reference, c("JOURNAL",  gsub(".*JOURNAL +([^.]+)\"*", "\\1", Ref[j])))
          t <- j+1
          while((length(grep("PUBMED", Ref[t], ignore.case = F)) == 0 && t <= length(Ref)) == T){
            reference[dim(reference)[1],2] <- paste(reference[dim(reference)[1],2], gsub("\\s", " ", Ref[t]), sep = "")
            t <- t+1
          }
        }
        if(length(grep("PUBMED", Ref[j])) == 1){
          reference <- rbind(reference, c("PUBMED",  gsub(".*PUBMED +([^.]+)\"*", "\\1", Ref[j])))
        }
      }
      reference <- apply(reference, 2 , function(x){gsub("\\s+", " ", x, perl = TRUE)})
      reference <- apply(reference, 2 , function(x){gsub("[^[:alnum:][:space:].:;',@()-]", "", x, perl = TRUE)})
      Reference[[length(Reference)+1]] <- data.frame(reference, stringsAsFactors = F)
      names(Reference)[[length(Reference)]] <- gsub("(.*?)(\\s.*)", "\\1", reference[1,2])
    }
  }
  return(Reference)
}
