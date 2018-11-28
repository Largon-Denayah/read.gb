source.F <- function(Feat, SQuali, SQualiN){
  Item <- c("/altitude=", "/bio_material=", "/cell_line=", "/cell_type=", "/chromosome=", "/citation=", "/clone=", "/clone_lib=", "/collected_by=", "/collection_date=", "/country=", "/cultivar=", "/culture_collection=", "/db_xref=", "/dev_stage=", "/ecotype=", "/haplogroup=", "/haplotype=", "/host=", "/identified_by=", "/isolate=", "/isolation_source=", "/lab_host=", "/lat_lon=", "/map=", "/mating_type=", "/mol_type=", "/note=", "/organelle=", "/organism=", "/plasmid=", "/pop_variant=", "/segment=", "/serotype=", "/serovar=", "/sex=", "/specimen_voucher=", "/strain=", "/sub_clone=", "/submitter_seqid=", "/sub_species=", "/sub_strain=", "/tissue_lib=", "/tissue_type=", "/type_material=", "/variety=")
  ItemN <- c("altitude", "bio_material", "cell_line", "cell_type", "chromosome", "citation", "clone", "clone_lib", "collected_by", "collection_date", "country", "cultivar", "culture_collection", "db_xref", "dev_stage", "ecotype", "haplogroup", "haplotype", "host", "identified_by", "isolate", "isolation_source", "lab_host", "lat_lon", "map", "mating_type", "mol_type", "note", "organelle", "organism", "plasmid", "pop_variant", "segment", "serotype", "serovar", "sex", "specimen_voucher", "strain", "sub_clone", "submitter_seqid", "sub_species", "sub_strain", "tissue_lib", "tissue_type", "type_material", "variety")
  Feat[length(Feat)] <- gsub("\\\",$", "", Feat[length(Feat)])
  source <- data.frame("Location" = "source", "Qualifier" = gsub(".*source +([^.]+)\"*", "\\1", Feat[1], perl = T), stringsAsFactors = F)
  for(i in 2:length(Feat)){
    for(j in 1:length(Item)){
      if(length(grep(Item[j], Feat[i], perl = T)) == 1){
        source <- rbind(source, c(ItemN[j],  gsub(".*=([^.]+)\"*", "\\1", Feat[i])))
        if((length(grep("\\\\\"$|\"\\\", $", Feat[i])) == 0 & i != length(Feat)) == T){
          t <- i+1
          while(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 0){
            source[dim(source)[1],2] <- paste(source[dim(source)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
            t <- t+1
          }
          if(length(grep("\\\\\"$|\"\\\", $", Feat[t])) == 1){
            source[dim(source)[1],2] <- paste(source[dim(source)[1],2], gsub("\\s", " ", Feat[t]), sep = " ")
          }
        }
      }
    }
    for(k in 1:length(SQuali)){
      if(length(grep(SQuali[k], Feat[i], perl = T)) == 1){
        source <- rbind(source, c(SQuali[k], SQualiN[k]))
      }
    }
    if(length(grep("PCR_primers=", Feat[i])) == 1){
      s <- gsub(".*=\\\\\\\"([^.]+)\"*", "\\1", Feat[i])
      k <-i+1
      while(length(grep("\\\\\"$", Feat[k])) != 1){
        s <- paste(s, Feat[k], collapse = "")
        k <- k+1
      }
      s <- paste(s, Feat[k], collapse = "")
      source <- rbind(source, c("PCR_primers",  s))
      source[length(source[,1]),2] <- gsub("\\\"", "", source[length(source[,1]),2], perl = TRUE)
    }
  }
  source <- apply(source, 2, function(x){gsub(" {2,}", " ", x, perl = TRUE)})
  source <- apply(source, 2, function(x){gsub("\"", "", x, fixed = T)})
  source <- apply(source, 2, function(x){gsub("\\", "", x, fixed = T)})
  source <- apply(source, 2, function(x){gsub("[^[:alnum:][:space:][]'.,:_<>()-]", "", x, perl = TRUE)})
  return(source)
}
