#' @include assembly_gap.F.R
#' @include C_region.F.R
#' @include CDS.F.R
#' @include centromere.F.R
#' @include D_loop.F.R
#' @include D_segment.F.R
#' @include exon.F.R
#' @include F_UTR.F.R
#' @include gap.F.R
#' @include gene.F.R
#' @include iDNA.F.R
#' @include intron.F.R
#' @include J_segment.F.R
#' @include mat_peptide.F.R
#' @include misc_binding.F.R
#' @include misc_difference.F.R
#' @include misc_feature.F.R
#' @include misc_recomb.F.R
#' @include misc_RNA.F.R
#' @include misc_structure.F.R
#' @include mobile_element.F.R
#' @include modified_base.F.R
#' @include mRNA.F.R
#' @include N_region.F.R
#' @include ncRNA.F.R
#' @include old_sequence.F.R
#' @include operon.F.R
#' @include oriT.F.R
#' @include polyA_site.F.R
#' @include precursor_RNA.F.R
#' @include prim_transcript.F.R
#' @include primer_bind.F.R
#' @include propeptide.F.R
#' @include protein_bind.F.R
#' @include regulatory.F.R
#' @include rep_origin.F.R
#' @include repeat_region.F.R
#' @include rRNA.F.R
#' @include S_region.F.R
#' @include sig_peptide.F.R
#' @include source.F.R
#' @include stem_loop.F.R
#' @include STS.F.R
#' @include T_UTR.F.R
#' @include telomere.F.R
#' @include tmRNA.F.R
#' @include transit_peptide.F.R
#' @include tRNA.F.R
#' @include unsure.F.R
#' @include V_region.F.R
#' @include V_segment.F.R
#' @include variation.F.R

Feature.sep <- function(y){
  TempF <- y$FEATURES
  Feature <- list()
  Tag <- c(" +assembly_gap {2,}", " +C_region {2,}", " +CDS {2,}", " +centromere {2,}", " +D-loop {2,}", " +D_segment {2,}", " +exon {2,}", " +gap {2,}",
           " +gene {2,}", " +iDNA {2,}", " +intron {2,}", " +J_segment {2,}", " +mat_peptide {2,}", " +misc_binding {2,}", " +misc_difference {2,}",
           " +misc_feature {2,}", " +misc_recomb {2,}", " +misc_RNA {2,}", " +misc_structure {2,}", " +mobile_element {2,}", " +modified_base {2,}",
           " +mRNA {2,}", " +ncRNA {2,}", " +N_region {2,}", " +old_sequence {2,}", " +operon {2,}", " +oriT {2,}", " +polyA_site {2,}", " +precursor_RNA {2,}",
           " +prim_transcript {2,}", " +primer_bind {2,}", " +propeptide {2,}", " +protein_bind {2,}", " +regulatory {2,}", " +repeat_region {2,}",
           " +rep_origin {2,}", " +rRNA {2,}", " +S_region {2,}", " +sig_peptide {2,}", " +source {2,}", " +stem_loop {2,}", " +STS {2,}", " +telomere {2,}",
           " +tmRNA {2,}", " +transit_peptide {2,}", " +tRNA {2,}", " +unsure {2,}", " +V_region {2,}", " +V_segment {2,}", " +variation {2,}", " +variation {2,}", " +variation {2,}")
  TagChr <- c()
  SQuali <- c("/environmental_sample", "/focus", "/germline", "/macronuclear", "/partial", "/proviral", "/pseudo(?!g)",
              "/rearranged", "/ribosomal_slippage", "/transgenic", "/trans_splicing")
  SQualiN <- c("environmental_sample", "focus", "germline", "macronuclear", "partial", "proviral", "pseudo", "rearranged",
               "ribosomal_slippage", "transgenic", "trans_splicing")

  ## Source part :
  for(i in 1:length(Tag)){
    if(grepl(Tag[i], TempF, fixed = F)[[1]] == TRUE){
      P <- as.numeric(gregexpr(Tag[i], TempF, fixed = F)[[1]])
      for(j in 1:length(P)){
        if(P[j] != -1){
          TagChr <- c(TagChr, P[j])
        }
      }
    }
  }
  TagChr <- sort(TagChr)

  for(k in 2:length(TagChr)){
    Feat <- substr(TempF, TagChr[k-1], TagChr[k]-3)
    Feat <- unlist(strsplit(Feat, "\\\", \\n?\\\"", fixed = F))
    if(length(grep(" +assembly_gap +", Feat[1])) == 1){
      assembly_gap <- assembly_gap.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(assembly_gap, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "assembly_gap"
    }
    if(length(grep(" +C_region +", Feat[1])) == 1){
      C_region <- C_region.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(C_region, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "C_region"
    }
    if(length(grep(" +CDS +", Feat[1])) == 1){
      CDS <- CDS.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(CDS, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "CDS"
    }
    if(length(grep(" +centromere +", Feat[1])) == 1){
      centromere <- centromere.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(centromere, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "centromere"
    }
    if(length(grep(" +D-loop +", Feat[1])) == 1){
      D_loop <- D_loop.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(D_loop, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "D_loop"
    }
    if(length(grep(" +D_segment +", Feat[1])) == 1){
      D_segment <- D_segment.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(D_segment, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "D_segment"
    }
    if(length(grep(" +exon +", Feat[1])) == 1){
      exon <- exon.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(exon, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "exon"
    }
    if(length(grep(" +gap +", Feat[1])) == 1){
      gap <- gap.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(gap, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "gap"
    }
    if(length(grep(" +gene +", Feat[1])) == 1){
      gene <- gene.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(gene, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "gene"
    }
    if(length(grep(" +iDNA +", Feat[1])) == 1){
      iDNA <- iDNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(iDNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "iDNA"
    }
    if(length(grep(" +intron +", Feat[1])) == 1){
      intron <- intron.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(intron, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "intron"
    }
    if(length(grep(" +J_segment +", Feat[1])) == 1){
      J_segment <- J_segment.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(J_segment, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "J_segment"
    }
    if(length(grep(" +mat_peptide +", Feat[1])) == 1){
      mat_peptide <- mat_peptide.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(mat_peptide, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "mat_peptide"
    }
    if(length(grep(" +misc_binding +", Feat[1])) == 1){
      misc_binding <- misc_binding.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_binding, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_binding"
    }
    if(length(grep(" +misc_difference +", Feat[1])) == 1){
      misc_difference <- misc_difference.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_difference, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_difference"
    }
    if(length(grep(" +misc_feature +", Feat[1])) == 1){
      misc_feature <- misc_feature.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_feature, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_feature"
    }
    if(length(grep(" +misc_recomb +", Feat[1])) == 1){
      misc_recomb <- misc_recomb(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_recomb, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_recomb"
    }
    if(length(grep(" +misc_RNA +", Feat[1])) == 1){
      misc_RNA <- misc_RNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_RNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_RNA"
    }
    if(length(grep(" +misc_structure +", Feat[1])) == 1){
      misc_structure <- misc_structure.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(misc_structure, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "misc_structure"
    }
    if(length(grep(" +mobile_element +", Feat[1])) == 1){
      mobile_element <- mobile_element.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(mobile_element, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "mobile_element"
    }
    if(length(grep(" +modified_base +", Feat[1])) == 1){
      modified_base <- modified_base.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(modified_base, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "modified_base"
    }
    if(length(grep(" +mRNA +", Feat[1])) == 1){
      mRNA <- mRNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(mRNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "mRNA"
    }
    if(length(grep(" +ncRNA +", Feat[1])) == 1){
      ncRNA <- ncRNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(ncRNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "ncRNA"
    }
    if(length(grep(" +N_region +", Feat[1])) == 1){
      N_region <- N_region.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(N_region, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "N_region"
    }
    if(length(grep(" +old_sequence +", Feat[1])) == 1){
      old_sequence <- old_sequence.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(old_sequence, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "old_sequence"
    }
    if(length(grep(" +operon +", Feat[1])) == 1){
      operon <- operon.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(operon, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "operon"
    }
    if(length(grep(" +oriT +", Feat[1])) == 1){
      oriT <- oriT.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(oriT, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "oriT"
    }
    if(length(grep(" +polyA_site +", Feat[1])) == 1){
      polyA_site <- polyA_site.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(polyA_site, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "polyA_site"
    }
    if(length(grep(" +precursor_RNA +", Feat[1])) == 1){
      precursor_RNA <- precursor_RNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(precursor_RNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "precursor_RNA"
    }
    if(length(grep(" +prim_transcript +", Feat[1])) == 1){
      prim_transcript <- prim_transcript.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(prim_transcript, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "prim_transcript"
    }
    if(length(grep(" +primer_bind +", Feat[1])) == 1){
      primer_bind <- primer_bind.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(primer_bind, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "primer_bind"
    }
    if(length(grep(" +propeptide +", Feat[1])) == 1){
      propeptide <- propeptide.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(propeptide, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "propeptide"
    }
    if(length(grep(" +protein_bind +", Feat[1])) == 1){
      protein_bind <- protein_bind.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(protein_bind, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "protein_bind"
    }
    if(length(grep(" +regulatory +", Feat[1])) == 1){
      regulatory <- regulatory.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(regulatory, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "regulatory"
    }
    if(length(grep(" +repeat_region +", Feat[1])) == 1){
      repeat_region <- repeat_region.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(repeat_region, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "repeat_region"
    }
    if(length(grep(" +rep_origin +", Feat[1])) == 1){
      rep_origin <- rep_origin.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(rep_origin, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "rep_origin"
    }
    if(length(grep(" +rRNA +", Feat[1])) == 1){
      rRNA <- rRNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(rRNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "rRNA"
    }
    if(length(grep(" +S_region +", Feat[1])) == 1){
      S_region <- S_region.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(S_region, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "S_region"
    }
    if(length(grep(" +sig_peptide +", Feat[1])) == 1){
      sig_peptide <- sig_peptide.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(sig_peptide, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "sig_peptide"
    }
    if(length(grep(" +source +", Feat[1])) == 1){
      source <- source.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(source, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "source"
    }
    if(length(grep(" +stem_loop +", Feat[1])) == 1){
      stem_loop <- stem_loop.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(stem_loop, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "stem_loop"
    }
    if(length(grep(" +STS +", Feat[1])) == 1){
      STS <- STS.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(STS, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "STS"
    }
    if(length(grep(" +telomere +", Feat[1])) == 1){
      telomere <- telomere.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(telomere, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "telomere"
    }
    if(length(grep(" +tmRNA +", Feat[1])) == 1){
      tmRNA <- tmRNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(tmRNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "tmRNA"
    }
    if(length(grep(" +transit_peptide +", Feat[1])) == 1){
      transit_peptide <- transit_peptide.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(transit_peptide, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "transit_peptide"
    }
    if(length(grep(" +tRNA +", Feat[1])) == 1){
      tRNA <- tRNA.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(tRNA, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "tRNA"
    }
    if(length(grep(" +unsure +", Feat[1])) == 1){
      unsure <- unsure.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(unsure, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "unsure"
    }
    if(length(grep(" +V_region +", Feat[1])) == 1){
      V_region <- V_region.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(V_region, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "V_region"
    }
    if(length(grep(" +V_segment +", Feat[1])) == 1){
      V_segment <- V_segment.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(V_segment, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "V_segment"
    }
    if(length(grep(" +variation +", Feat[1])) == 1){
      variation <- variation.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(variation, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "variation"
    }
    if(length(grep(" +3[[:punct:]]UTR +", Feat[1])) == 1){
      T_UTR <- T_UTR.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(T_UTR, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "T_UTR"
    }
    if(length(grep(" +5[[:punct:]]UTR +", Feat[1])) == 1){
      F_UTR <- F_UTR.F(Feat, SQuali, SQualiN)
      Feature[[length(Feature)+1]] <- data.frame(F_UTR, stringsAsFactors = F)
      names(Feature)[[length(Feature)]] <- "F_UTR"
    }

  }
  return(Feature)
}
