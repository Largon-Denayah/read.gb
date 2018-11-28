#' This function opens files with .gb extention from the NCBI/GenBank Nucleotide website and returns a list containing shaped record(s).
#' Records with > 200'000 bp may experience long processing times, especially if they have numerous FEATURES items
#'
#' @param File  character. The name of the file which the data are to be read from. It can contains several records
#' @param DNA   logical. If TRUE, the DNA sequence in the ORIGIN part will be merged in one character string. If FALSE, the default layout will be kept. Default if TRUE
#' @param Type  character. Should the output contain FEATURES and REFERENCE parts ? Possible values are "full" for a full record, "nofeat" to ignore FEATURES part, "noref" to ignore REFERENCE part and "nfnr" to ignore both parts. Default is "full"
#'
#' @include DNA.treatment.R
#' @include Feature.sep.R
#' @include Reference.sep.R
#' @include Reorganize.report.R
#'
#' @return a table containing the data
#'
#' @examples
#' \dontrun{
#' read.gb(File = "sequence.gb", DNA = TRUE, Type = "full")
#' }
#'
#' @export
read.gb <- function(File, DNA = TRUE, Type = "full"){
  x <- readChar(File, file.info(File)$size, nchars = 99999999)
  Sample <- list()
  SampleS <- gregexpr("LOCUS", x)[[1]]
  SampleE <- gregexpr("(?<!:)//", x, perl = T)[[1]]
  for(k in 1:length(SampleS)){
    Temp <- substr(x, SampleS[k], SampleE[k])
    y <- Reorganize.report(Temp)

    if(Type == "full" | Type == "nofeat"){
      y$REFERENCE <- Reference.sep(y)
    }
    if(Type == "full" | Type == "noref"){
      y$FEATURES <- Feature.sep(y)
    }
    if(Type == "noref" | Type == "nfnr"){
      y[["REFERENCE"]] <- NULL
    }
    if(Type == "nofeat" | Type == "nfnr"){
      y[["FEATURES"]] <- NULL
    }

    if(DNA == TRUE){
      y$ORIGIN <- DNA.treatment(y)
    }
    Sample[[length(Sample) + 1]] <- y
    names(Sample)[[length(Sample)]] <- strsplit(y$LOCUS, "\\s")[[1]][1]
    message(paste("Sample", k, "in", length(SampleS), "done", sep = " "))
  }
  return(Sample)
}
