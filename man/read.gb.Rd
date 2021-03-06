% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.gb.R
\name{read.gb}
\alias{read.gb}
\title{This function opens files with .gb extention from the NCBI/GenBank Nucleotide website and returns a list containing shaped record(s).
Records with > 200'000 bp may experience long processing times, especially if they have numerous FEATURES items}
\usage{
read.gb(File, DNA = TRUE, Type = "full")
}
\arguments{
\item{File}{character. The name of the file which the data are to be read from. It can contains several records}

\item{DNA}{logical. If TRUE, the DNA sequence in the ORIGIN part will be merged in one character string. If FALSE, the default layout will be kept. Default if TRUE}

\item{Type}{character. Should the output contain FEATURES and REFERENCE parts ? Possible values are "full" for a full record, "nofeat" to ignore FEATURES part, "noref" to ignore REFERENCE part and "nfnr" to ignore both parts. Default is "full"}
}
\value{
a table containing the data
}
\description{
This function opens files with .gb extention from the NCBI/GenBank Nucleotide website and returns a list containing shaped record(s).
Records with > 200'000 bp may experience long processing times, especially if they have numerous FEATURES items
}
\examples{
\dontrun{
read.gb(File = "sequence.gb", DNA = TRUE, Type = "full")
}

}
