\name{revCompl}
\alias{revCompl}
\title{
Reverse Complementary DNA or RNA Sequence
}
\description{
Sequence manipulation Tool for Molecular Biology. Retrieves the complementary, reverse or reverse-complementary sequence of the DNA or RNA sequence(s) passed as argument.
}
\usage{
revCompl(DNAsequence, rev = TRUE, compl = TRUE)
}
\arguments{
  \item{DNAsequence}{
characther string containing the DNA/RNA sequence(s) to be manipulated
}
  \item{rev}{
boolean. If TRUE, the reverse sequence is returned. Can be combined with compl = TRUE to return the reverse-complement of the sequence(s) passed in as argument
}
  \item{compl}{
boolean. If TRUE, the complementary sequence is returned. Can be combined with rev = TRUE to return the reverse-complement of the sequence(s) passed in as argument
}
}
\value{
character String including the reverse-complementary sequences.
}
\author{
Damiano Fantini <damiano.fantini@gmail.com>
}
\references{
https://github.com/dami82/damitools
}

\examples{
dnaSequences <- c("AAACCG","AACUUUGGG","CCAANNNNTTGG")
revCompl(dnaSequences)
}
