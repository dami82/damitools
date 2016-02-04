revCompl <- function(DNAsequence, rev = TRUE, compl = TRUE) {
  if (is.character(DNAsequence)){
    resultVect <- sapply(DNAsequence, (function(seqString){
      mySeq <- toupper(seqString)
      if (rev == TRUE) {
        mySeq <- paste(sapply(1:nchar(mySeq), (function(i){
          pos <- nchar(mySeq) + 1 - i
          substr(mySeq, pos, pos)
        })), collapse = "")
      }
      if (compl == TRUE) {
        mySeq <- paste(sapply(1:nchar(mySeq), (function(i){
          aBase <- substr(mySeq, i, i)
          complBase <- c("A","T","C","G","N", "A")
          names(complBase) <- c("T","A","G","C","N","U")
          returnBase <- complBase[aBase]
          if(is.na(returnBase)){ stop("Bad input")}
          returnBase
        })), collapse = "")
      }
      return(mySeq)
    }))
    names(resultVect) <- NULL
    return(resultVect)
  } else {
    warning("Bad input")
  }
}


mySequences <- c("AGGTAAC", "UUGUCCAUA", "CCCANNNNTGGG")
revCompl(mySequences)
