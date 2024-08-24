# Format like this
# frequency: [frequency]
# start: [start] {1979 1} {1980}
# Allows storage of metadata inside the file
import.tsformat <- function(fn, skip=0, header=FALSE, ...) {
  # Metadata is in the first two rows
  # frequency and start can be switched
  tmp <- readLines(fn, 3)
  meta <- tmp[1:2]
  maybeDoc <- tmp[3]
  hasDoc <- FALSE
  documentation <- ""
  linesToSkip <- skip+2
  docCheck <- regexpr("^#\\s*doc:", maybeDoc, perl=TRUE)
  if (docCheck[1] > 0) {
    hasDoc <- TRUE
    documentation <- substring(maybeDoc, attr(docCheck, "match.length")+1)
    linesToSkip <- linesToSkip + 1
  }
  
  # Get the frequency
  f <- grep("#\\s*frequency:(.*)", meta, value=TRUE)
  freq <- as.integer(strsplit(f, "#\\s*frequency:", perl=TRUE)[[1]][2])
  
  # Get the start date
  s <- grep("#\\s*start:(.*)", meta, value=TRUE)
  startDate <- if (freq == 1) {
    as.integer(strsplit(s, "#\\s*start:", perl=TRUE)[[1]][2])
  } else if ((freq == 4) | (freq == 12)) {
    tmp <- trimws(strsplit(s, "#\\s*start:", perl=TRUE)[[1]][2])
    tmp2 <- strsplit(tmp, "\\s+", perl=TRUE)
    c(as.integer(tmp2[[1]][1]), as.integer(tmp2[[1]][2]))
  } else {
    stop("import.tsformat currently only works with data frequency 1, 4, or 12")
  }
  
  if (hasDoc) {
    result <- ts(read.csv(fn, skip=linesToSkip, header=header, ...), start=startDate, frequency=freq)
    attr(result, "doc") <- trimws(documentation)
    return(result)
  } else {
    return(ts(read.csv(fn, skip=linesToSkip, header=header, ...), start=startDate, frequency=freq))
  }
}

