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

# Does the real work for converting weekly data to monthly
# Does no interpolation: If the observation for the week
# is in a particular month, it counts all of that observation only for
# that month, so a Jan 31 observation counts entirely for January.
# The data is required to be in FRED CSV format. If not, anything
# could happen. There are no checks in place for that case.
# Not necessarily for daily data, but when there are some days within
# a month (weekly, daily, etc.)
fred.to.monthly <- function(data.raw, fnc=mean) {
  dates <- as.Date(data.raw[,1], format="%Y-%m-%d")
  months <- format(dates, "%Y-%m")
  monthly.values <- as.numeric(by(data.raw[,2], months, fnc))
  s <- min(dates)
  s.year <- as.numeric(format(s, "%Y"))
  s.month <- as.numeric(format(s, "%m"))
  return(ts(monthly.values, frequency=12, start=c(s.year, s.month)))
}

# Private function to convert monthly to quarterly
# Doesn't do any checks
toq <- function(dta, fnc=mean) {
  year.start <- start(dta)[1]
  month.start <- start(dta)[2]
  quarter.start <- (month.start + 2) %/% 3
  qs <- tstools::yq(dta)
  quarterly.values <- as.numeric(by(as.numeric(dta), qs, fnc))
  return(ts(quarterly.values, frequency=4, start=c(year.start, quarter.start)))
}


# name: Name of the file downloaded from FRED
# Has to be default format CSV file, including header
# Returns ts object with correct frequency and dates
# Will load weekly data and aggregate to monthly or quarterly
# Will aggregate monthly to quarterly
# WARNING If using summation, you need to confirm the first and
# last months are full months
# Leaving weekly for now, but that will eventually change to daily
import.fred <- function(name, highfreq=FALSE, weekly=FALSE, agg="monthly", fnc=mean) {
  f <- file(name)
  lines <- readLines(f, n=3)
  close(f)
  
  # This is a better approach than what I used to have here, because
  # it can accommodate whitespace and throw errors if it's not an
  # appropriate date.
  date1.text <- strsplit(lines[2], ",")[[1]][1]
  date1 <- as.Date(date1.text, format="%Y-%m-%d")
  year1 <- as.numeric(format(date1, "%Y"))
  month1 <- as.numeric(format(date1, "%m"))
  
  date2.text <- strsplit(lines[3], ",")[[1]][1]
  date2 <- as.Date(date2.text, format="%Y-%m-%d")
  year2 <- as.numeric(format(date2, "%Y"))
  month2 <- as.numeric(format(date2, "%m"))
  
  #~   year1 <- as.integer(substr(lines[2], 1, 4))
  #~   month1 <- as.integer(substr(lines[2], 6, 7))
  #~   year2 <- as.integer(substr(lines[3], 1, 4))
  #~   month2 <- as.integer(substr(lines[3], 6, 7))
  data.raw <- read.csv(name, header=TRUE)
  # Legacy support for weekly
  if (weekly) {
    highfreq <- TRUE
  }
  
  if (highfreq) {
    result <- fred.to.monthly(data.raw, fnc)
    if (agg == "monthly") {
      return(result)
    } else if (agg == "quarterly") {
      # Slight efficiency opportunity: Add a quarterly conversion function
      # to convert.fred.weekly
      return(toq(result, fnc))
    } else {
      stop("In call to import.fred: argument 'agg' has to be 'monthly' or 'quarterly'. Use read.csv if you just want to load weekly data.")
    }
  }
  
  # If we get here, it's monthly, quarterly, or annual data
  if (year2 > year1) {
    if (month1 == 1) {
      # Has to be annual data
      return(ts(data.raw[,2], frequency=1, start=year1))
    } else if (month1 == 10) {
      # Has to be quarterly data
      return(ts(data.raw[,2], frequency=4, start=c(year1, (month1 + 2) %/% 3)))
    } else if (month1 == 12) {
      # Has to be monthly data
      result <- ts(data.raw[,2], frequency=12, start=c(year1, month1))
      if (agg == "quarterly") {
        return(toq(result, fnc))
      } else {
        return(result)
      }
    }
    # Can rule out annual data
  } else {
    if ((month2 - month1) == 1) {
      # Has to be monthly data
      result <- ts(data.raw[,2], frequency=12, start=c(year1, month1))
      if (agg == "quarterly") {
        return(toq(result, fnc))
      } else {
        return(result)
      }
    } else if ((month2 - month1) == 3) {
      # Has to be quarterly data
      return(ts(data.raw[,2], frequency=4, start=c(year1, (month1 + 2) %/% 3)))
    }
  }
}
