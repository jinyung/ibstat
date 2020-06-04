# Take data read by readib/ readib_zh and generate daily/ monthly stats
# timerange should be in c("H:M:S", "H:M:S") format, with the first as lower 
# bound and second as higher bound, should be in 24 hour format, as the data are 
# converted into 24 hour format already

ibsummary <- function(dat, type = c('day', 'month'), timerange) {
  
  # split the date time column
  date_time_ls <- strsplit(as.character(dat$`Date/Time`), ' ')
  datec <- sapply(date_time_ls, `[`, 1)
  ampm <- sapply(date_time_ls, `[`, 2)
  time <- sapply(date_time_ls, `[`, 3)
  # convert time into 24 hour format
  time <- format(strptime(paste(time, ampm), "%I:%M:%S %p"), "%H:%M:%S")
  
  # filter time range if given
  if (!missing(timerange)) {
    subsetidx <- which(time >= timerange[1] & time < timerange[2])
    datec <- datec[subsetidx]
    dat <- dat[subsetidx, ] 
  }
  
  # stats
  temp <- dat$Value
  day <- format(as.Date(datec), '%Y/%m/%d')
  month <- format(as.Date(datec), '%Y/%m')
  type <- match.arg(type)
  if (type == 'day') {
    bytype <- day
    lydate <- unique(day)
  } else {
    bytype <- month
    lydate <- unique(month)
  }
  lymean <- c(by(temp, bytype, mean))
  lymax <- c(by(temp, bytype, max))
  lymin <- c(by(temp, bytype, min))

  # output
  lyresult <- data.frame(Date = lydate, min = lymin, max = lymax, mean = lymean)
  rownames(lyresult) <- NULL
  lyresult
}