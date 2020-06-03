# read the temperatures into a dataframe, skipping the metadata
# skip by detecting "Date/Time" header
readib <- function(filename) {
  opencon <- file(filename, open = "rt")
  x <- TRUE
  while (x) {
    x <- !grepl("Date/Time", readLines(opencon, n = 1))
  }
  dat <- read.table(opencon) #insert additional parameters to read.table
  close(opencon)
  colnames(dat) <- c('Date/Time', 'Unit', 'Value')
  dat
}

# same as readib, but read the file encoded in big5 (traditional chinese 
# character), change the chinese character into latin, specifically "上午" > AM
# and "下午" > PM, and convert it into UTF-8
readib_zh <- function(filename) {
  tempdat <- readLines(filename, encoding="big5")
  tempdat <- iconv(tempdat, "big5", "utf8")
  # skip the metadata
  startl <- grep("Date/Time", tempdat) 
  header <- tempdat[startl]
  tempdat <- tempdat[-c(1:startl)]
  # replace the chinese characters into latin
  tempdat <- gsub("上午", 'AM', tempdat) 
  tempdat <- gsub("下午", 'PM', tempdat) 
  # save readlines object into temp csv file and read back into data frame
  temp_file <- tempfile()
  write(tempdat, temp_file, sep = ',')
  rm(tempdat)
  dat <- read.csv(temp_file, header = FALSE)
  colnames(dat) <- c('Date/Time', 'Unit', 'Value')
  dat
}
