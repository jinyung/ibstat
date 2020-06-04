# get extreme temperature events at conditions of temperature > threshold
# and duration > threshold

ibex <- function(dat, temp_cut, duration_cut, int = 1, 
                   type = c('min', 'max')) {
  
  # processing
  date_time_ls <- strsplit(as.character(dat$`Date/Time`), ' ')
  datec <- sapply(date_time_ls, `[`, 1)
  day <- format(as.Date(datec), '%Y/%m/%d')
  temp <- dat$Value
  month <- format(as.Date(datec), '%Y/%m')
  
  # if negative, get something smaller, if positive, get something larger
  # cutfunc <- ifelse(temp_cut > 0, `>`, `<`)
  # extreme_event <- cutfunc(temp, temp_cut)
  type <- match.arg(type)
  if (type == 'min') {
    extreme_event <- temp <= temp_cut
  } else {
    extreme_event <- temp >= temp_cut
  }
  
  # how many times the extreme temp appear in a day, times the interval = total
  # hours appear in a day
  extreme_event_day_hour <- c(by(extreme_event, day, sum)) * int
  
  # second criteria = duration
  extreme_event_two_cutoff_freq <- extreme_event_day_hour >= duration_cut
  
  # frequency of extremem event match both cutoff criteria, summarized by month
  extreme_event_two_cutoff_freq_by_month <- c(by(extreme_event_two_cutoff_freq,
                                                 format(as.Date(unique(day)), '%Y/%m'), sum))
  
  # days in between two events
  extreme_event_idx <- which(extreme_event_two_cutoff_freq)
  day_diff_between_events <- diff(as.Date(unique(day)[extreme_event_idx]))
  
  # output
  extreme_event_result <- list(
    event_date_and_interval = data.frame(ex_Date = names(extreme_event_idx), 
                                         ex_Day_Interval = c(NA, day_diff_between_events)), 
    event_freq_by_month = data.frame(Month = unique(month), 
                                     Frequency = extreme_event_two_cutoff_freq_by_month))
  extreme_event_result                           
}