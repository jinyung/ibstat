# wrappers

run_ibsummary <- function() {
  dir <- choose.dir()
  filelist <- list.files(dir, pattern = '.csv', full.names = TRUE)
  savedir <- file.path(dir, 'ibsummary_results')
  dir.create(savedir)
  for(i in filelist) {
    dat <- readib_zh(i)
    write.csv(ibsummary(dat), file.path(savedir, paste0('ibsummary_', basename(i))), 
              row.names = FALSE)
    write.csv(ibsummary(dat, timerange = c('00:00', '06:00')), 
              file.path(savedir, paste0('ibsummary_0000_0600_', basename(i))), 
              row.names = FALSE)
    write.csv(ibsummary(dat, timerange = c('06:00', '12:00')), 
              file.path(savedir, paste0('ibsummary_0600_1200_', basename(i))), 
              row.names = FALSE)
    write.csv(ibsummary(dat, timerange = c('12:00', '18:00')), 
              file.path(savedir, paste0('ibsummary_1200_1800_', basename(i))), 
              row.names = FALSE)
    write.csv(ibsummary(dat, timerange = c('18:00', '24:00')), 
              file.path(savedir, paste0('ibsummary_1800_2400_', basename(i))), 
              row.names = FALSE)
  }
}

run_ibex <- function(temp_cut, duration_cut, type = c('min', 'max')) {
  dir <- choose.dir()
  filelist <- list.files(dir, pattern = '.csv', full.names = TRUE)
  savedir <- file.path(dir, 'ibex_results')
  dir.create(savedir)
  type <- match.arg(type)
  for(i in filelist) {
    dat <- readib_zh(i)
    result <- ibex(dat, temp_cut, duration_cut, int = 1, type = type)
    write.csv(result$event_date_and_interval, 
              file.path(savedir, 
                        paste0('ibex_event_date_and_interval_temp_', 
                               temp_cut, '_duration_', 
                               duration_cut, '_', type, '_', basename(i))), 
              row.names = FALSE)
    write.csv(result$event_freq_by_month, 
              file.path(savedir, 
                        paste0('ibex_event_freq_by_month_temp_', 
                               temp_cut, '_duration_', 
                               duration_cut, '_', type, '_', basename(i))), 
              row.names = FALSE)
  }
}