library(tidyverse)
create_data <- function(N){
  d1 <- tibble(
    ID = 1:N,
    Arm = sample(c('Treatment', 'SOC'), N, replace=TRUE),
    Age = rnorm(N, 60, 5),
    Gender = sample(c('Male','Female'), N, replace=T, prob = c(.3, .7)),
    Years_of_followup = round(rnorm(N, 12, 2),1),
    Vital_status = sample(c('Dead','Alive'), N, replace=TRUE, prob=c(0.2, 0.8))
  )
  return(d1)
}

set.seed(1035)

dummy_data <- map(c(10, 16, 27), create_data)

outdir <- here::here('slides/lectures/week2/children')
rio::export(dummy_data, file.path(outdir,'study_data.xlsx'))

for(i in 1:3){
  outfile <- file.path(outdir, paste0('study',i,'.csv'))
  write_csv(dummy_data[[i]], outfile)
}
