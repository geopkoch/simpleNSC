
simpleNSC.return <- function(path){

  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,readr,tidyr,lubridate) 
  
  df <- read.csv(path)
  df$Enrollment.Begin <- as.Date(parse_date_time(x = df$Enrollment.Begin, 
                          orders = "ymd",
                          locale = "eng"))
  df$Enrollment.End <- as.Date(parse_date_time(x = df$Enrollment.End, 
                                               orders = "ymd",
                                               locale = "eng"))
  df$Graduation.Date <- as.Date(parse_date_time(x = df$Graduation.Date, 
                                                orders = "ymd",
                                                locale = "eng"))
  df.flat <- select(df,6,9,10,12,14,15,18) %>%
    drop_na() %>% 
    group_by(Requester.Return.Field,College.Code.Branch,College.Name,X2.year...4.year,Enrollment.Major.1) %>%
    summarise(FirstEnrollment = min(Enrollment.Begin),
              LastEnrollment = max(Enrollment.End)) 
  df.grad <- df %>%
    select(6,9,22,23:25) %>%
    filter(Graduated. == 'Y')
  df.final <- left_join(df.flat,df.grad,by = c("Requester.Return.Field","College.Code.Branch"))
  return(df.final)
}
