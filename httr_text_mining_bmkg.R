library(rvest)
library(dplyr)
library(lubridate)

url <- 'https://www.bmkg.go.id/gempabumi/gempabumi-terkini.bmkg'

out_df %>% mutate(`Waktu Gempa` =dmy_hms(`Waktu Gempa`)) %>% filter(as.Date(`Waktu Gempa`) == Sys.Date())
head(out_df)
write.csv(out_df, 'current_earthquake_data.csv', row.names = FALSE)

