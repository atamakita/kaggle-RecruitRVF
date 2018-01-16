# 休日を編集
# 1. 土日の祝日は非祝日扱い
# 2. 祝前日は祝日扱い
# 3. holidayは祝日と祝前日の2通りのみ
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)
library(dplyr)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(date = "date_info.csv")
lst.data <- lst.path %>% purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# 日付の修正 --------------------------------------------------------------------
# 1. 土日の祝日は非祝日扱い
df.date <- lst.data[["date"]] %>% 
  dplyr::mutate(holiday = ifelse(!(day_of_week %in% c("Saturday", "Sunday")) & (holiday_flg == 1), 
                                 "holiday", "weekday"))

# 2. 祝前日は祝日扱い
df.date %<>% dplyr::mutate(holiday_1 = lead(holiday, 1)) %>% 
  dplyr::mutate(holiday = ifelse(holiday_1 == "holiday", "preholiday", holiday))
  
# 1. 土日の祝日は非祝日扱い
df.date %<>% 
  dplyr::mutate(holiday = ifelse((day_of_week %in% c("Saturday", "Sunday")) & (holiday != "weekday"), 
                                 "weekday", holiday))

# 保存する
write.table(df.date, file = "./001_date_info_revised.csv", sep = ",", na = "", row.names = F)

df.date %<>% dplyr::select(calendar_date, holiday) %>% 
  dplyr::rename(visit_date = calendar_date)

data.train %<>% dplyr::left_join(., df.date, by = c("visit_date"))
data.test %<>% dplyr::left_join(., df.date, by = c("visit_date")) %>% 
  tidyr::replace_na(list(holiday = "weekday"))
  

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
f3 <- nrow(data.test) - nrow(na.omit(data.test))
f4 <- nrow(data.train) - nrow(na.omit(data.train))
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./602_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./602_test.csv", sep = ",", na = "", row.names = F) 
}


