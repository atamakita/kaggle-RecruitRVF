# date_info関連の特徴量
# 休日の情報
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")


# 読込 --------------------------------------------------------------------
lst.data <- list()
lst.data[["date"]] <- readr::read_csv(file = "./001_date_info_grouped.csv", 
                                      locale = readr::locale(encoding = "UTF-8"))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# date_info --------------------------------------------------------------------
df.date_info <- lst.data[["date"]] %>% 
  dplyr::mutate(ds = calendar_date, 
                isholiday = if_else(is.na(holiday), 0, 1)) %>% 
  tidyr::replace_na(list(holiday = "weekday")) %>% 
  dplyr::select(ds, isholiday, holiday)

data.train %<>% dplyr::left_join(., df.date_info, by = c("visit_date" = "ds"))
data.test %<>% dplyr::left_join(., df.date_info, by = c("visit_date" = "ds"))

# チェック --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total Nsample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./601_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./601_test.csv", sep = ",", na = "", row.names = F)



