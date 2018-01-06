# air_vst関連の特徴量
# 対象日の月、曜日、日、整数値
# このスクリプト単体で動作する

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(# air_rsv = "air_reserve.csv", 
                 # air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv", 
                 # hpg_rsv = "hpg_reserve.csv", 
                 # hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 # stores  = "store_id_relation.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- lst.data[["air_vst"]]
data.test <- lst.data[["submit"]] %>% 
  tidyr::separate(col = id, into = c("air_store_id", "visit_date"), sep = "_2017", remove = F) %>%
  dplyr::mutate(visit_date = paste0("2017", visit_date) %>% ymd)

# air_vst --------------------------------------------------------------------
## visit_date関連
# 月, 日, 曜日, 内部整数値
data.train %<>% 
  dplyr::mutate(month_vst = month(visit_date), 
                day_vst   = day(visit_date), 
                wday_vst  = wday(visit_date, label = T, abbr = F), 
                int_vst   = visit_date %>% as.numeric)
data.test %<>% 
  dplyr::mutate(month_vst = month(visit_date), 
                day_vst   = day(visit_date), 
                wday_vst  = wday(visit_date, label = T, abbr = F), 
                int_vst   = visit_date %>% as.numeric)

data.test %<>% dplyr::select(-id)

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./101_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./101_test.csv", sep = ",", na = "", row.names = F)
