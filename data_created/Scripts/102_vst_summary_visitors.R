# air_vst関連の特徴量
# 全期間と月、曜日ごとの最大小値、平均値、中央値、サンプル数
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(# air_rsv = "air_reserve.csv", 
                 # air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv")#,
                 # hpg_rsv = "hpg_reserve.csv", 
                 # hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 # stores  = "store_id_relation.csv", 
                 # submit  = "sample_submission.csv")
lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train
data.test <- temp$test

# air_vst --------------------------------------------------------------------
## visitors関連
# 過去の全体及び曜日及び月別の最大値, 最小値, 平均値, 中央値
data.train %<>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(max_vst_all = max(visitors, na.rm=T), 
                min_vst_all = min(visitors, na.rm=T),
                mean_vst_all = mean(visitors, na.rm=T), 
                med_vst_all = median(visitors, na.rm=T), 
                cnt_vst_all = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id, wday_vst) %>% 
  dplyr::mutate(max_vst_wday = max(visitors, na.rm=T), 
                min_vst_wday = min(visitors, na.rm=T),
                mean_vst_wday = mean(visitors, na.rm=T), 
                med_vst_wday = median(visitors, na.rm=T), 
                cnt_vst_wday = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id, month_vst) %>% 
  dplyr::mutate(max_vst_month = max(visitors, na.rm=T), 
                min_vst_month = min(visitors, na.rm=T),
                mean_vst_month = mean(visitors, na.rm=T), 
                med_vst_month = median(visitors, na.rm=T), 
                cnt_vst_month = length(visitors)) %>% 
  dplyr::ungroup()


# 全期間
data.test <- data.train %>% 
  dplyr::select(air_store_id,  
                max_vst_all, min_vst_all, mean_vst_all, med_vst_all, cnt_vst_all) %>% 
  dplyr::distinct(air_store_id, .keep_all = T) %>% 
  dplyr::left_join(data.test, ., by = c("air_store_id"))

# 曜日別
# NAの場合は各店舗の曜日別の値の中央値で置換する
df.id_wday <- data.train %>% 
  dplyr::select(air_store_id, wday_vst, 
                max_vst_wday, min_vst_wday, mean_vst_wday, med_vst_wday, cnt_vst_wday) %>% 
  purrr::map_at(c("max_vst_wday", "min_vst_wday", "mean_vst_wday", "med_vst_wday", "cnt_vst_wday"), 
                ~ as.double(.x)) %>% 
  dplyr::bind_cols() %>% 
  dplyr::distinct(air_store_id, wday_vst, .keep_all = T) %>% 
  tidyr::complete(air_store_id, wday_vst) %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(max_vst_wday  = if_else(is.na(max_vst_wday),  median(max_vst_wday, na.rm=T), max_vst_wday), 
                min_vst_wday  = if_else(is.na(min_vst_wday),  median(min_vst_wday, na.rm=T), min_vst_wday), 
                mean_vst_wday = if_else(is.na(mean_vst_wday), median(mean_vst_wday, na.rm=T), mean_vst_wday), 
                med_vst_wday  = if_else(is.na(med_vst_wday),  median(med_vst_wday, na.rm=T), med_vst_wday), 
                cnt_vst_wday  = if_else(is.na(cnt_vst_wday),  median(cnt_vst_wday, na.rm=T), cnt_vst_wday)) %>% 
  dplyr::ungroup()

data.test %<>% dplyr::left_join(., df.id_wday, by = c("air_store_id", "wday_vst"))


# 月別
# NAの場合は各店舗の月別の値の中央値で置換する
df.id_month <- data.train %>% 
  dplyr::select(air_store_id, month_vst, 
                max_vst_month, min_vst_month, mean_vst_month, med_vst_month, cnt_vst_month) %>% 
  purrr::map_at(c("max_vst_month", "min_vst_month", "mean_vst_month", "med_vst_month", "cnt_vst_month"), 
                ~ as.double(.x)) %>% 
  dplyr::bind_cols() %>% 
  dplyr::distinct(air_store_id, month_vst, .keep_all = T) %>% 
  tidyr::complete(air_store_id, month_vst) %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(max_vst_month  = if_else(is.na(max_vst_month), median(max_vst_month, na.rm=T), max_vst_month), 
                min_vst_month  = if_else(is.na(min_vst_month), median(min_vst_month, na.rm=T), min_vst_month), 
                mean_vst_month = if_else(is.na(mean_vst_month), median(mean_vst_month, na.rm=T), mean_vst_month), 
                med_vst_month  = if_else(is.na(med_vst_month), median(med_vst_month, na.rm=T), med_vst_month), 
                cnt_vst_month  = if_else(is.na(cnt_vst_month), median(cnt_vst_month, na.rm=T), cnt_vst_month)) %>% 
  dplyr::ungroup()

data.test %<>% dplyr::left_join(., df.id_month, by = c("air_store_id", "month_vst"))


data.train %<>% dplyr::select(-visitors, -month_vst, -day_vst, -wday_vst, -int_vst)
data.test %<>% dplyr::select(-visitors, -month_vst, -day_vst, -wday_vst, -int_vst)

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./102_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./102_test.csv", sep = ",", na = "", row.names = F)
