# air_rsv関連の特徴量
# 全期間と月、曜日ごとの最大小値、平均値、中央値、サンプル数
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_rsv = "air_reserve.csv")
lst.data <- lst.path %>% purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)

# air_rsv --------------------------------------------------------------------
df.air_rsv <- lst.data[["air_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(reserve_visitors_air = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(month_air_rsv = month(visit_date), 
                day_air_rsv   = day(visit_date), 
                wday_air_rsv  = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(max_air_rsv_all = max(reserve_visitors_air, na.rm=T), 
                min_air_rsv_all = min(reserve_visitors_air, na.rm=T),
                mean_air_rsv_all = mean(reserve_visitors_air, na.rm=T), 
                med_air_rsv_all = median(reserve_visitors_air, na.rm=T), 
                cnt_air_rsv_all = length(reserve_visitors_air)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id, wday_air_rsv) %>% 
  dplyr::mutate(max_air_rsv_wday = max(reserve_visitors_air, na.rm=T), 
                min_air_rsv_wday = min(reserve_visitors_air, na.rm=T),
                mean_air_rsv_wday = mean(reserve_visitors_air, na.rm=T), 
                med_air_rsv_wday = median(reserve_visitors_air, na.rm=T), 
                cnt_air_rsv_wday = length(reserve_visitors_air)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id, month_air_rsv) %>% 
  dplyr::mutate(max_air_rsv_month = max(reserve_visitors_air, na.rm=T), 
                min_air_rsv_month = min(reserve_visitors_air, na.rm=T),
                mean_air_rsv_month = mean(reserve_visitors_air, na.rm=T), 
                med_air_rsv_month = median(reserve_visitors_air, na.rm=T), 
                cnt_air_rsv_month = length(reserve_visitors_air)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(wday_air_rsv = as.character(wday_air_rsv))

# 全期間
# air_rsvが存在しない先はNA
merge_air_rsv_all <- function(data, df.air_rsv) {
  
  data <- df.air_rsv %>% 
    dplyr::distinct(air_store_id, .keep_all = T) %>% 
    dplyr::select(air_store_id, reserve_visitors_air, 
                  max_air_rsv_all, min_air_rsv_all, mean_air_rsv_all, med_air_rsv_all, cnt_air_rsv_all) %>% 
    dplyr::left_join(data, ., by = "air_store_id") %>% 
    dplyr::mutate(isNoRecord_air_rsv = dplyr::if_else(is.na(reserve_visitors_air), T, F) %>% as.numeric)
  
  return(data)
}
data.train %<>% merge_air_rsv_all(df.air_rsv)
data.test %<>% merge_air_rsv_all(df.air_rsv)

# 曜日別
merge_air_rsv_wday <- function(data, df.air_rsv) {
  
  temp <- df.air_rsv %>% 
    dplyr::distinct(air_store_id, wday_air_rsv, .keep_all= T) %>% 
    dplyr::select(air_store_id, wday_air_rsv, 
                  max_air_rsv_wday, min_air_rsv_wday, mean_air_rsv_wday, med_air_rsv_wday, cnt_air_rsv_wday) %>% 
    purrr::map_at(c("max_air_rsv_wday", "min_air_rsv_wday", "mean_air_rsv_wday", 
                    "med_air_rsv_wday", "cnt_air_rsv_wday"), 
                  ~ as.double(.x)) %>% 
    dplyr::bind_cols() %>% 
    dplyr::distinct(air_store_id, wday_air_rsv, .keep_all = T) %>% 
    tidyr::complete(air_store_id, wday_air_rsv) %>% 
    tidyr::replace_na(list(max_air_rsv_wday = 0, min_air_rsv_wday = 0, 
                           mean_air_rsv_wday = 0, med_air_rsv_wday = 0, cnt_air_rsv_wday = 0))
  
  data %<>% dplyr::left_join(., temp, by = c("air_store_id", "wday_vst" = "wday_air_rsv"))
  
  return(data)
}
data.train %<>% merge_air_rsv_wday(df.air_rsv)
data.test %<>% merge_air_rsv_wday(df.air_rsv)

# 月別
merge_air_rsv_month <- function(data, df.air_rsv) {
  
  temp <- df.air_rsv %>% 
    dplyr::distinct(air_store_id, month_air_rsv, .keep_all= T) %>% 
    dplyr::select(air_store_id, month_air_rsv, 
                  max_air_rsv_month, min_air_rsv_month, mean_air_rsv_month, med_air_rsv_month, cnt_air_rsv_month) %>%
    purrr::map_at(c("max_air_rsv_month", "min_air_rsv_month", "mean_air_rsv_month", 
                    "med_air_rsv_month", "cnt_air_rsv_month"), 
                  ~ as.double(.x)) %>% 
    dplyr::bind_cols() %>% 
    dplyr::distinct(air_store_id, month_air_rsv, .keep_all = T) %>% 
    tidyr::complete(air_store_id, month_air_rsv) %>% 
    tidyr::replace_na(list(max_air_rsv_month = 0, min_air_rsv_month = 0, 
                           mean_air_rsv_month = 0, med_air_rsv_month = 0, cnt_air_rsv_month = 0))
  
  data %<>% dplyr::left_join(., temp, by = c("air_store_id", "month_vst" = "month_air_rsv"))
  
  return(data)
}
data.train %<>% merge_air_rsv_month(df.air_rsv)
data.test %<>% merge_air_rsv_month(df.air_rsv)

data.train %<>% dplyr::select(-wday_vst, -month_vst)
data.test %<>% dplyr::select(-wday_vst, -month_vst)

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./201_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./201_test.csv", sep = ",", na = "", row.names = F)


