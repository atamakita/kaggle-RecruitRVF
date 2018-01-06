# hpg_rsv関連の特徴量
# 全期間と月、曜日ごとの最大小値、平均値、中央値、サンプル数
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(hpg_rsv = "hpg_reserve.csv", 
                 stores  = "store_id_relation.csv")
lst.data <- lst.path %>% purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)


# hpg_rsv --------------------------------------------------------------------
df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(hpg_store_id, visit_date) %>% 
  dplyr::summarise(reserve_visitors_hpg = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(month_hpg_rsv = month(visit_date), 
                day_hpg_rsv   = day(visit_date), 
                wday_hpg_rsv  = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::group_by(hpg_store_id) %>% 
  dplyr::mutate(max_hpg_rsv_all = max(reserve_visitors_hpg, na.rm=T), 
                min_hpg_rsv_all = min(reserve_visitors_hpg, na.rm=T),
                mean_hpg_rsv_all = mean(reserve_visitors_hpg, na.rm=T), 
                med_hpg_rsv_all = median(reserve_visitors_hpg, na.rm=T), 
                cnt_hpg_rsv_all = length(reserve_visitors_hpg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hpg_store_id, wday_hpg_rsv) %>% 
  dplyr::mutate(max_hpg_rsv_wday = max(reserve_visitors_hpg, na.rm=T), 
                min_hpg_rsv_wday = min(reserve_visitors_hpg, na.rm=T),
                mean_hpg_rsv_wday = mean(reserve_visitors_hpg, na.rm=T), 
                med_hpg_rsv_wday = median(reserve_visitors_hpg, na.rm=T), 
                cnt_hpg_rsv_wday = length(reserve_visitors_hpg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hpg_store_id, month_hpg_rsv) %>% 
  dplyr::mutate(max_hpg_rsv_month = max(reserve_visitors_hpg, na.rm=T), 
                min_hpg_rsv_month = min(reserve_visitors_hpg, na.rm=T),
                mean_hpg_rsv_month = mean(reserve_visitors_hpg, na.rm=T), 
                med_hpg_rsv_month = median(reserve_visitors_hpg, na.rm=T), 
                cnt_hpg_rsv_month = length(reserve_visitors_hpg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(wday_hpg_rsv = as.character(wday_hpg_rsv)) %>%
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id")

# 全期間
# hpg_rsvが存在しない先はNA
merge_hpg_rsv_all <- function(data, df.hpg_rsv) {
  
  data <- df.hpg_rsv %>% 
    dplyr::distinct(air_store_id, .keep_all = T) %>% 
    dplyr::select(air_store_id, reserve_visitors_hpg, 
                  max_hpg_rsv_all, min_hpg_rsv_all, mean_hpg_rsv_all, med_hpg_rsv_all, cnt_hpg_rsv_all) %>% 
    dplyr::left_join(data, ., by = "air_store_id") %>% 
    dplyr::mutate(isNoRecord_hpg_rsv = dplyr::if_else(is.na(reserve_visitors_hpg), T, F) %>% as.numeric)
  
  return(data)
}
data.train %<>% merge_hpg_rsv_all(df.hpg_rsv)
data.test %<>% merge_hpg_rsv_all(df.hpg_rsv)

# 曜日別
merge_hpg_rsv_wday <- function(data, df.hpg_rsv) {
  
  temp <- df.hpg_rsv %>% 
    dplyr::distinct(air_store_id, wday_hpg_rsv, .keep_all= T) %>% 
    dplyr::select(air_store_id, wday_hpg_rsv, 
                  max_hpg_rsv_wday, min_hpg_rsv_wday, mean_hpg_rsv_wday, med_hpg_rsv_wday, cnt_hpg_rsv_wday) %>% 
    purrr::map_at(c("max_hpg_rsv_wday", "min_hpg_rsv_wday", "mean_hpg_rsv_wday", 
                    "med_hpg_rsv_wday", "cnt_hpg_rsv_wday"), 
                  ~ as.double(.x)) %>% 
    dplyr::bind_cols() %>% 
    dplyr::distinct(air_store_id, wday_hpg_rsv, .keep_all = T) %>% 
    tidyr::complete(air_store_id, wday_hpg_rsv) %>% 
    tidyr::replace_na(list(max_hpg_rsv_wday = 0, min_hpg_rsv_wday = 0, 
                           mean_hpg_rsv_wday = 0, med_hpg_rsv_wday = 0, cnt_hpg_rsv_wday = 0))
  
  data %<>% dplyr::left_join(., temp, by = c("air_store_id", "wday_vst" = "wday_hpg_rsv"))
  
  return(data)
}
data.train %<>% merge_hpg_rsv_wday(df.hpg_rsv)
data.test %<>% merge_hpg_rsv_wday(df.hpg_rsv)

# 月別
merge_hpg_rsv_month <- function(data, df.hpg_rsv) {
  
  temp <- df.hpg_rsv %>% 
    dplyr::distinct(air_store_id, month_hpg_rsv, .keep_all= T) %>% 
    dplyr::select(air_store_id, month_hpg_rsv, 
                  max_hpg_rsv_month, min_hpg_rsv_month, mean_hpg_rsv_month, med_hpg_rsv_month, cnt_hpg_rsv_month) %>%
    purrr::map_at(c("max_hpg_rsv_month", "min_hpg_rsv_month", "mean_hpg_rsv_month", 
                    "med_hpg_rsv_month", "cnt_hpg_rsv_month"), 
                  ~ as.double(.x)) %>% 
    dplyr::bind_cols() %>% 
    dplyr::distinct(air_store_id, month_hpg_rsv, .keep_all = T) %>% 
    tidyr::complete(air_store_id, month_hpg_rsv) %>% 
    tidyr::replace_na(list(max_hpg_rsv_month = 0, min_hpg_rsv_month = 0, 
                           mean_hpg_rsv_month = 0, med_hpg_rsv_month = 0, cnt_hpg_rsv_month = 0))
  
  data %<>% dplyr::left_join(., temp, by = c("air_store_id", "month_vst" = "month_hpg_rsv"))
  
  return(data)
}
data.train %<>% merge_hpg_rsv_month(df.hpg_rsv)
data.test %<>% merge_hpg_rsv_month(df.hpg_rsv)

data.train %<>% dplyr::select(-wday_vst, -month_vst)
data.test %<>% dplyr::select(-wday_vst, -month_vst)

# チェック --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total sample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./401_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./401_test.csv", sep = ",", na = "", row.names = F)


