# XGBoostのための特徴量作り

library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)

# ---------------------------------------------------------------- ディレクトリ
setwd("/Users/shunsukeakita/Park/RecruitRVF/submit/12_171229_02/")

# ---------------------------------------------------------------- 函数
# source("./002_estimateHPGSeasonal.R", encoding = "UTF-8")
# source("./003_estimateAIRvst.R", encoding = "UTF-8")

# ---------------------------------------------------------------- 読込
# ファイル名
lst.path <- list(air_rsv = "air_reserve.csv", 
                 air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv", 
                 hpg_rsv = "hpg_reserve.csv", 
                 hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 stores  = "store_id_relation.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../../data_org/", x)))
lst.data[["date"]] <- readr::read_csv(file = "../../data_created/001_date_info_grouped.csv", 
                                      locale = readr::locale(encoding = "UTF-8"))

# 外でつくった時系列頻度などのサマリー
lst.data[["summary"]] <- readr::read_csv("../../data_created/n_record_info.csv")
id.rsv <- lst.data[["summary"]]

# hpg_rsvの予約者数の時系列から求めた季節変動成分(手でグループ化したholidayが入っているので注意)
# HPGresult <- lst.data %$% 
#   getHPGSeasonalComponent(f.hpgrsv = hpg_rsv, f.holidays = date, f.params = NULL, isCV = T)
# lst.data[["hpg_seasonal"]] <- HPGresult$result

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(!is.na(holiday)) %>% 
  dplyr::select(ds = calendar_date, holiday = holiday)

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
  #tidyr::replace_na(list(max_vst_all = -1, min_vst_all = -1, mean_vst_all = -1, 
  #                       med_vst_all = -1, cnt_vst_all = -1)) %>% 
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


# 全来店人数が多いか少ないか
crit.visitors <- data.train %$% median(med_vst_all) # 17
data.train %<>% dplyr::mutate(islargeStore = if_else(med_vst_all >= crit.visitors, 1, 0))
data.test  %<>% dplyr::mutate(islargeStore = if_else(med_vst_all >= crit.visitors, 1, 0))

# air_str --------------------------------------------------------------------
data.train <- lst.data[["air_str"]] %>% 
  tidyr::separate(air_area_name, c("air_area_ken_name"), sep = " ", extra = "drop") %>% 
  dplyr::rename(latitude_air = latitude, longitude_air = longitude) %>% 
  dplyr::left_join(data.train, ., by = c("air_store_id"))

data.test <- lst.data[["air_str"]] %>% 
  tidyr::separate(air_area_name, c("air_area_ken_name"), sep = " ", extra = "drop") %>% 
  dplyr::rename(latitude_air = latitude, longitude_air = longitude) %>% 
  dplyr::left_join(data.test, ., by = c("air_store_id"))

# New Features from kaggle
# https://www.kaggle.com/tunguz/surprise-me-2/code
data.train %<>% 
  dplyr::mutate(maxdiff_lat_air = max(latitude_air, na.rm = T) - latitude_air, 
                maxdiff_lon_air = max(longitude_air, na.rm = T) - longitude_air, 
                sum_lat_lon_air = longitude_air + latitude_air)

data.test %<>% 
  dplyr::mutate(maxdiff_lat_air = max(latitude_air, na.rm = T) - latitude_air, 
                maxdiff_lon_air = max(longitude_air, na.rm = T) - longitude_air, 
                sum_lat_lon_air = longitude_air + latitude_air)

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
  dplyr::ungroup()

# 全期間
# air_rsvが存在しない先はNA
merge_air_rsv_all <- function(data, df.air_rsv) {
  
  # temp <- df.air_rsv %>% 
  #   dplyr::distinct(air_store_id, .keep_all = T) %>% 
  #   dplyr::select(air_store_id, reserve_visitors, 
  #                 max_rsv_all, min_rsv_all, mean_rsv_all, med_rsv_all, cnt_rsv_all) %>% 
  #   purrr::map_at(c("max_vst_all", "min_vst_all", "mean_vst_all", "med_vst_all", "cnt_vst_all"), 
  #                 ~ as.double(.x)) %>% 
  #   dplyr::bind_cols() %>% 
  #   dplyr::distinct(air_store_id, .keep_all = T) %>% 
  #   dplyr::group_by(air_store_id) %>% 
  #   dplyr::mutate(max_vst_all  = if_else(is.na(max_vst_all),  0, max_vst_all), 
  #                 min_vst_all  = if_else(is.na(min_vst_all),  0, min_vst_all), 
  #                 mean_vst_all = if_else(is.na(mean_vst_all), 0, mean_vst_all), 
  #                 med_vst_all  = if_else(is.na(med_vst_all),  0, med_vst_all), 
  #                 cnt_vst_all  = if_else(is.na(cnt_vst_all),  0, cnt_vst_all)) %>% 
  #   dplyr::ungroup()

  data <- df.air_rsv %>% 
    dplyr::distinct(air_store_id, .keep_all = T) %>% 
    dplyr::select(air_store_id, reserve_visitors_air, 
                  max_air_rsv_all, min_air_rsv_all, mean_air_rsv_all, med_air_rsv_all, cnt_air_rsv_all) %>% 
    dplyr::left_join(data, ., by = "air_store_id") %>% 
    dplyr::mutate(isNoRecord_air_rsv = dplyr::if_else(is.na(reserve_visitors_air), T, F) %>% as.numeric)
    # tidyr::replace_na(list(reserve_visitors = -1,  max_rsv_all = -1, min_rsv_all = -1, 
    #                        mean_rsv_all = -1, med_rsv_all = -1, cnt_rsv_all = -1))
  
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

backup <- list(train = data.train, test = data.test)
data.train <- backup$train
data.test <- backup$test

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

# hpg_str --------------------------------------------------------------------
data.train <- lst.data[["hpg_str"]] %>% 
  tidyr::separate(hpg_area_name, c("hpg_area_ken_name"), sep = " ", extra = "drop") %>% 
  dplyr::rename(latitude_hpg = latitude, longitude_hpg = longitude) %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::left_join(data.train, ., by = c("air_store_id"))

data.test <- lst.data[["hpg_str"]] %>% 
  tidyr::separate(hpg_area_name, c("hpg_area_ken_name"), sep = " ", extra = "drop") %>% 
  dplyr::rename(latitude_hpg = latitude, longitude_hpg = longitude) %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>%
  dplyr::left_join(data.test, ., by = c("air_store_id"))

# New Features from kaggle
# https://www.kaggle.com/tunguz/surprise-me-2/code
data.train %<>% 
  dplyr::mutate(maxdiff_lat_hpg = max(latitude_hpg, na.rm = T) - latitude_hpg, 
                maxdiff_lon_hpg = max(longitude_hpg, na.rm = T) - longitude_hpg, 
                sum_lat_lon_hpg = longitude_hpg + latitude_hpg)

data.test %<>% 
  dplyr::mutate(maxdiff_lat_hpg = max(latitude_hpg, na.rm = T) - latitude_hpg, 
                maxdiff_lon_hpg = max(longitude_hpg, na.rm = T) - longitude_hpg, 
                sum_lat_lon_hpg = longitude_hpg + latitude_hpg)

# date --------------------------------------------------------------------
data.train %<>% 
  dplyr::left_join(., holidays, by = c("visit_date" = "ds")) %>%
  tidyr::replace_na(list(holiday = "weekday")) %>% 
  dplyr::mutate(isholiday = if_else(holiday == "weekday", 0, 1))

data.test %<>% 
  dplyr::left_join(., holidays, by = c("visit_date" = "ds")) %>%
  tidyr::replace_na(list(holiday = "weekday")) %>% 
  dplyr::mutate(isholiday = if_else(holiday == "weekday", 0, 1))

# チェック
data.train %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .)
data.test %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .)

# 出力 --------------------------------------------------------------------
# trainとtestデータ
write.table(data.train, file = "./train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./test.csv", sep = ",", na = "", row.names = F)

colnames(data.test) %in% colnames(data.train)

# モデル別に使用する列名(の雛形)
if (F) {
  df.coltype <- data.test %>% 
    purrr::map(~ class(.x)) %>% 
    do.call("rbind", .) %>% 
    data.frame %>% 
    tibble::rownames_to_column() %>% 
    set_colnames(c("colnm", "type", "a")) %>% 
    dplyr::select(-a) %>% 
    dplyr::mutate(submit = NA, 
                  model_vst = NA, 
                  model_vst_air = NA, 
                  model_vst_hpg = NA, 
                  model_vst_air_hpg = NA)
  write.table(df.coltype, file = "./column_list.csv", sep = ",", na = "", row.names = F)  
}


# おまけ --------------------------------------------------------------------
# library(Rtsne)
# 
# usecols <- data.train %>% 
#   purrr::map(~ class(.x)) %>% 
#   do.call("rbind", .) %>% 
#   data.frame %>% 
#   tibble::rownames_to_column() %>% 
#   set_colnames(c("var", "a", "b")) %>% 
#   dplyr::filter(a %in% c("numeric", "integer")) %>% .$var
# 
# res <- Rtsne(data.train[,usecols])
# 
# res.tsne <- res$Y %>% tibble::as_data_frame()
# res.tsne <- data.train %>% 
#   dplyr::select(air_store_id, visit_date, month_vst, wday_vst, 
#                 air_genre_name, air_area_ken_name, holiday, isNoRecord_rsv) %>% 
#   purrr::map_dfr(~ parse_character(.x)) %>% 
#   cbind(., res.tsne) %>% 
#   tibble::as_data_frame()
# 
# write.table(res.tsne, file = "./result_tsne_train.csv", sep = ",", na = "", row.names = F)
# 
# res.tsne %>% 
#   dplyr::sample_n(10000) %>% 
#   ggplot(., aes(x=V1, y=V2, colour = holiday)) + 
#   geom_point()




