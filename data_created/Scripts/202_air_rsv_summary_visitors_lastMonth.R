# air_rsv関連の特徴量
# 先月の最大小値、平均値、中央値、サンプル数
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

CUTOFF_ALL <- 10 # 予約件数がこれ以下のサンプルは削除

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
# 全期間の平均値, 中央値, 件数
df.air_rsv <- lst.data[["air_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_air_rsv_all = mean(visitors, na.rm=T), 
                   med_air_rsv_all = median(visitors, na.rm=T), 
                   cnt_air_rsv_all = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(cnt_air_rsv_all >= CUTOFF_ALL)

data.train %<>% dplyr::left_join(., df.air_rsv, by = "air_store_id")
data.test %<>% dplyr::left_join(., df.air_rsv, by = "air_store_id")

# 1ヶ月間の最大値, 最小値, 平均値, 中央値
# たとえば、5月のレコードには4月の集計値が結びつく
df.air_rsv <- lst.data[["air_rsv"]] %>% 
  rbind(tibble::data_frame(air_store_id = "air_decoy",
                           visit_datetime = ymd_hms("2015-12-31 19:00:00"), 
                           reserve_datetime = ymd_hms("2015-12-31 19:00:00"), 
                           reserve_visitors = as.integer(0)), .) %>%
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::mutate(year = year(visit_date), month = month(visit_date)) %>% 
  dplyr::group_by(air_store_id, year, month) %>% 
  dplyr::summarise(max_air_rsv_lastmonth = max(reserve_visitors, na.rm=T), 
                   min_air_rsv_lastmonth = min(reserve_visitors, na.rm=T),
                   mean_air_rsv_lastmonth = mean(reserve_visitors, na.rm=T), 
                   med_air_rsv_lastmonth = median(reserve_visitors, na.rm=T), 
                   cnt_air_rsv_lastmonth = length(reserve_visitors)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(air_store_id, year, month) %>% 
  dplyr::filter(!((year == 2017) & (month %in% c(5:12)))) %>% 
  dplyr::filter(!((year == 2015) & (month %in% c(1:11)))) %>% 
  dplyr::filter(air_store_id != "air_decoy") %>% 
  dplyr::group_by(air_store_id) %>% 
  tidyr::fill(max_air_rsv_lastmonth, min_air_rsv_lastmonth, mean_air_rsv_lastmonth, 
              med_air_rsv_lastmonth, cnt_air_rsv_lastmonth) %>% 
  tidyr::fill(max_air_rsv_lastmonth, min_air_rsv_lastmonth, mean_air_rsv_lastmonth, 
              med_air_rsv_lastmonth, cnt_air_rsv_lastmonth, .direction = "up") %>% 
  dplyr::ungroup()

# マージ用のキーを作成
df.air_rsv %<>% 
  dplyr::mutate(key = paste0(year, "-", month, "-01") %>% ymd) %>% 
  dplyr::mutate(key = key + months(1)) %>%
  dplyr::select(-year, -month)


data.train %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.air_rsv, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key, -wday_vst, -month_vst)

data.test %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.air_rsv, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key, -wday_vst, -month_vst)


# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
f3 <- nrow(data.test) - nrow(na.omit(data.test))
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./202_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./202_test.csv", sep = ",", na = "", row.names = F) 
}


