# hpg_rsv関連の特徴量
# 先月の最大小値、平均値、中央値、サンプル数
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

CUTOFF_ALL <- 10 # 予約件数がこれ以下のサンプルは削除

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
# 全期間の平均値, 中央値, 件数
df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::filter(!is.na(air_store_id)) %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_hpg_rsv_all = mean(visitors, na.rm=T), 
                   med_hpg_rsv_all = median(visitors, na.rm=T), 
                   p05_hpg_rsv_all = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_hpg_rsv_all = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_hpg_rsv_all = log(1 + visitors) %>% sd(na.rm = T), 
                   cnt_hpg_rsv_all = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(cnt_hpg_rsv_all >= CUTOFF_ALL)

data.train %<>% dplyr::left_join(., df.hpg_rsv, by = "air_store_id")
data.test %<>% dplyr::left_join(., df.hpg_rsv, by = "air_store_id")

# 1ヶ月間の最大値, 最小値, 平均値, 中央値
# たとえば、5月のレコードには4月の集計値が結びつく
df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::filter(!is.na(air_store_id)) %>% 
  rbind(tibble::data_frame(hpg_store_id = "hpg_decoy",
                           visit_datetime = ymd_hms("2015-12-31 19:00:00"), 
                           reserve_datetime = ymd_hms("2015-12-31 19:00:00"), 
                           reserve_visitors = as.integer(0), 
                           air_store_id = "air_decoy"), .) %>%
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = year(visit_date), month = month(visit_date)) %>% 
  dplyr::group_by(air_store_id, year, month) %>% 
  dplyr::summarise(mean_hpg_rsv_lastmonth = mean(visitors, na.rm=T), 
                   med_hpg_rsv_lastmonth = median(visitors, na.rm=T), 
                   p05_hpg_rsv_lastmonth = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_hpg_rsv_lastmonth = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_hpg_rsv_lastmonth = log(1 + visitors) %>% sd(na.rm = T),
                   cnt_hpg_rsv_lastmonth = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(air_store_id, year, month) %>% 
  dplyr::filter(!((year == 2017) & (month %in% c(5:12)))) %>% 
  dplyr::filter(!((year == 2015) & (month %in% c(1:11)))) %>% 
  dplyr::filter(air_store_id != "air_decoy") %>% 
  dplyr::group_by(air_store_id) %>% 
  tidyr::fill(mean_hpg_rsv_lastmonth, med_hpg_rsv_lastmonth, cnt_hpg_rsv_lastmonth, 
              p05_hpg_rsv_lastmonth, p95_hpg_rsv_lastmonth, sd_log_hpg_rsv_lastmonth, .direction = "up") %>% 
  tidyr::fill(mean_hpg_rsv_lastmonth, med_hpg_rsv_lastmonth, cnt_hpg_rsv_lastmonth, 
              p05_hpg_rsv_lastmonth, p95_hpg_rsv_lastmonth, sd_log_hpg_rsv_lastmonth) %>% 
  dplyr::ungroup()


# マージ用のキーを作成
df.hpg_rsv %<>% 
  dplyr::mutate(key = paste0(year, "-", month, "-01") %>% ymd) %>% 
  dplyr::mutate(key = key + months(1)) %>%
  dplyr::select(-year, -month)

data.train %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.hpg_rsv, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key, -wday_vst, -month_vst)

data.test %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.hpg_rsv, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key, -wday_vst, -month_vst)


# 曜日別の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
# 標準偏差はサンプル数的に怪しいかもしれない
df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::filter(!is.na(air_store_id)) %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::group_by(air_store_id, wday) %>%
  dplyr::summarise(mean_hpg_rsv_wday = mean(visitors, na.rm=T), 
                   med_hpg_rsv_wday = median(visitors, na.rm=T), 
                   p05_hpg_rsv_wday = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_hpg_rsv_wday = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_hpg_rsv_wday = log(1 + visitors) %>% sd(na.rm = T), 
                   cnt_hpg_rsv_wday = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(air_store_id, wday) %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(mean_hpg_rsv_wday = mean_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                med_hpg_rsv_wday = med_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                p05_hpg_rsv_wday = p05_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                p95_hpg_rsv_wday = p95_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                sd_log_hpg_rsv_wday = sd_log_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cnt_hpg_rsv_wday = cnt_hpg_rsv_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .)) %>% 
  dplyr::ungroup()

data.train %<>% dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::left_join(., df.hpg_rsv, by = c("air_store_id", "wday")) %>% 
  dplyr::select(-wday)
data.test %<>% dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::left_join(., df.hpg_rsv, by = c("air_store_id", "wday")) %>% 
  dplyr::select(-wday)


# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./402_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./402_test.csv", sep = ",", na = "", row.names = F) 
}
