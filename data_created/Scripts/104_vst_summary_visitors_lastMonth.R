# air_vst関連の特徴量
# 先月および週別の最大小値、平均値、中央値、サンプル数
# 欠損値は先月の値を使用し、先月の値が無い場合は来月の値を使用する
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)
library(RcppRoll)
library(glue)

WS <- 5 # Window Size(roll_maxの幅)
SS <- 6 # Spacer Size(lagの幅)

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

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# air_vst --------------------------------------------------------------------
## visitors関連
# 全期間の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
df.air_vst <- lst.data[["air_vst"]] %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_vst_all = mean(visitors, na.rm=T), 
                   med_vst_all = median(visitors, na.rm=T), 
                   p05_vst_all = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_vst_all = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_vst_all = log(1 + visitors) %>% sd(na.rm = T), 
                   cnt_vst_all = length(visitors)) %>% 
  dplyr::ungroup()

if (F) {
  df.air_vst %>% tidyr::gather(key = nm, value = value, 
                               mean_vst_all, med_vst_all, p05_vst_all, p95_vst_all) %>% 
    ggplot(., aes(x = value, y = ..density.., colour = nm)) + 
    geom_histogram(bins = 100) + 
    facet_wrap(~ nm, ncol = 2)
  
  ggplot(df.air_vst, aes(x = sd_log_vst_all, y = ..density..)) + 
    geom_histogram(bins = 100, col = "blue")
}

data.train %<>% dplyr::left_join(., df.air_vst, by = "air_store_id")
data.test %<>% dplyr::left_join(., df.air_vst, by = "air_store_id")

data.train %>% dim
data.test %>% dim

# 1ヶ月間の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
# 使用上は、たとえば5月のレコードには4月の集計値が結びつく
df.air_vst <- lst.data[["air_vst"]] %>% 
  rbind(tibble::data_frame(air_store_id = "air_decoy", 
                           visit_date = ymd("2015-12-31"), 
                           visitors = 0), .) %>% 
  dplyr::mutate(year = year(visit_date), month = month(visit_date)) %>% 
  dplyr::group_by(air_store_id, year, month) %>% 
  dplyr::summarise(mean_vst_lastmonth = mean(visitors, na.rm=T), 
                   med_vst_lastmonth = median(visitors, na.rm=T), 
                   p05_vst_lastmonth = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_vst_lastmonth = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_vst_lastmonth = log(1 + visitors) %>% sd(na.rm = T), 
                   cnt_vst_lastmonth = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(air_store_id, year, month) %>% 
  dplyr::filter(!((year == 2017) & (month %in% c(5:12)))) %>% 
  dplyr::filter(!((year == 2015) & (month %in% c(1:11)))) %>% 
  dplyr::filter(air_store_id != "air_decoy") %>% 
  dplyr::group_by(air_store_id) %>% 
  tidyr::fill(mean_vst_lastmonth, med_vst_lastmonth, cnt_vst_lastmonth, 
              p05_vst_lastmonth, p95_vst_lastmonth, sd_log_vst_lastmonth, .direction = "up") %>% 
  tidyr::fill(mean_vst_lastmonth, med_vst_lastmonth, cnt_vst_lastmonth, 
              p05_vst_lastmonth, p95_vst_lastmonth, sd_log_vst_lastmonth) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cnt_vst_lastmonth = ifelse((year == 2017) & (month == 4), 
                                           yes = cnt_vst_lastmonth * 30 / 22, 
                                           no = as.double(cnt_vst_lastmonth))) # 4月は22日までなので補正

# マージ用のキーを作成
df.air_vst %<>% 
  dplyr::mutate(key = paste0(year, "-", month, "-01") %>% ymd) %>% 
  dplyr::mutate(key = key + months(1)) %>%
  dplyr::select(-year, -month)


data.train %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key)

data.test %<>% dplyr::mutate(key = floor_date(visit_date, "month")) %>% 
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "key")) %>% 
  dplyr::select(-key)

data.train %>% dim
data.test %>% dim


# 曜日別の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
# 標準偏差はサンプル数的に怪しいかもしれない
df.air_vst <- lst.data[["air_vst"]] %>% 
  dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::group_by(air_store_id, wday) %>%
  dplyr::summarise(mean_vst_wday = mean(visitors, na.rm=T), 
                   med_vst_wday = median(visitors, na.rm=T), 
                   p05_vst_wday = quantile(visitors, p = 0.05, na.rm = T) %>% as.vector, 
                   p95_vst_wday = quantile(visitors, p = 0.95, na.rm = T) %>% as.vector, 
                   sd_log_vst_wday = log(1 + visitors) %>% sd(na.rm = T), 
                   cnt_vst_wday = length(visitors)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(air_store_id, wday) %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(mean_vst_wday = mean_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                med_vst_wday = med_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                p05_vst_wday = p05_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                p95_vst_wday = p95_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                sd_log_vst_wday = sd_log_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cnt_vst_wday = cnt_vst_wday %>% ifelse(is.na(.), yes = median(., na.rm = T), no = .)) %>% 
  dplyr::ungroup()
 
if (F) {
  df.air_vst %>% tidyr::gather(key = nm, value = value, 
                               mean_vst_wday, med_vst_wday, p05_vst_wday, p95_vst_wday) %>% 
    ggplot(., aes(x = value, y = ..density.., colour = nm)) + 
    geom_histogram(bins = 100) + 
    facet_wrap(~ nm, ncol = 2)
  
  df.air_vst %>% 
  
  ggplot(df.air_vst, aes(x = sd_log_vst_wday, y = ..density..)) + 
    geom_histogram(bins = 100, col = "blue")
}

data.train %<>% dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "wday")) %>% 
  dplyr::select(-wday)
data.test %<>% dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "wday")) %>% 
  dplyr::select(-wday)

# testの曜日別にはNAが存在するので補正する
data.test %<>% dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(mean_vst_wdays)

data.train %>% dim
data.test %>% dim

# 若干怪しいので凍結
if (F) {
  # 6週間前から5曜日分の集計値を計算する
  # testのNAは全期間の曜日別集計値で埋めている(議論の余地はある)
  df.submit <- lst.data[["submit"]] %>% 
    tidyr::separate(col = "id", into = c("air_store_id", "visit_date"), sep = "_2017") %>% 
    dplyr::mutate(visit_date = paste0("2017", visit_date) %>% ymd)
  
  df.air_vst <- lst.data[["air_vst"]] %>%
    rbind(., df.submit) %>% 
    dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
    dplyr::group_by(air_store_id, wday) %>%
    dplyr::mutate(max_vst_lastwdays =  roll_maxr(visitors, n = WS, fill = NA, na.rm=T),
                  min_vst_lastwdays =  roll_minr(visitors, n = WS, fill = NA, na.rm=T),
                  mean_vst_lastwdays = roll_meanr(visitors, n = WS, fill = NA, na.rm=T),
                  med_vst_lastwdays =  roll_medianr(visitors, n = WS, fill = NA, na.rm=T)) %>% 
    dplyr::mutate(max_vst_lastwdays =  lag(max_vst_lastwdays, n = SS), 
                  min_vst_lastwdays =  lag(min_vst_lastwdays, n = SS), 
                  mean_vst_lastwdays = lag(mean_vst_lastwdays, n = SS), 
                  med_vst_lastwdays =  lag(med_vst_lastwdays, n = SS)) %>% 
    dplyr::ungroup()
  
  data.train <- df.air_vst %>% 
    dplyr::select(-visitors, -wday) %>% 
    dplyr::left_join(data.train, ., by = c("air_store_id", "visit_date"))
  
  data.test <- df.air_vst %>% 
    dplyr::mutate(max_vst_lastwdays =  if_else(is.na(max_vst_lastwdays), max(visitors), max_vst_lastwdays), 
                  min_vst_lastwdays =  if_else(is.na(min_vst_lastwdays), min(visitors), min_vst_lastwdays), 
                  mean_vst_lastwdays = if_else(is.na(mean_vst_lastwdays), mean(visitors), mean_vst_lastwdays), 
                  med_vst_lastwdays =  if_else(is.na(med_vst_lastwdays), 
                                               as.double(median(visitors)), med_vst_lastwdays)) %>% 
    dplyr::select(-visitors, -wday) %>% 
    dplyr::left_join(data.test, ., by = c("air_store_id", "visit_date"))
  
}

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
f3 <- nrow(data.test) - nrow(na.omit(data.test))
f4 <- nrow(data.train) - nrow(na.omit(data.train))
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else if (f3 != 0) {
  stop("detect NA in test data") 
} else if (f4 != 0) {
  stop("detect NA in train data") 
} else {
  write.table(data.train, file = "./104_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./104_test.csv", sep = ",", na = "", row.names = F)
}
