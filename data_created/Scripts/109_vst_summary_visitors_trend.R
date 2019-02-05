# air_vst関連の特徴量
# 先月および週別の最大小値、平均値、中央値、サンプル数
# 最大小値と平均値と中央値はtrendでわってlogをとる
# 欠損値は先月の値を使用し、先月の値が無い場合は来月の値を使用する
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)
library(RcppRoll)
library(glue)
library(zoo)



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

# 803の結果
df.803 <- list(train = "./803_train.csv", test = "./803_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# air_vst --------------------------------------------------------------------
## 前準備
# 各店舗の観測初日と最終日を習得
df.term_ids <- lst.data[["air_vst"]] %>% 
  dplyr::group_by(air_store_id) %>%
  dplyr::summarise(firstdate = min(visit_date), 
                   lastdate = ymd("2017-05-31")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(daylength = (lastdate - firstdate) %>% as.numeric)

# 各店舗についてfirstdateの365日前からダミー列を作る
makedummydf <- function(x) {
  id <- x$air_store_id[1]
  first <- x$firstdate[1]
  len <- x$daylength[1]
  
  ret <- tibble::data_frame(air_store_id = id, 
                            visit_date = first + days(-365:len))
  return(ret)
}

df.master_df <- df.term_ids %>% 
  split(., .$air_store_id) %>% 
  purrr::map(~ makedummydf(.x)) %>% 
  purrr::reduce(rbind) %>% 
  dplyr::left_join(., lst.data[["air_vst"]], by = c("air_store_id", "visit_date")) %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(visitors_yesterday = visitors %>% dplyr::lag(n = 1), 
                visitors_lastweek  = visitors %>% dplyr::lag(n = 7)) %>% 
  dplyr::ungroup()

## visitors関連
# 過去365日間の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
df.air_vst <- 
  df.master_df %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_365 = roll_meanr(x = visitors_yesterday, n = 365, na.rm = T), 
                cummed_vst_365 = rollapplyr(visitors_yesterday, width = 365, 
                                            FUN = "quantile", p = 0.5, na.rm = T) %>% 
                  append(rep(NA, 364), .),
                cump05_vst_365 = rollapplyr(visitors_yesterday, width = 365, 
                                            FUN = "quantile", p = 0.05, na.rm = T) %>% 
                  append(rep(NA, 364), .),
                cump95_vst_365 = rollapplyr(visitors_yesterday, width = 365, 
                                            FUN = "quantile", p = 0.95, na.rm = T) %>% 
                  append(rep(NA, 364), .),
                cumsd_log_vst_365 = visitors_yesterday %>% log1p %>% roll_sdr(n = 365, na.rm = T), 
                cumcnt_vst_365 = is.na(visitors_yesterday) %>% not %>% 
                  rollapplyr(., width = 365, FUN = "sum", na.rm = T) %>% 
                  append(rep(NA, 364), .)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_365 = cummean_vst_365 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cummed_vst_365 = cummed_vst_365 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump05_vst_365 = cump05_vst_365 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump95_vst_365 = cump95_vst_365 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumsd_log_vst_365 = cumsd_log_vst_365 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumcnt_vst_365 = cumcnt_vst_365 %>% ifelse(is.na(.), yes = -1, no = .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-visitors, -visitors_yesterday, -visitors_lastweek)

if (F) {
  df.air_vst %>% tidyr::gather(key = nm, value = value, 
                               mean_vst_all, med_vst_all, p05_vst_all, p95_vst_all) %>% 
    ggplot(., aes(x = value, y = ..density.., colour = nm)) + 
    geom_histogram(bins = 100) + 
    facet_wrap(~ nm, ncol = 2)
  
  ggplot(df.air_vst, aes(x = sd_log_vst_all, y = ..density..)) + 
    geom_histogram(bins = 100, col = "blue")
}

data.train %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))
data.test %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))

data.train %>% dim
data.test %>% dim

# 過去60日間の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
df.air_vst <- 
  df.master_df %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_60 = roll_meanr(x = visitors_yesterday, n = 60, na.rm = T), 
                cummed_vst_60 = rollapplyr(visitors_yesterday, width = 60, 
                                            FUN = "quantile", p = 0.5, na.rm = T) %>% 
                  append(rep(NA, 59), .),
                cump05_vst_60 = rollapplyr(visitors_yesterday, width = 60, 
                                            FUN = "quantile", p = 0.05, na.rm = T) %>% 
                  append(rep(NA, 59), .),
                cump95_vst_60 = rollapplyr(visitors_yesterday, width = 60, 
                                            FUN = "quantile", p = 0.95, na.rm = T) %>% 
                  append(rep(NA, 59), .),
                cumsd_log_vst_60 = visitors_yesterday %>% log1p %>% roll_sdr(n = 60, na.rm = T), 
                cumcnt_vst_60 = is.na(visitors_yesterday) %>% not %>% 
                  rollapplyr(., width = 60, FUN = "sum", na.rm = T) %>% 
                  append(rep(NA, 59), .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_60 = cummean_vst_60 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cummed_vst_60 = cummed_vst_60 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump05_vst_60 = cump05_vst_60 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump95_vst_60 = cump95_vst_60 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumsd_log_vst_60 = cumsd_log_vst_60 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumcnt_vst_60 = cumcnt_vst_60 %>% ifelse(is.na(.), yes = -1, no = .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-visitors, -visitors_yesterday, -visitors_lastweek)

data.train %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))
data.test %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))

data.train %>% dim
data.test %>% dim


# 直前半年間(24週)の曜日別の平均値, 中央値, 件数, 25%tile, 75%tile, 標準偏差(log1p %>% sd)
# 標準偏差はサンプル数的に怪しいかもしれない
df.air_vst <- 
  df.master_df %>% 
  dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  # dplyr::filter(air_store_id == "air_00a91d42b08b08d9") %>% 
  dplyr::group_by(air_store_id, wday) %>%
  dplyr::mutate(cummean_vst_wday24 = roll_meanr(x = visitors_lastweek, n = 24, na.rm = T), 
                cummed_vst_wday24 = rollapplyr(visitors_lastweek, width = 24, 
                                           FUN = "quantile", p = 0.5, na.rm = T) %>% 
                  append(rep(NA, 23), .),
                cump05_vst_wday24 = rollapplyr(visitors_lastweek, width = 24, 
                                           FUN = "quantile", p = 0.05, na.rm = T) %>% 
                  append(rep(NA, 23), .),
                cump95_vst_wday24 = rollapplyr(visitors_lastweek, width = 24, 
                                           FUN = "quantile", p = 0.95, na.rm = T) %>% 
                  append(rep(NA, 23), .),
                cumsd_log_vst_wday24 = visitors_lastweek %>% log1p %>% roll_sdr(n = 24, na.rm = T), 
                cumcnt_vst_wday24 = is.na(visitors_lastweek) %>% not %>% 
                  rollapplyr(., width = 24, FUN = "sum", na.rm = T) %>% 
                  append(rep(NA, 23), .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_wday24 = cummean_vst_wday24 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cummed_vst_wday24 = cummed_vst_wday24 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump05_vst_wday24 = cump05_vst_wday24 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cump95_vst_wday24 = cump95_vst_wday24 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumsd_log_vst_wday24 = cumsd_log_vst_wday24 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumcnt_vst_wday24 = cumcnt_vst_wday24 %>% ifelse(is.na(.), yes = -1, no = .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-visitors, -visitors_yesterday, -visitors_lastweek)
 
data.train %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))
data.test %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))

data.train %>% dim
data.test %>% dim
  
data.train %<>% dplyr::select(-wday)
data.test %<>% dplyr::select(-wday)



# 直前3ヶ月(12週)の曜日別の平均値, 中央値, 件数
df.air_vst <- 
  df.master_df %>% 
  dplyr::mutate(wday = wday(visit_date, label = T, abbr = F)) %>%
  # dplyr::filter(air_store_id == "air_00a91d42b08b08d9") %>% 
  dplyr::group_by(air_store_id, wday) %>%
  dplyr::mutate(cummean_vst_wday12 = roll_meanr(x = visitors_lastweek, n = 12, na.rm = T), 
                cummed_vst_wday12 = rollapplyr(visitors_lastweek, width = 12, 
                                               FUN = "quantile", p = 0.5, na.rm = T) %>% 
                  append(rep(NA, 11), .),
                cumcnt_vst_wday12 = is.na(visitors_lastweek) %>% not %>% 
                  rollapplyr(., width = 12, FUN = "sum", na.rm = T) %>% 
                  append(rep(NA, 11), .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(cummean_vst_wday12 = cummean_vst_wday12 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cummed_vst_wday12 = cummed_vst_wday12 %>% 
                  ifelse(is.na(.), yes = median(., na.rm = T), no = .), 
                cumcnt_vst_wday12 = cumcnt_vst_wday12 %>% ifelse(is.na(.), yes = -1, no = .)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-visitors, -visitors_yesterday, -visitors_lastweek)

data.train %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))
data.test %<>% dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date"))

data.train %>% dim
data.test %>% dim

data.train %<>% dplyr::select(-wday)
data.test %<>% dplyr::select(-wday)


# trendをマージして統計値をtrendで割る
translate_visitors2 <- function(x, f.trend) {
  ret <- log((1 + x) / f.trend)
  return(ret)
}

data.train %<>% 
  dplyr::left_join(., df.803[["train"]], by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(cmean_logtr_vst_365 = translate_visitors2(cummean_vst_365, trend), 
                cmed_logtr_vst_365 =  translate_visitors2(cummed_vst_365, trend), 
                cp05_logtr_vst_365 =  translate_visitors2(cump05_vst_365, trend), 
                cp95_logtr_vst_365 =  translate_visitors2(cump95_vst_365, trend), 
                cmean_logtr_vst_60 = translate_visitors2(cummean_vst_60, trend), 
                cmed_logtr_vst_60 =  translate_visitors2(cummed_vst_60, trend), 
                cp05_logtr_vst_60 =  translate_visitors2(cump05_vst_60, trend), 
                cp95_logtr_vst_60 =  translate_visitors2(cump95_vst_60, trend), 
                cmean_logtr_vst_wday24 = translate_visitors2(cummean_vst_wday24, trend), 
                cmed_logtr_vst_wday24 =  translate_visitors2(cummed_vst_wday24, trend), 
                cp05_logtr_vst_wday24 =  translate_visitors2(cump05_vst_wday24, trend), 
                cp95_logtr_vst_wday24 =  translate_visitors2(cump95_vst_wday24, trend), 
                cmean_logtr_vst_wday12 = translate_visitors2(cummean_vst_wday12, trend), 
                cmed_logtr_vst_wday12 =  translate_visitors2(cummed_vst_wday12, trend)) %>% 
  dplyr::select(-cummean_vst_365, -cummed_vst_365, -cump05_vst_365, -cump95_vst_365, 
                -cummean_vst_60, -cummed_vst_60, -cump05_vst_60, -cump95_vst_60, 
                -cummean_vst_wday24, -cummed_vst_wday24, -cump05_vst_wday24, -cump95_vst_wday24, 
                -cummean_vst_wday12, -cummed_vst_wday12, 
                -trend, -trend_log)

data.test %<>% 
  dplyr::left_join(., df.803[["test"]], by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(cmean_logtr_vst_365 = translate_visitors2(cummean_vst_365, trend), 
                cmed_logtr_vst_365 =  translate_visitors2(cummed_vst_365, trend), 
                cp05_logtr_vst_365 =  translate_visitors2(cump05_vst_365, trend), 
                cp95_logtr_vst_365 =  translate_visitors2(cump95_vst_365, trend), 
                cmean_logtr_vst_60 = translate_visitors2(cummean_vst_60, trend), 
                cmed_logtr_vst_60 =  translate_visitors2(cummed_vst_60, trend), 
                cp05_logtr_vst_60 =  translate_visitors2(cump05_vst_60, trend), 
                cp95_logtr_vst_60 =  translate_visitors2(cump95_vst_60, trend), 
                cmean_logtr_vst_wday24 = translate_visitors2(cummean_vst_wday24, trend), 
                cmed_logtr_vst_wday24 =  translate_visitors2(cummed_vst_wday24, trend), 
                cp05_logtr_vst_wday24 =  translate_visitors2(cump05_vst_wday24, trend), 
                cp95_logtr_vst_wday24 =  translate_visitors2(cump95_vst_wday24, trend), 
                cmean_logtr_vst_wday12 = translate_visitors2(cummean_vst_wday12, trend), 
                cmed_logtr_vst_wday12 =  translate_visitors2(cummed_vst_wday12, trend)) %>% 
  dplyr::select(-cummean_vst_365, -cummed_vst_365, -cump05_vst_365, -cump95_vst_365, 
                -cummean_vst_60, -cummed_vst_60, -cump05_vst_60, -cump95_vst_60, 
                -cummean_vst_wday24, -cummed_vst_wday24, -cump05_vst_wday24, -cump95_vst_wday24, 
                -cummean_vst_wday12, -cummed_vst_wday12, 
                -trend, -trend_log)

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
  write.table(data.train, file = "./109_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./109_test.csv", sep = ",", na = "", row.names = F)
}
