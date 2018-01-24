# 2016/01, 2017/01, 2016/06のデータを対象外とする
# 特徴量ではないことに注意
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)


# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# date --------------------------------------------------------------------
data.train %<>% 
  dplyr::mutate(visit_month = month(visit_date)) %>% 
  dplyr::mutate(isExcludeDate = visit_month %>% is_in(c(1, 6)) %>% as.numeric) %>% 
  dplyr::select(-visit_month)

data.test %<>% 
  dplyr::mutate(isExcludeDate = 0)
  
# チェック --------------------------------------------------------------------
# 1, 6月では1, それ以外は0かいなか
flg <- data.train %>% 
  dplyr::mutate(m = month(visit_date)) %>% 
  dplyr::count(m, isExcludeDate) %>% 
  nrow()
if (flg != 12) {
  stop("error")
}

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./805_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./805_test.csv", sep = ",", na = "", row.names = F) 
}


