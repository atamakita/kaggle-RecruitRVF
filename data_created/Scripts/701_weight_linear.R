# サンプルウェイト
# 線形ウェイト(昔ほど小さい)
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
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date, int_vst)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date, int_vst)

# weight --------------------------------------------------------------------
min_vst_int <- data.train %$% min(int_vst) %>% add(-1)
max_vst_int <- data.train %$% max(int_vst)

data.train %<>% dplyr::mutate(weight = (int_vst - min_vst_int) / (max_vst_int - min_vst_int)) %>% 
  dplyr::select(-int_vst)

data.test %<>% dplyr::mutate(weight = 1) %>% 
  dplyr::select(-int_vst)

# チェック --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total Nsample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./701_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./701_test.csv", sep = ",", na = "", row.names = F)



