# 予約情報(air, hpg)が存在しているか否か
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_rsv = "air_reserve.csv", 
                 hpg_rsv = "hpg_reserve.csv", 
                 stores  = "store_id_relation.csv")
lst.data <- lst.path %>% purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# air_rsv --------------------------------------------------------------------
df.air_rsv <- lst.data[["air_rsv"]]

df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::filter(!is.na(air_store_id))
  
id.air <- df.air_rsv$air_store_id %>% unique
id.hpg <- df.hpg_rsv$air_store_id %>% unique

data.train %<>% 
  dplyr::mutate(isNoRecord_air_rsv = if_else((air_store_id %in% id.air), 0, 1), 
                isNoRecord_hpg_rsv = if_else((air_store_id %in% id.hpg), 0, 1))
  
data.test %<>% 
  dplyr::mutate(isNoRecord_air_rsv = if_else((air_store_id %in% id.air), 0, 1), 
                isNoRecord_hpg_rsv = if_else((air_store_id %in% id.hpg), 0, 1))

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
f3 <- nrow(data.test) - nrow(na.omit(data.test))
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else if (f3 != 0) {
  stop("detect NA in test data") 
} else {
  write.table(data.train, file = "./802_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./802_test.csv", sep = ",", na = "", row.names = F) 
}


