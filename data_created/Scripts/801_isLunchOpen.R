# ランチ営業を行なっているか否か
# 9:00 ~ 15:00に来店予約が入っていればランチ営業を行なっているとする
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
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date, wday_vst, month_vst)

# air_rsv --------------------------------------------------------------------
LUNCH_START <- 9
LUNCH_END <- 15

df.air_rsv <- lst.data[["air_rsv"]] %>% 
  dplyr::mutate(visit_hour = hour(visit_datetime)) %>% 
  dplyr::count(air_store_id, visit_hour) %>% 
  dplyr::filter((LUNCH_START <= visit_hour) & (visit_hour <= LUNCH_END))

df.hpg_rsv <- lst.data[["hpg_rsv"]] %>% 
  dplyr::mutate(visit_hour = hour(visit_datetime)) %>% 
  dplyr::count(hpg_store_id, visit_hour) %>% 
  dplyr::filter((LUNCH_START <= visit_hour) & (visit_hour <= LUNCH_END)) %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id") %>% 
  dplyr::filter(!is.na(air_store_id))
  
id.vec <- c(df.air_rsv$air_store_id, df.hpg_rsv$air_store_id) %>% unique


data.train %<>% 
  dplyr::mutate(islunch = if_else((air_store_id %in% id.vec), 1, 0)) %>% 
  dplyr::select(air_store_id, visit_date, islunch)
  
data.test %<>% 
  dplyr::mutate(islunch = if_else((air_store_id %in% id.vec), 1, 0)) %>% 
  dplyr::select(air_store_id, visit_date, islunch)
  

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./801_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./801_test.csv", sep = ",", na = "", row.names = F) 
}


