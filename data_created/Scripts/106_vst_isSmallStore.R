# air_vst関連の特徴量
# 来店人数中央値が5人以下か否か
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

CRIT_VISITORS <- 5

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_vst = "air_visit_data.csv",
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
# 全来店人数が多いか少ないか
df.air_vst <- lst.data[["air_vst"]] %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarize(isSmallStore = if_else(median(visitors) <= CRIT_VISITORS, 1, 0)) %>% 
  dplyr::ungroup()

data.train %<>% dplyr::left_join(., df.air_vst, by = "air_store_id")
data.test %<>% dplyr::left_join(., df.air_vst, by = "air_store_id")

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./106_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./106_test.csv", sep = ",", na = "", row.names = F)
