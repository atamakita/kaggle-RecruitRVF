# air_vst関連の特徴量
# 来店人数中央値が一定の水準以上か否か
# 102_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# 101_の結果
temp <- list(train = "./102_train.csv", test = "./102_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x), col_types = "cDddddddddddddddd"))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train
data.test <- temp$test

# air_vst --------------------------------------------------------------------
# 全来店人数が多いか少ないか
crit.visitors <- data.train %$% median(med_vst_all) # 17
data.train %<>% dplyr::mutate(islargeStore = if_else(med_vst_all >= crit.visitors, 1, 0)) %>% 
  dplyr::select(air_store_id, visit_date, islargeStore)
data.test  %<>% dplyr::mutate(islargeStore = if_else(med_vst_all >= crit.visitors, 1, 0)) %>% 
  dplyr::select(air_store_id, visit_date, islargeStore)

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./103_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./103_test.csv", sep = ",", na = "", row.names = F)
