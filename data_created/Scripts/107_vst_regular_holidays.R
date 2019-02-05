# air_vst関連の特徴量
# 店舗ごとの定休日とvisitorsの関連性を探る
# 残念ながら定休日のリークはなさそう
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_vst = "air_visit_data.csv")#,
                 # hpg_rsv = "hpg_reserve.csv", 
                 # hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 # stores  = "store_id_relation.csv", 
                 # submit  = "sample_submission.csv")
lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(-int_vst)
data.test <- temp$test %>% dplyr::select(-int_vst)

# air_vst --------------------------------------------------------------------
if (F) {
  ids <- data.train$air_store_id %>% unique
  index <- 3
  data.train %>% 
    dplyr::filter(air_store_id == ids[index]) %>% 
    # dplyr::filter(visit_date > ymd("2017-01-01")) %>% 
    dplyr::count(wday_vst)
  
  data.train %>% 
    dplyr::filter(air_store_id == ids[index]) %>% 
    # dplyr::filter(visit_date > ymd("2017-01-01")) %>% 
    split(., .$wday_vst) %>% 
    purrr::map(~ tail(.x, 10))
  
  data.train %>% 
    dplyr::filter(air_store_id == ids[index]) %$%
    hist(log1p(visitors), breaks = 30)
}

