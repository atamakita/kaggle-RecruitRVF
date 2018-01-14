# air_vstの外れ値を店舗ごとに求める
# 特徴量ではないことに注意
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)


CRITERION <- 0.99 # これ以上のパーセンタイル点は外れ値とする


# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_vst = "air_visit_data.csv")
lst.data <- lst.path %>% purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# air_vst --------------------------------------------------------------------
df.air_vst <- lst.data[["air_vst"]] %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::mutate(isOutlier = ifelse(test = (visitors >= quantile(visitors, p = CRITERION)), 
                                   yes = 1, no = 0)) %>% 
  dplyr::ungroup()
  
if (F) {
  ids <- df.air_vst %$% unique(air_store_id)
  df.air_vst %>% 
    dplyr::filter(air_store_id == ids[49]) %>% 
    ggplot(., aes(x = visit_date, y = visitors, colour = isOutlier)) + 
    geom_point()
  
  df.air_vst %>% 
    dplyr::filter(air_store_id == ids[49]) %>% 
    ggplot(., aes(x = visitors, y = ..density..)) + 
    geom_histogram(bins = 30)
}

data.train %<>% 
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date")) %>% 
  dplyr::select(-visitors)
  
data.test %<>% 
  dplyr::left_join(., df.air_vst, by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(isOutlier = 0) %>% 
  dplyr::select(-visitors)
  

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else {
  write.table(data.train, file = "./804_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./804_test.csv", sep = ",", na = "", row.names = F) 
}


