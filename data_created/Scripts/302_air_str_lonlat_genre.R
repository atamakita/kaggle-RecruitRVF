# air_str関連の特徴量
# 経度緯度とジャんるの情報
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_str = "air_store_info.csv")
lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)


# air_str --------------------------------------------------------------------
df.air_str <- lst.data[["air_str"]] %>% 
  dplyr::select(air_store_id, latitude, longitude, air_genre_name) %>% 
  dplyr::rename(latitude_air = latitude, longitude_air = longitude)

data.train <- df.air_str %>% dplyr::left_join(data.train, ., by = "air_store_id")
data.test <- df.air_str %>% dplyr::left_join(data.test, ., by = "air_store_id")

# チェック --------------------------------------------------------------------
f1 <- data.train %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum
f2 <- data.test %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum

if (f1 != 0 | f2 != 0) { stop("detect NA values") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./302_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./302_test.csv", sep = ",", na = "", row.names = F)


