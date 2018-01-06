# air_str関連の特徴量
# 経度緯度の計算値
# 302....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# 302_の結果
temp <- list(train = "./502_train.csv", test = "./502_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x), col_types = "cDddc"))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(-hpg_genre_name)
data.test <- temp$test %>% dplyr::select(-hpg_genre_name)


# hpg_str --------------------------------------------------------------------
data.train %<>% 
  dplyr::mutate(maxdiff_lat_hpg = max(latitude_hpg, na.rm = T) - latitude_hpg, 
                maxdiff_lon_hpg = max(longitude_hpg, na.rm = T) - longitude_hpg, 
                sum_lat_lon_hpg = longitude_hpg + latitude_hpg) %>% 
  dplyr::select(-latitude_hpg, -longitude_hpg)

data.test %<>% 
  dplyr::mutate(maxdiff_lat_hpg = max(latitude_hpg, na.rm = T) - latitude_hpg, 
                maxdiff_lon_hpg = max(longitude_hpg, na.rm = T) - longitude_hpg, 
                sum_lat_lon_hpg = longitude_hpg + latitude_hpg) %>% 
  dplyr::select(-latitude_hpg, -longitude_hpg)

# チェック --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total sample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./503_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./503_test.csv", sep = ",", na = "", row.names = F)


