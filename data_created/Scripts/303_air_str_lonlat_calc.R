# air_str関連の特徴量
# 経度緯度の計算値
# 302....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# 302_の結果
temp <- list(train = "./302_train.csv", test = "./302_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(-air_genre_name)
data.test <- temp$test %>% dplyr::select(-air_genre_name)


# air_str --------------------------------------------------------------------
data.train %<>% 
  dplyr::mutate(maxdiff_lat_air = max(latitude_air, na.rm = T) - latitude_air, 
                maxdiff_lon_air = max(longitude_air, na.rm = T) - longitude_air, 
                sum_lat_lon_air = longitude_air + latitude_air) %>% 
  dplyr::select(-latitude_air, -longitude_air)

data.test %<>% 
  dplyr::mutate(maxdiff_lat_air = max(latitude_air, na.rm = T) - latitude_air, 
                maxdiff_lon_air = max(longitude_air, na.rm = T) - longitude_air, 
                sum_lat_lon_air = longitude_air + latitude_air) %>% 
  dplyr::select(-latitude_air, -longitude_air)

# チェック --------------------------------------------------------------------
# NAが含まれていないか
f1 <- data.train %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum
f2 <- data.test %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum
if (f1 != 0 | f2 != 0) { stop("detect NA values") }

# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total sample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./303_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./303_test.csv", sep = ",", na = "", row.names = F)


