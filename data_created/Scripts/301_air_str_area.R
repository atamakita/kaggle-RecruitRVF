# air_str関連の特徴量
# 店舗所在地の情報
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
  tidyr::separate(air_area_name, c("air_area_ken_name", "air_area_shi_name", "air_area_cho_name"), 
                  sep = " ", extra = "drop") %>% 
  dplyr::mutate(air_area_kenshi_name = paste0(air_area_ken_name, "__", air_area_shi_name), 
                air_area_kenshicho_name = paste0(air_area_kenshi_name, "__", air_area_cho_name)) %>% 
  dplyr::select(-latitude, -longitude, -air_genre_name)

data.train <- df.air_str %>% dplyr::left_join(data.train, ., by = "air_store_id")
data.test <- df.air_str %>% dplyr::left_join(data.test, ., by = "air_store_id")

# チェック --------------------------------------------------------------------
f1 <- data.train %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum
f2 <- data.test %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .) %>% sum

if (f1 != 0 | f2 != 0) { stop("detect NA values") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./301_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./301_test.csv", sep = ",", na = "", row.names = F)


