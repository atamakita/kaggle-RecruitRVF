# hpg_str関連の特徴量
# 店舗所在地の情報
# 101_....Rの結果が必要

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(hpg_str = "hpg_store_info.csv", 
                 stores  = "store_id_relation.csv")
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
df.hpg_str <- lst.data[["hpg_str"]] %>% 
  tidyr::separate(hpg_area_name, c("hpg_area_ken_name", "hpg_area_shi_name", "hpg_area_cho_name"), 
                  sep = " ", extra = "drop") %>% 
  dplyr::mutate(hpg_area_kenshi_name = paste0(hpg_area_ken_name, "__", hpg_area_shi_name), 
                hpg_area_kenshicho_name = paste0(hpg_area_kenshi_name, "__", hpg_area_cho_name)) %>% 
  dplyr::select(-latitude, -longitude, -hpg_genre_name) %>% 
  dplyr::left_join(., lst.data[["stores"]], by = "hpg_store_id")

data.train <- df.hpg_str %>% dplyr::left_join(data.train, ., by = "air_store_id") %>% 
  dplyr::select(-hpg_store_id)
data.test <- df.hpg_str %>% dplyr::left_join(data.test, ., by = "air_store_id") %>% 
  dplyr::select(-hpg_store_id)


# チェック --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
if (f1 != 0 | f2 != 0) { stop("detect total sample change") }

# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./501_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./501_test.csv", sep = ",", na = "", row.names = F)


