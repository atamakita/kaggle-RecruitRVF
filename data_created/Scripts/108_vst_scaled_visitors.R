# air_vst関連の特徴量
# y_pred = log(trend) + log((1+visitors) / trend)とし、
# 1項目をbase_margin, 2項目をXGBoostで予測する
# testは予測値にlog(trend)をたすことを忘れないようにすること
# 803を使用する

library(magrittr)
library(lubridate)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")


# 関数 --------------------------------------------------------------------

neglog <- function(x) sign(x) * log(1 + abs(x))

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(# air_rsv = "air_reserve.csv", 
                 # air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv", 
                 # hpg_rsv = "hpg_reserve.csv", 
                 # hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 # stores  = "store_id_relation.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))


lst.trend <- c("train", "test") %>% 
  purrr::map(~ readr::read_csv(file = paste0("./803_", .x, ".csv"), 
                               locale = readr::locale(encoding = "UTF-8"))) %>% 
  set_names(c("train", "test"))


# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- lst.data[["air_vst"]]
data.test <- lst.data[["submit"]] %>% 
  tidyr::separate(col = id, into = c("air_store_id", "visit_date"), sep = "_2017", remove = F) %>%
  dplyr::mutate(visit_date = paste0("2017", visit_date) %>% ymd) %>% 
  dplyr::select(-id)

# air_vst --------------------------------------------------------------------
# trendをvisitorsから引いてlog1p
data.train %<>% 
  dplyr::left_join(., lst.trend[["train"]], by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(visitors2 = log((1 + visitors) / trend), 
                trend_log = log(trend)) %>% 
  dplyr::select(-visitors, -trend)
  
  
data.test %<>% 
  dplyr::left_join(., lst.trend[["test"]], by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(visitors2 = 0, 
                trend_log = log(trend)) %>% 
  dplyr::select(-visitors, -trend)

if (F) {
  ids <- data.train$air_store_id %>% unique
  data.train %>% 
    dplyr::filter(air_store_id == ids[200]) %$% 
    hist(visitors2, breaks = 50)
}


# 出力 --------------------------------------------------------------------
write.table(data.train, file = "./108_train.csv", sep = ",", na = "", row.names = F)
write.table(data.test, file = "./108_test.csv", sep = ",", na = "", row.names = F)
