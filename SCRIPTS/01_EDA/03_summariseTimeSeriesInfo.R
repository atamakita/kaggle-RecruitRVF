# 各店舗の時系列情報を集約

library(magrittr)
library(ggplot2)
library(dplyr)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)
library(TDA)

# ---------------------------------------------------------------- ディレクトリ
setwd("/Users/shunsukeakita/Park/RecruitRVF/SCRIPTS/01_EDA/")

# ---------------------------------------------------------------- 読込
# ファイル名
lst.path <- list(air_rsv = "air_reserve.csv", 
                 air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv", 
                 hpg_rsv = "hpg_reserve.csv", 
                 hpg_str = "hpg_store_info.csv", 
                 date    = "date_info.csv", 
                 stores  = "store_id_relation.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../../data_org/", x)))

# ---------------------------------------------------------------- 情報を集約

air_rsv.info <- lst.data[["air_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime) %>% parse_character) %>% 
  dplyr::group_by(air_store_id, visit_date) %>% 
  dplyr::summarise(reserve_visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_air_rsv = mean(reserve_visitors, na.rm=T), 
                   medi_air_rsv = median(reserve_visitors, na.rm=T), 
                   n_air_rsv = length(reserve_visitors)) %>% 
  dplyr::arrange(dplyr::desc(n_air_rsv))

hpg_rsv.info <- lst.data[["hpg_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime) %>% parse_character) %>% 
  dplyr::group_by(hpg_store_id, visit_date) %>% 
  dplyr::summarise(reserve_visitors = sum(reserve_visitors, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hpg_store_id) %>% 
  dplyr::summarise(mean_hpg_rsv = mean(reserve_visitors, na.rm=T), 
                   medi_hpg_rsv = median(reserve_visitors, na.rm=T), 
                   n_hpg_rsv = length(reserve_visitors)) %>% 
  dplyr::arrange(dplyr::desc(n_hpg_rsv)) %>% 
  dplyr::left_join(lst.data[["stores"]], ., by = c("hpg_store_id"))

air_vst.info <- lst.data[["air_vst"]] %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_air_vst = mean(visitors, na.rm=T), 
                   medi_air_vst = median(visitors, na.rm=T), 
                   n_air_vst = length(visitors)) %>% 
  dplyr::arrange(dplyr::desc(n_air_vst))

df.info <- dplyr::left_join(air_vst.info, air_rsv.info, by=c("air_store_id")) %>% 
  dplyr::left_join(., hpg_rsv.info, by = c("air_store_id")) %>% 
  dplyr::left_join(., lst.data[["air_str"]], by = "air_store_id") %>% 
  dplyr::select(-latitude, -longitude) %>% 
  dplyr::left_join(., lst.data[["hpg_str"]], by = "hpg_store_id") %>% 
  dplyr::select(-latitude, -longitude)
  
write.table(df.info, file = "../../data_created/n_record_info.csv", sep = ",", na = "", row.names = F)

# ---------------------------------------------------------------- 軽く集計
df.info %>% 
  dplyr::group_by(air_genre_name) %>% 
  dplyr::summarise(med = median(medi_air_rsv, na.rm = T), 
                   N = length(medi_air_rsv))
  

