# 個別店舗が外れ値をとる日は店舗固有のイベント日なのではないか？


library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/SCRIPTS/01_EDA/")

# データ読み込み --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_rsv = "air_reserve.csv", 
                 air_str = "air_store_info.csv", 
                 air_vst = "air_visit_data.csv", 
                 hpg_rsv = "hpg_reserve.csv", 
                 hpg_str = "hpg_store_info.csv", 
                 # date    = "date_info.csv", 
                 stores  = "store_id_relation.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../../data_org/", x)))
lst.data[["date"]] <- readr::read_csv(file = "../../data_created/001_date_info_grouped.csv", 
                                      locale = readr::locale(encoding = "UTF-8"))

# 外でつくった時系列頻度などのサマリー
lst.data[["summary"]] <- readr::read_csv("../../data_created/n_record_info.csv")

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(!is.na(holiday)) %>% 
  dplyr::select(ds = calendar_date, holiday = holiday)


# データ抽出 --------------------------------------------------------------------
# [1] "air_52a08ef3efdb4bb0" "air_17a6ab40f97fd4d8" "air_7514d90009613cd6"
# [4] "air_0768ab3910f7967f" "air_8f13ef0f5e8c64dd" "air_629edf21ea38ac2d"

id.use <- lst.data[["summary"]] %>% 
  dplyr::select(air_store_id, hpg_store_id, n_air_vst, n_air_rsv, n_hpg_rsv, air_genre_name) %>% 
  na.omit

id.vec <- id.use$air_store_id
for (id in id.vec) {
  if (F) {
    id.air <- id.vec[125]
    # id.air <- "air_52a08ef3efdb4bb0"
    id.hpg <- id.use %>% dplyr::filter(air_store_id == id.air) %>% .$hpg_store_id
    genre.air <- id.use %>% dplyr::filter(air_store_id == id.air) %>% .$air_genre_name
  }
  
  # air_vst
  data.vst <- lst.data[["air_vst"]] %>% 
    dplyr::filter(air_store_id == !!id.air) %>% 
    dplyr::mutate(visit_date = parse_character(visit_date))
  
  # air_rsv
  data.air <- lst.data[["air_rsv"]] %>% 
    dplyr::filter(air_store_id == !!id.air) %>% 
    dplyr::mutate(visit_date = date(visit_datetime) %>% parse_character) %>% 
    dplyr::group_by(visit_date) %>% 
    dplyr::summarise(visitors_air = sum(reserve_visitors, na.rm = T))
  
  # hpg_rsv
  data.hpg <- lst.data[["hpg_rsv"]] %>% 
    dplyr::filter(hpg_store_id == !!id.hpg) %>% 
    dplyr::mutate(visit_date = date(visit_datetime) %>% parse_character) %>% 
    dplyr::group_by(visit_date) %>% 
    dplyr::summarise(visitors_hpg = sum(reserve_visitors, na.rm = T))
    
  data <- list(data.vst, data.air, data.hpg) %>% 
    purrr::reduce(dplyr::left_join, by = "visit_date") %>% 
    dplyr::mutate(visit_date = ymd(visit_date), 
                  wday = wday(visit_date, label = T, abbr = F), 
                  day = day(visit_date))
  
  gp.ts_bytype <- data %>% 
    tidyr::gather(key = type, value = visitors, visitors, visitors_air, visitors_hpg) %>% 
    ggplot(aes(x = visit_date, y = visitors, group = type)) + 
    geom_line(na.rm = T) + 
    geom_point(aes(colour = wday)) + 
    theme_gray (base_family = "HiraKakuPro-W3") + 
    geom_vline(data = holidays, aes(xintercept = ds), alpha = 0.4) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    scale_x_date(date_breaks = "1 month") + 
    facet_wrap(~ type, nrow = 3) + 
    scale_y_log10() + 
    labs(title = glue::glue("{id} , genre : {genre.air}"))
  
  data.diff <- data %>% 
    tidyr::replace_na(list(visitors_air = 0, visitors_hpg = 0)) %>% 
    dplyr::mutate(diff_vst_air = visitors - visitors_air, 
                  diff_vst_hpg = visitors - visitors_hpg, 
                  diff_all     = visitors - visitors_air - visitors_hpg)
  
  gp.ts_diff <- data.diff %>% 
    tidyr::gather(key = type, value = visitors, 
                  visitors, diff_all, diff_vst_air, diff_vst_hpg) %>% 
    dplyr::mutate(type = forcats::fct_inorder(type)) %>% 
    ggplot(aes(x = visit_date, y = visitors, group = type)) + 
    geom_line(na.rm = T) + 
    geom_point(aes(colour = wday)) + 
    theme_gray (base_family = "HiraKakuPro-W3") + 
    geom_vline(data = holidays, aes(xintercept = ds), alpha = 0.4) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    scale_x_date(date_breaks = "1 month") + 
    facet_wrap(~ type, nrow = 4) + 
    scale_y_log10() + 
    labs(title = glue::glue("{id} , genre : {genre.air}"))
  
}




