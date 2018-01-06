# hpgの時系列解析
# 予約レコードのある店舗数の時系列を使用していることに注意(来店人数ではない)

library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)

# ---------------------------------------------------------------- ディレクトリ
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/Scripts/")

# ---------------------------------------------------------------- 読込
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
lst.data[["date"]] <- readr::read_csv(file = "../001_date_info_grouped.csv", 
                                      locale = readr::locale(encoding = "UTF-8"))

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(!is.na(holiday))

# ---------------------------------------------------------------- 分析
data <- lst.data[["hpg_rsv"]] %>% 
  dplyr::mutate(visit_date = date(visit_datetime))

# hpgデータの予約履歴数時系列  
data.count <- data %>% 
  dplyr::group_by(hpg_store_id, visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(visit_date = readr::parse_character(visit_date)) %>% 
  dplyr::select(visit_date, all_visitors) %>% 
  dplyr::count(visit_date) %>% 
  dplyr::mutate(visit_date = ymd(visit_date))

# ---------------------------------------------------------------- prophet
## 予測
# 休日データ(手で設定したイベント ex.お盆)
event_df <- holidays %>% 
  dplyr::select(ds = calendar_date, holiday = holiday)

# インサンプル
insample <- data.count[15:470,] %>% 
  dplyr::mutate(n = log(1 + n)) %>% 
  dplyr::select(ds = visit_date, y = n)

# モデルインスタンス作成
model <- prophet(fit = F, 
                 weekly.seasonality = T, 
                 yearly.seasonality = T, 
                 holidays = event_df) %>% 
  add_seasonality(period = 365./12., name = "monthly", fourier.order = 10)

# 学習
model %<>% fit.prophet(., insample)

# アウトサンプル期間を追加
daylength <- interval("2016-01-01", "2017-05-31") %>% divide_by(days(1))
future <- tibble::tibble(ds = ymd("2016-01-01") + days(0:daylength))

# 予測
fore <- predict(model, future)

# 出力
fore %>% 
  dplyr::select(ds, trend, 
                holidays, sanganichi, GW, obon, nenmatsu, other, pre, 
                seasonal, weekly, monthly, yearly) %>% 
  magrittr::set_colnames(paste0(names(.), "_hpgTS")) %>%
  dplyr::rename(ds = ds_hpgTS) %>% 
  write.table(., file = "../002_hpg_seasonality.csv", sep = ",", na = "", row.names = F)


# ---------------------------------------------------------------- おまけ
## 分析用のdf
# 予測と実績の残差をみる
act <- insample %>% dplyr::mutate(ds = readr::parse_character(ds))

est <- fore %>% dplyr::select(ds, yhat) %>% tibble::as.tibble() %>% 
  dplyr::mutate(ds = readr::parse_character(ds))

res <- act %>% 
  dplyr::left_join(est, ., by = "ds") %>% 
  tibble::as_data_frame() %>% 
  dplyr::mutate(residual = y - yhat, 
                ds = ymd(ds)) %>% 
  dplyr::select(ds, residual)

# 残差の時系列
res %>% ggplot(aes(ds, residual)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month")
  
# 季節調整の時系列
prophet_plot_components(model, fore)








