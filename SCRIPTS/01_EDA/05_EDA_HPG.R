# hpgの時系列解析

library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)

# ---------------------------------------------------------------- ディレクトリ
setwd("/Users/shunsukeakita/Park/RecruitRVF/SCRIPTS/01_EDA/")

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
lst.data[["date"]] <- readr::read_csv(file = "../../data_created/001_date_info_grouped.csv", 
                                      locale = readr::locale(encoding = "UTF-8"))


# 外でつくった時系列頻度などのサマリー
lst.data[["summary"]] <- readr::read_csv("../../data_created/n_record_info.csv")
id.rsv <- lst.data[["summary"]]

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(!is.na(holiday))
  dplyr::filter(holiday_flg == 1)

# ---------------------------------------------------------------- 分析
data <- lst.data[["hpg_rsv"]] %>% 
  dplyr::mutate(reserve_date = date(reserve_datetime),
                reserve_hour = hour(reserve_datetime),
                reserve_wday = wday(reserve_datetime, label = TRUE, abbr = F),
                visit_date = date(visit_datetime),
                visit_hour = hour(visit_datetime),
                visit_wday = wday(visit_datetime, label = TRUE, abbr = F),
                diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
                diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# hpgの店舗ごとの予約来店人数時系列
data.hpgTS <- data %>% 
  dplyr::group_by(hpg_store_id, visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  dplyr::mutate(visit_date = readr::parse_character(visit_date)) %>% 
  tidyr::spread(hpg_store_id, all_visitors) %>% 
  dplyr::mutate(visit_date = ymd(visit_date))

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
  add_seasonality(period = 365./12., name = "monthly", fourier.order = 5)

# 学習
model %<>% fit.prophet(., insample)

# アウトサンプル期間を追加
future <- insample %>% 
  tail(., 1) %$% ds %>% 
  interval(., "2017-05-31") %>% 
  divide_by(days(1)) %>% 
  make_future_dataframe(model, .)

# 予測
fore <- predict(model, future)

  
## submitファイルの作製
# 全期間をsubmit形式で作成
temp <- fore %>% 
  dplyr::select(ds, yhat) %>% 
  dplyr::mutate(id = paste0(id, "_", ds), 
                visitors = exp(yhat) - 1) %>% 
  dplyr::select(-ds, -yhat) %>% 
  tibble::as_data_frame()

# 出力用
df.submit %<>% rbind(., tail(temp, 39))

## 分析用のdf
# 予測と実績の残差をみる
act <- insample %>% 
  dplyr::mutate(key = paste0(id, "_", ds))

est <- fore %>% 
  dplyr::select(ds, yhat) %>% 
  dplyr::mutate(key = paste0(id, "_", ds))

res <- act %>% 
  dplyr::select(-ds) %>% 
  dplyr::left_join(est, ., by = "key") %>% 
  tibble::as_data_frame() %>% 
  dplyr::mutate(residual = y - yhat, 
                ds = ymd(ds)) %>% 
  dplyr::select(ds, residual)

res %>% ggplot(aes(ds, residual)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month")
  
prophet_plot_components(model, fore)


data.tidy %>%
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'hpg' visit date")







