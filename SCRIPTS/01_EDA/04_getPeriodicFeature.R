# prophetで予測する

library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)
library(forecast)

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
lst.data[["date"]] <- readr::read_csv(file = "../../data_created/date_info_addname.csv", 
                            locale = readr::locale(encoding = "UTF-8"))


# 外でつくった時系列頻度などのサマリー
lst.data[["summary"]] <- readr::read_csv("../../data_created/n_record_info.csv")
id.rsv <- lst.data[["summary"]]

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(holiday_flg == 1)

# ---------------------------------------------------------------- prophetで予測
# prophetのtips
# yhatが予測値ぽい
# insampleと予測値のplotはplot(model, fore)


data <- lst.data[["air_vst"]] %>% 
  dplyr::mutate(visit_month = month(visit_date),
                visit_day = day(visit_date),
                visit_wday = wday(visit_date, label = TRUE, abbr = F), 
                log_visitors = log(1 + visitors))


# 来店総人数時系列
gp_lst <- list()
for (id_idx in 1:5) {
  id <- id.rsv %>% data.frame %$% air_store_id %>% .[id_idx]
  
  # 予測
  event_df <- holidays %>% 
    dplyr::select(ds = calendar_date, holiday = holiday2)
  
  insample <- data %>% 
    dplyr::filter(air_store_id == !!id) %>% 
    dplyr::select(ds = visit_date, y = log_visitors)
  
  model <- insample %>% 
    prophet(., 
            weekly.seasonality = T, 
            yearly.seasonality = F, 
            holidays = event_df)
  
  future = make_future_dataframe(model, 50)
  fore <- predict(model, future)
  
  plot(model, fore)
  # plot_forecast_component(fore, name=c("trend"))
  gp_lst[[id_idx]] <- prophet_plot_components(model, fore)
  
  if (F) {
    # 予測と実績の残差をみる
    act <- insample %>% 
      dplyr::mutate(key = paste0(id, "_", ds))
    
    est <- fore %>% 
      dplyr::select(ds, yhat) %>% 
      dplyr::mutate(key = paste0(id, "_", ds))
    
    res <- act %>% 
      dplyr::select(-ds) %>% 
      dplyr::left_join(est, ., by = "key") %>% 
      dplyr::mutate(residual = y - yhat)
  }
}

# 調整成分を並べる
gp_lst %>% 
  purrr::map(function(x) x[[3]]) %>% 
  Rmisc::multiplot(plotlist = ., cols = 1)


# 個別の来店時系列
id <- id.rsv %>% data.frame %$% air_store_id %>% .[1]
data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(log_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' Actual visit date", title = id)




