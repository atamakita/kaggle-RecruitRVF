# 予約時系列が存在する先のEDA

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

# 休日
holidays <- lst.data[["date"]] %>% 
  dplyr::filter(holiday_flg == 1)

# ---------------------------------------------------------------- 個別店舗の予約時系列(air)
data <- lst.data[["air_rsv"]] %>% 
  dplyr::mutate(reserve_date = date(reserve_datetime),
                reserve_hour = hour(reserve_datetime),
                reserve_wday = wday(reserve_datetime, label = TRUE),
                visit_date = date(visit_datetime),
                visit_hour = hour(visit_datetime),
                visit_wday = wday(visit_datetime, label = TRUE),
                diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
                diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# air_rsvにレコードがある債務者を抽出
id.rsv <- lst.data[["air_rsv"]] %>% 
  dplyr::group_by(air_store_id) %>% 
  dplyr::summarise(mean_reserve_visitors = mean(reserve_visitors, na.rm=T), 
                   medi_reserve_visitors = median(reserve_visitors, na.rm=T), 
                   n = length(reserve_visitors)) %>% 
  dplyr::arrange(dplyr::desc(n)) %>% 
  dplyr::left_join(., lst.data[["stores"]], by = c("air_store_id"))

id.rsv %$% plot(n, medi_reserve_visitors)


# 上位2店舗の予約人数時系列を確認
# 予約来店人数の予約日別時系列
# .[5]はランチが多い
id <- "air_7514d90009613cd6"
id <- id.rsv %>% data.frame %$% air_store_id %>% .[200]
p1 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' visit date", title = paste0(id, " - visits by day (with holidays)"))

p2_vst <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(visit_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "blue")

p2_rsv <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(reserve_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_hour, all_visitors)) +
  geom_col(fill = "blue")


p3 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::filter(diff_hour < 24*5) %>%
  dplyr::group_by(diff_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "blue") +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,1,2,3,4),2,3, byrow = T)
Rmisc::multiplot(p1, p2_rsv, p2_vst, p3, layout=layout)


# ---------------------------------------------------------------- 個別店舗の来店時系列
data <- lst.data[["air_vst"]] %>% 
  dplyr::mutate(visit_month = month(visit_date),
                visit_day = day(visit_date),
                visit_wday = wday(visit_date, label = TRUE, abbr = F))

# 来店総人数時系列
id <- id.rsv %>% data.frame %$% air_store_id %>% .[1]
p1 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' Actual visit date", title = id)

# 月別ヒストグラム
p2 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "blue", bins = 50) +
  scale_x_log10()

# 曜日別来店者中央値
p3 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", 
        axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")

p4 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
Rmisc::multiplot(p1, p2, p3, p4, layout=layout)

# ---------------------------------------------------------------- 来店人数から予約人数を引く

id <- id.rsv %>% data.frame %$% air_store_id %>% .[1]
data.rsv <- lst.data[["air_rsv"]] %>% 
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::mutate(visit_date = date(visit_datetime)) %>% 
  dplyr::group_by(visit_date) %>% 
  dplyr::summarise(reserve_visitors = sum(reserve_visitors))
data.vst <- lst.data[["air_vst"]] %>% 
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::full_join(., data.rsv, by = c("visit_date")) %>% 
  tidyr::replace_na(list(reserve_visitors = 0, visitors = 0)) %>% 
  dplyr::mutate(noreserve_visitors = visitors - reserve_visitors)

p <- data.vst %>% 
  ggplot(aes(visit_date, noreserve_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' Actual visit date", title = id)

# ---------------------------------------------------------------- 適当に予測してみる
data <- lst.data[["air_vst"]] %>% 
  dplyr::mutate(visit_month = month(visit_date),
                visit_day = day(visit_date),
                visit_wday = wday(visit_date, label = TRUE, abbr = F), 
                log_visitors = log(1 + visitors))

# 来店総人数時系列
id <- id.rsv %>% data.frame %$% air_store_id %>% .[1]
p1 <- data %>%
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(log_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' Actual visit date", title = id)

# 予測
event_df <- holidays %>% 
  dplyr::select(ds = calendar_date) %>% 
  dplyr::mutate(holiday = !!"holiday")
model <- data %>% 
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::select(ds = visit_date, y = log_visitors) %>% 
  prophet(., weekly.seasonality = T, yearly.seasonality = T, holidays = event_df)

future = make_future_dataframe(model, 50)
fore <- predict(model, future)
plot(model, fore)
plot_forecast_component(fore, name=c("yearly"))

# ---------------------------------------------------------------- カオス分析
id <- id.rsv %>% data.frame %$% air_store_id %>% .[1]
df.date <- tibble::data_frame(visit_date = (ymd("2016-01-1") + days(0:(16*31 + 24))))
df.chaos <- 
  lst.data[["air_vst"]] %>% 
  dplyr::filter(air_store_id == !!id) %>% 
  dplyr::left_join(df.date, ., by = c("visit_date")) %>%
  dplyr::mutate(visit_month = month(visit_date),
                visit_day = day(visit_date),
                visit_wday = wday(visit_date, label = TRUE, abbr = F), 
                visitors_next = lead(visitors, n = 7))
df.chaos%>% 
  ggplot(., aes(x = visitors, y = visitors_next, colour = visit_wday)) + 
  geom_point()

df.chaos %>% 
  dplyr::select(visitors, visitors_next) %>% 
  TDA::alphaComplexDiag()

