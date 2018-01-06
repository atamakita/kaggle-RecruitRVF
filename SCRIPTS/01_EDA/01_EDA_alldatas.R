# とりあえずデータを俯瞰する

library(magrittr)
library(ggplot2)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意

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

# チェック
if (T) {
  # headと型
  lst.data %>% purrr::map(head)
  
  # サイズ
  lst.data %>% purrr::map(dim)
}

# ---------------------------------------------------------------- air_reserve
data <- lst.data[["air_rsv"]]

# 予約実行時間と予約来店時間の差分日数などを加える
data %<>% dplyr::mutate(reserve_date = date(reserve_datetime),
                        reserve_hour = hour(reserve_datetime),
                        reserve_wday = wday(reserve_datetime, label = TRUE),
                        visit_date = date(visit_datetime),
                        visit_hour = hour(visit_datetime),
                        visit_wday = wday(visit_datetime, label = TRUE),
                        diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
                        diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))


# 予約来店人数の予約日別時系列
p1_vst <- data %>%
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' visit date", title = "visits by day (with holidays)")

p1_rsv <- data %>%
  dplyr::group_by(reserve_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' reserve date", title = "reserves by day (with holidays)")

p2_vst <- data %>%
  dplyr::group_by(visit_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "blue")

p2_rsv <- data %>%
  dplyr::group_by(reserve_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_hour, all_visitors)) +
  geom_col(fill = "blue")


p3 <- data %>%
  dplyr::filter(diff_hour < 24*5) %>%
  dplyr::group_by(diff_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "blue") +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,1,2,2,2,3,4,5),3,3, byrow = T)
Rmisc::multiplot(p1_rsv, p1_vst, p2_rsv, p2_vst, p3, layout=layout)

if (F) {
  # カレンダーで予約人数を描写する
  data.visit <- data %>% 
    dplyr::group_by(visit_datetime) %>% 
    dplyr::summarise(reserve_visitors = sum(reserve_visitors)) %>% 
    dplyr::mutate(date = date(visit_datetime), 
                         month = month(visit_datetime), 
                         wday = wday(visit_datetime, label = T), 
                         hour = hour(visit_datetime))
  # カレンダー用df
  PlotData <- data.visit %>%
    frame_calendar(x = hour, y = reserve_visitors, date = date, calendar = "monthly")
  
  # カレンダープロット
  ggplot(PlotData, aes(x = .hour, y = .reserve_visitors, group = date)) %>% 
    add(geom_line(col = "red")) %>% 
    add(scale_color_brewer(palette = "Dark2")) %>%  
    add(theme(legend.position = "bottom",
          panel.grid = element_blank(), 
          panel.background = element_rect(fill = "#0a0a0a"),
          plot.background = element_rect(fill = "#0a0a0a"))) %>% 
    sugrrants::prettify(., label = "label", size = 3)
  
}

# ---------------------------------------------------------------- hpg_reserve
data <- lst.data[["hpg_rsv"]]

# 予約実行時間と予約来店時間の差分日数などを加える
data %<>% dplyr::mutate(reserve_date = date(reserve_datetime),
                        reserve_hour = hour(reserve_datetime),
                        reserve_wday = wday(reserve_datetime, label = TRUE, abbr = F),
                        visit_date = date(visit_datetime),
                        visit_hour = hour(visit_datetime),
                        visit_wday = wday(visit_datetime, label = TRUE, abbr = F),
                        diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
                        diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# 予約来店人数の予約日別時系列
p1_vst <- data %>%
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'hpg' visit date")

p1_rsv <- data %>%
  dplyr::group_by(reserve_date) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'hpg' reserve date")

p2_vst <- data %>%
  dplyr::group_by(visit_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "blue")

p2_rsv <- data %>%
  dplyr::group_by(reserve_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_hour, all_visitors)) +
  geom_col(fill = "blue")


p3 <- data %>%
  dplyr::filter(diff_hour < 24*5) %>%
  dplyr::group_by(diff_hour) %>%
  dplyr::summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "blue") +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,1,2,2,2,3,4,5),3,3, byrow = T)
Rmisc::multiplot(p1_rsv, p1_vst, p2_rsv, p2_vst, p3, layout=layout)

# ---------------------------------------------------------------- air_visit
data <- lst.data[["air_vst"]]

# 予約実行時間と予約来店時間の差分日数などを加える
data %<>% dplyr::mutate(visit_month = month(visit_date),
                        visit_day = day(visit_date),
                        visit_wday = wday(visit_date, label = TRUE, abbr = F))

# 来店総人数時系列
p1 <- data %>%
  dplyr::group_by(visit_date) %>%
  dplyr::summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(color = 'steelblue') + 
  geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 month") +
  labs(x = "'air' Actual visit date")

# 月別ヒストグラム
p2 <- data %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "blue", bins = 50) +
  scale_x_log10()

# 曜日別来店者中央値
p3 <- data %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")
  
p4 <- data %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
Rmisc::multiplot(p1, p2, p3, p4, layout=layout)

# ---------------------------------------------------------------- store_info
data.air_str <- lst.data[["air_str"]] %>% 
  dplyr::rename(latitude_air = latitude, longitude_air = longitude)
data.hpg_str <- lst.data[["hpg_str"]] %>% 
  dplyr::rename(latitude_hpg = latitude, longitude_hpg = longitude)
data.dict <- lst.data[["stores"]]

# air-hpgの連結df
data <- dplyr::full_join(data.dict, data.air_str, by = c("air_store_id")) %>% 
  dplyr::full_join(., data.hpg_str, by = c("hpg_store_id"))

# air, hpgのサンプル数
data %>% dplyr::summarise(N_air = sum(!is.na(air_store_id)), 
                          N_hpg = sum(!is.na(hpg_store_id)), 
                          N_and = sum(!is.na(air_store_id) & !is.na(hpg_store_id)), 
                          N     = length(air_store_id))

# ジャンルの対応表(NA除く)
data %>% 
  dplyr::filter(!is.na(air_store_id) & !is.na(hpg_store_id)) %>% 
  dplyr::count(air_genre_name, hpg_genre_name) %>% 
  tidyr::spread(air_genre_name, n, fill=0) %>% 
  View


# ---------------------------------------------------------------- submission
data <- lst.data[["submit"]] %$% 
  stringr::str_split(string = id, pattern = "_", simplify = T) %>% 
  data.frame %>% 
  dplyr::transmute(air_store_id = paste0(X1, "_", X2), 
                   visit_date = X3)
 
# submitに含まれる先はair_XXXファイルに含まれるか？
(unique(data$air_store_id) %in% unique(lst.data[["air_rsv"]]$air_store_id)) %>% sum

id_air_sub <- data %$% unique(air_store_id)
id_air_rsv <- lst.data[["air_rsv"]] %$% unique(air_store_id)
id_air_vst <- lst.data[["air_vst"]] %$% unique(air_store_id)
id_air_str <- lst.data[["air_str"]] %$% unique(air_store_id)

length(id_air_sub) # 821
length(id_air_rsv) # 314
length(id_air_vst) # 829
length(id_air_str) # 829
(id_air_sub %in% id_air_rsv) %>% sum # rsvの店はsubmitには含まれている
(id_air_sub %in% id_air_vst) %>% sum # vstにsubmitの店はすべて含まれる
(id_air_str %in% id_air_vst) %>% sum # strとvstの店は一致

# 基本的な変動は個社の時系列を使用して、補助的な情報をair_rsvやhpgからひっぱる方針












