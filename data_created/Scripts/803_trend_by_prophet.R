# prophetで予測する
# air_vstが推定するのはtrend成分とイベント成分のみ
# 101が必要

library(magrittr)
library(ggplot2)
library(dplyr)
library(readr)
library(sugrrants)
library(lubridate) # 要素で渡すとエラーがでるので注意
library(prophet)

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/")

# 読込 --------------------------------------------------------------------
# ファイル名
lst.path <- list(air_vst = "air_visit_data.csv", 
                 submit  = "sample_submission.csv")

lst.data <- lst.path %>% 
  purrr::map(function(x) readr::read_csv(file = paste0("../data_org/", x)))
lst.data[["date"]] <- readr::read_csv(file = "./001_date_info_grouped.csv", 
                            locale = readr::locale(encoding = "UTF-8"))

# 101_の結果
temp <- list(train = "./101_train.csv", test = "./101_test.csv") %>% 
  purrr::map(~ readr::read_csv(file = paste0(.x)))

# 特徴量作り --------------------------------------------------------------------
# trainとtestに分ける
data.train <- temp$train %>% dplyr::select(air_store_id, visit_date)
data.test <- temp$test %>% dplyr::select(air_store_id, visit_date)

# prophetで予測 --------------------------------------------------------------------
# prophetのtips
# yhatが予測値ぽい
# insampleと予測値のplotはplot(model, fore)
# 日付の差はinterval(old, new) / days(1)

# 来店人数データ(目的変数の時系列データ)
data <- lst.data[["air_vst"]] %>% 
  dplyr::mutate(visit_month = month(visit_date),
                visit_day = day(visit_date),
                visit_wday = wday(visit_date, label = TRUE, abbr = F), 
                log_visitors = log(1 + visitors))

# 休日データ(手で設定したイベント ex.お盆)
event_df <- lst.data[["date"]] %>% 
  dplyr::filter(!is.na(holiday)) %>% 
  dplyr::select(ds = calendar_date, holiday = holiday)

# submit対象のid
idvec.target <- data$air_store_id %>% unique

# 来店総人数時系列
df.output <- c() # outputファイル
df.result <- tibble::data_frame(ds = ymd("2016-01-01") + days(1:516)) %>% 
  dplyr::mutate(ds = readr::parse_character(ds))# 店舗毎の残差をまとめたファイル

count <- 0
for (id in idvec.target) {
  if (F) {
    id <- idvec.target[4]
  }
  
  count %<>% add(1)
  paste0(count, " / ", length(idvec.target)) %>% print
  
  ## 予測
  # インサンプル
  insample <- data %>% 
    dplyr::filter(air_store_id == !!id) %>% 
    dplyr::select(ds = visit_date, y = log_visitors) %>% 
    dplyr::mutate(ds = parse_character(ds))
  
  # モデルインスタンス作成
  model <- prophet(fit = F, 
                   daily.seasonality = F, 
                   weekly.seasonality = 5, 
                   yearly.seasonality = F, 
                   holidays = event_df)
  
  # 学習
  model %<>% fit.prophet(., insample)
  
  # アウトサンプル期間を追加
  future <- insample %>% 
    tail(., 1) %$% ds %>% 
    interval(., "2017-05-31") %>% 
    divide_by(days(1)) %>% 
    make_future_dataframe(model, .) %>% 
    tibble::as_data_frame() %>% 
    dplyr::mutate(ds = parse_character(ds))
  
  # 予測
  fore <- predict(model, future)
  
  # plot(model, fore)
  if (F) {
    temp <- fore %>% 
      dplyr::select(ds, yhat) %>% 
      dplyr::mutate(ds = as.character(ds)) %>% 
      dplyr::left_join(., insample, by = "ds") %>% 
      dplyr::mutate(ds = ymd(ds)) %>% 
      tidyr::gather(key = type, value = log_vstr, yhat, y) %>% 
      dplyr::mutate(vstr = exp(log_vstr) - 1)
    
    ggplot(temp, aes(x = ds, y = log_vstr, colour = type)) + 
      geom_point()
    
    ggplot(temp, aes(x = ds, y = vstr, colour = type)) + 
      geom_point()
  }
  # plot_forecast_component(fore, name=c("yhat"))
  
  # 出力用ファイルの作成
  temp <- fore %>% 
    dplyr::select(ds, trend) %>% 
    dplyr::mutate(air_store_id = id, 
                  visit_date = date(ds), 
                  trend_log = trend, 
                  trend = exp(trend) - 1) %>% 
    dplyr::select(air_store_id, visit_date, trend, trend_log)
  
  df.output %<>% rbind(., temp)
}


data.train %<>% dplyr::left_join(., df.output, by = c("air_store_id", "visit_date"))
data.test %<>% dplyr::left_join(., df.output, by = c("air_store_id", "visit_date"))

# 出力 --------------------------------------------------------------------
# サンプル数が変化していないか
f1 <- nrow(data.train) - 252108
f2 <- nrow(data.test) - 32019
f3 <- nrow(data.test) - nrow(na.omit(data.test))
f4 <- nrow(data.train) - nrow(na.omit(data.train))
if (f1 != 0 | f2 != 0) { 
  stop("detect total Nsample change") 
} else if (f3 != 0) {
  stop("detect NA in test data") 
} else if (f4 != 0) {
  stop("detect NA in train data") 
} else {
  write.table(data.train, file = "./803_train.csv", sep = ",", na = "", row.names = F)
  write.table(data.test, file = "./803_test.csv", sep = ",", na = "", row.names = F) 
}


# residualの分析 --------------------------------------------------------------------
if (F) {
  # summaryを作る
  df.summ <- df.result %>% 
    dplyr::select(-ds) %>% 
    purrr::map(function(x) data.frame(N = sum(!is.na(x)), 
                                      min = min(x, na.rm = T), 
                                      med = median(x, na.rm = T), 
                                      max = max(x, na.rm = T), 
                                      mean = mean(x, na.rm = T), 
                                      sd = sd(x, na.rm = T), 
                                      residual = mean(x ^ 2, na.rm = T) %>% sqrt)) %>% 
    do.call("rbind", .) %>% 
    tibble::rownames_to_column("id") %>% 
    dplyr::left_join(., lst.data[["summary"]], by = c("id" = "air_store_id")) %>%
    dplyr::arrange(desc(residual))
  
  ggplot(df.summ, aes(x = residual, y = ..density..)) + 
    geom_histogram(alpha = 0.4, bins = 200, position = "identity") + 
    geom_density()
    
  targetrange <- c(50:60)
  for (i in targetrange) {
    if (F) {
      i <- 54
    }
    id <- df.summ$id[i]
    p1 <- df.result %>%
      dplyr::select(ds, !!id) %>% 
      dplyr::mutate(ds = ymd(ds)) %>%
      dplyr::rename(residual = !!id) %>% 
      ggplot(aes(ds, residual)) +
      geom_line(color = 'steelblue') + 
      geom_point(color = 'black') + 
      geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
      scale_x_date(date_breaks = "1 month")
    # labs(x = "'air' visit date", title = paste0(id, " - visits by day (with holidays)"))
    
    p2 <- data %>% 
      dplyr::filter(air_store_id == !!id) %>% 
      dplyr::select(ds = visit_date, y = log_visitors) %>%
      dplyr::mutate(y_ = exp(y) + 1) %>% 
      ggplot(aes(ds, y)) +
      geom_line(color = 'steelblue') + 
      geom_point(color = 'black') + 
      geom_vline(data = holidays, aes(xintercept = calendar_date), alpha = 0.4) + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
      scale_x_date(date_breaks = "1 month")
    
    data %>% 
      dplyr::filter(air_store_id == !!id) %>% 
      dplyr::select(ds = visit_date, y = log_visitors) %>% 
      ggplot(aes(x = y, y = ..density..)) +
      geom_histogram(fill = 'steelblue', bins = 10)
    
    Rmisc::multiplot(plotlist = list(p1, p2))
    Sys.sleep(2)
  }
  
  # 休日前は休日にしても良いかも
}
