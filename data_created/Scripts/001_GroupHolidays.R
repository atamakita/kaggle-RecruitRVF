# 休日を編集
# 1. グループ化(三が日, GW, お盆, 年末)
# 2. 祝日前日を一つの休日カテゴリとする

library(magrittr)
library(ggplot2)
library(lubridate)

# ---------------------------------------------------------------- ディレクトリ
setwd("/Users/shunsukeakita/Park/RecruitRVF/data_created/Scripts/")

# ---------------------------------------------------------------- 読込

if (F) {
  # 手で入力したcsv
  answer <- readr::read_csv(file = "../../data_created/date_info_addname.csv", 
                            locale = readr::locale(encoding = "UTF-8"))
}

# 休日
holidays <- paste0("../../data_org/", "date_info.csv") %>% 
  readr::read_csv(file = ., 
                  locale = readr::locale(encoding = "UTF-8"), 
                  col_types = "cci")

# ---------------------------------------------------------------- 1. 休日のグループ化
# 特別な時期
sanganichi.lst <- list(date = c("2016-01-01", "2016-01-02", "2016-01-03", 
                                "2017-01-01", "2017-01-02", "2017-01-03"), 
                       holiday = rep("sanganichi", 6)) %>% dplyr::bind_cols()

GW.lst <- list(date = c(as.character(ymd("2016-04-29") + days(0:9)), 
                        as.character(ymd("2017-04-29") + days(0:8))), 
               holiday = rep("GW", 19)) %>% dplyr::bind_cols()

obon.lst <- list(date = c("2016-08-11", "2016-08-12", "2016-08-13", "2016-08-14", "2016-08-15"), 
                 holiday = rep("obon", 5)) %>% dplyr::bind_cols()

nenmatsu.lst <- list(date = c("2016-12-29", "2016-12-30", "2016-12-31"), 
                 holiday = rep("nenmatsu", 3)) %>% dplyr::bind_cols()

holiday.group <- list(a = sanganichi.lst, b = GW.lst, c = obon.lst, d = nenmatsu.lst) %>% 
  dplyr::bind_rows()

# その他の休日
holiday.other <- dplyr::anti_join(holidays, holiday.group, c("calendar_date" = "date")) %>% 
  dplyr::filter(holiday_flg == 1) %>% 
  dplyr::mutate(holiday = "other") %>% 
  dplyr::select(date = calendar_date, holiday = holiday)

# 休日の前日
holiday.pre <- rbind(holiday.group, holiday.other) %>% 
  dplyr::mutate(date = ymd(date) - days(1), holiday = "pre") %>% 
  dplyr::mutate(date = readr::parse_character(date)) %>% 
  dplyr::anti_join(., holiday.group, by = "date") %>% 
  dplyr::anti_join(., holiday.other, by = "date")

holidays <- rbind(holiday.group, holiday.other) %>% 
  rbind(., holiday.pre) %>% 
  dplyr::left_join(holidays, ., by = c("calendar_date" = "date"))

write.table(holidays, file = "../001_date_info_grouped.csv", sep = ",", na = "", row.names = F)



