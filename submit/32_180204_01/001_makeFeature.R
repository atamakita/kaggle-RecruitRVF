# XGBoostのための特徴量作り

library(magrittr)
library(ggplot2)
library(tidyverse)
library(lubridate) # 要素で渡すとエラーがでるので注意

# ディレクトリ --------------------------------------------------------------------
setwd("/Users/shunsukeakita/Park/RecruitRVF/submit/32_180204_01/")

# 関数 --------------------------------------------------------------------
# 特徴量の読み込み
read_data_rrvf <- function(num, type) {
  path <- paste0("../../data_created/", num, "_", type, ".csv")
  coltypes <- readr::read_lines(path, n_max = 1) %>% 
    stringr::str_split(",") %>% 
    unlist %>% length %>% 
    stringr::str_dup("c", times = .)
  
  glue::glue("file: {path}, col_types: {coltypes}") %>% print 
  
  data <- readr::read_csv(file = path, 
                          col_types = coltypes, 
                          locale = readr::locale(encoding = "UTF-8"))
  return(data)
}


# outsampleに対するPCA
# f.data: dataframe
# f.stats: insampleの各変数の平均値と標準偏差
# f.pca: prcomp$rotation
transrate_PCA <- function(f.data, f.stats, f.pca) {
  if (F) {
    f.data <- data.test
    f.stats <- df.statsPCA
    f.pca <- pca$rotation
  }
  
  f.pca %<>% data.frame %>% rownames_to_column("varnm")
  f.data %<>% dplyr::select(!!f.pca$varnm) %>% 
    purrr::map_dfr(~ parse_guess(.x)) %>% 
    purrr::imap(~ .x %>% 
                  add(., -dplyr::filter(f.stats, varnm == .y)[1,1]) %>%
                  divide_by(., dplyr::filter(f.stats, varnm == .y)[1,2])) %>% 
    dplyr::bind_cols()
  
  lst.pca <- list()
  for (nm in colnames(f.pca)[-1]) {
    lst.pca[[nm]] <- f.data %>% as.matrix %>% multiply_by_matrix(f.pca[,nm]) %>% as.vector
  }
  ret <- lst.pca %>% dplyr::bind_cols()
  
  return(ret)
}


# モデルフラグの付与
# flg_vst(rsv情報を使用しない)のみ現状有効
attach_model_flg <- function(data) {
  if (F) {
    data <- data.test
  }
  
  # air_vst
  data %<>% dplyr::mutate(flg_vst = ifelse((as.numeric(isOutlier) == 1) | 
                                             (as.numeric(cumcnt_vst_wday12) <= 3),# | (isExcludeDate == 1) , 
                                           0, 1))
  
  # air_vst, air_rsv
  data %<>% dplyr::mutate(flg_vst_air = 0)
  
  # air_vst, hpg_rsv
  data %<>% dplyr::mutate(flg_vst_hpg = 0)
  
  # air_vst, air_rsv, hpg_rsv
  data %<>% 
    dplyr::mutate(flg_vst_air_hpg = 0)
  
  data %>% dplyr::select(flg_vst, flg_vst_air, flg_vst_hpg, flg_vst_air_hpg) %>% 
    purrr::map(~ .x %>% table) %>% 
    do.call("rbind", .) %>% print
  
  return(data)
}


# 読み込み --------------------------------------------------------------------
list.files("../../data_created/")
# 使用する特徴量番号
use_features <- c(101, 103, 106, 108, 109, 301, 302, 303, 
                  602, 801, 802, 804, 805)
use_weight <- 701

# 特徴量
lst.train <- use_features %>% purrr::map(~ read_data_rrvf(.x, "train"))
lst.test  <- use_features %>% purrr::map(~ read_data_rrvf(.x, "test"))

# サンプルウェイト
weight.train <- read_data_rrvf(use_weight, "train") %>% 
  dplyr::mutate(visit_date = ymd(visit_date))

# マージ --------------------------------------------------------------------
data.train <- lst.train %>% 
  purrr::reduce(dplyr::left_join, by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(id = paste0(air_store_id, "_", visit_date))

data.test <- lst.test %>% 
  purrr::reduce(dplyr::left_join, by = c("air_store_id", "visit_date")) %>% 
  dplyr::mutate(id = paste0(air_store_id, "_", visit_date))

# チェック
data.train %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .)
data.test %>% purrr::map(~ is.na(.x) %>% sum) %>% do.call("rbind", .)


# モデルフラグ付与 --------------------------------------------------------------------
data.train %<>% attach_model_flg()
data.test %<>% attach_model_flg() %>% dplyr::mutate(flg_vst = 1)

# PCA --------------------------------------------------------------------
# まずは型変換
dataforPCA <- data.train %>% purrr::map_dfr(~ parse_guess(.x))

# 数値型の変数だけを取り出す
usecols <- dataforPCA %>% 
  purrr::map(~ class(.x)) %>% 
  do.call("rbind", .) %>% 
  data.frame %>% 
  rownames_to_column() %>% 
  set_colnames(c("varnm", "class")) %>% 
  dplyr::filter(class %>% is_in(c("integer", "numeric"))) %$% varnm

dataforPCA %<>% dplyr::select(!!usecols) %>% 
  dplyr::select(-visitors, -month_vst, -day_vst, -int_vst, -visitors2,  
                -cumcnt_vst_365, -cumcnt_vst_60, -cumcnt_vst_wday12, -cumcnt_vst_wday24, 
                -cumsd_log_vst_365, -cumsd_log_vst_60, -cumsd_log_vst_wday24,
                -latitude_air, -longitude_air, 
                -maxdiff_lat_air, -maxdiff_lon_air, -sum_lat_lon_air, 
                -trend_log, -isOutlier, -isExcludeDate, 
                -flg_vst, -flg_vst_air, -flg_vst_hpg, -flg_vst_air_hpg)

# trainデータの統計量をtestデータの変換用に取得
df.statsPCA <- dataforPCA %>% 
  purrr::map_dfr(~ data.frame(stats_mean = .x %>% mean(., na.rm = T), 
                              stats_sd   = .x %>% sd(., na.rm = T))) %>% 
  dplyr::mutate(varnm = colnames(dataforPCA))


# PCAの結果+trainデータの変換値
pca <- dataforPCA %>% 
  purrr::imap(~ .x %>% 
                add(., -dplyr::filter(df.statsPCA, varnm == .y)[1,1]) %>% 
                divide_by(., dplyr::filter(df.statsPCA, varnm == .y)[1,2])) %>% # 正規化
  dplyr::bind_cols() %>% 
  prcomp(.)

data.train.pca <- pca$x %>% tibble::as_data_frame()
data.test.pca <- transrate_PCA(data.test, df.statsPCA, pca$rotation)

# チェック
if (T) {
  data.train.pca.forcheck <- transrate_PCA(data.train, df.statsPCA, pca$rotation)
  flg <- purrr::pmap(list(org = data.train.pca, test = data.train.pca.forcheck), 
                     ~ dplyr::near(.x, .y, tol = 1e-8) %>% all) %>% 
    flatten_lgl() %>% not %>% sum
  if (flg != 0) {
    stop("detect difference between prcomp() and transarte_PCA results")
  }
  rm(data.train.pca.forcheck)
}

# 一旦出力しておく
list(prcomp_result = pca, stats = df.statsPCA) %>% saveRDS(object = ., file = "result_pca.rds")

# 元のデータに追加して分割
data.train %<>% purrr::map_dfr(~ parse_guess(.x)) %>% cbind(., data.train.pca)
data.test %<>% purrr::map_dfr(~ parse_guess(.x)) %>% cbind(., data.test.pca)
 
# binary encoding --------------------------------------------------------------------
# binary matrixを作る
make_binmat <- function(size) {
  if (F) {
    size <- 3
  }
  
  ret <- data_frame(a1 = c(rep(1,2^size),rep(0,2^size)))
  
  for (i in (size-1):0) {
    ret %<>% dplyr::mutate(rep(c(rep(1,2^i),rep(0,2^i)), 2 ^ (size - i)))
    j <- size - i + 1
    colnames(ret) <- paste0("bin", c(1:j))
  }
  
  return(ret)
}

# air_store_idのbin-encoding
Nid <- data.train$air_store_id %>% unique %>% length
df.bin <- make_binmat(9) %>% set_colnames(paste0("air_store_id_", colnames(.)))
df.bin <- df.bin[1:Nid,]
df.bin$air_store_id <- data.train$air_store_id %>% unique

data.train %<>% dplyr::left_join(., df.bin, by = "air_store_id")
data.test %<>% dplyr::left_join(., df.bin, by = "air_store_id")

# day_vstのbin-encoding
Nid <- data.train$day_vst %>% unique %>% length
df.bin <- make_binmat(4) %>% set_colnames(paste0("day_vst_", colnames(.)))
df.bin <- df.bin[1:Nid,]
df.bin$day_vst <- data.train$day_vst %>% unique

data.train %<>% dplyr::left_join(., df.bin, by = "day_vst")
data.test %<>% dplyr::left_join(., df.bin, by = "day_vst")

# air_genre_nameのbin-encoding
Nid <- data.train$air_genre_name %>% unique %>% length
df.bin <- make_binmat(3) %>% set_colnames(paste0("air_genre_name_", colnames(.)))
df.bin <- df.bin[1:Nid,]
df.bin$air_genre_name <- data.train$air_genre_name %>% unique

data.train %<>% dplyr::left_join(., df.bin, by = "air_genre_name")
data.test %<>% dplyr::left_join(., df.bin, by = "air_genre_name")

# air_ken_name_nameのbin-encoding
Nid <- data.train$air_area_ken_name %>% unique %>% length
df.bin <- make_binmat(3) %>% set_colnames(paste0("air_area_ken_name_", colnames(.)))
df.bin <- df.bin[1:Nid,]
df.bin$air_area_ken_name <- data.train$air_area_ken_name %>% unique

data.train %<>% dplyr::left_join(., df.bin, by = "air_area_ken_name")
data.test %<>% dplyr::left_join(., df.bin, by = "air_area_ken_name")

# 出力 --------------------------------------------------------------------
# trainとtestデータ
write.table(data.train, file = "./train.csv", sep = ",", na = "", row.names = F, quote = F)
write.table(data.test, file = "./test.csv", sep = ",", na = "", row.names = F, quote = F)
write.table(weight.train, file = "./weight.csv", sep = ",", na = "", row.names = F, quote = F)

colnames(data.test) %in% colnames(data.train)

# モデル別に使用する列名(の雛形)
if (F) {
  df.coltype <- data.test %>% 
    purrr::map(~ class(.x)) %>% 
    do.call("rbind", .) %>% 
    data.frame %>% 
    tibble::rownames_to_column() %>% 
    set_colnames(c("colnm", "type")) %>% 
    dplyr::mutate(submit = NA, 
                  model_vst = NA, 
                  model_vst_air = NA, 
                  model_vst_hpg = NA, 
                  model_vst_air_hpg = NA)
  write.table(df.coltype, file = "./column_list.csv", sep = ",", na = "", row.names = F)  
}


# おまけ --------------------------------------------------------------------
# library(Rtsne)
# 
# usecols <- data.train %>% 
#   purrr::map(~ class(.x)) %>% 
#   do.call("rbind", .) %>% 
#   data.frame %>% 
#   tibble::rownames_to_column() %>% 
#   set_colnames(c("var", "a", "b")) %>% 
#   dplyr::filter(a %in% c("numeric", "integer")) %>% .$var
# 
# res <- Rtsne(data.train[,usecols])
# 
# res.tsne <- res$Y %>% tibble::as_data_frame()
# res.tsne <- data.train %>% 
#   dplyr::select(air_store_id, visit_date, month_vst, wday_vst, 
#                 air_genre_name, air_area_ken_name, holiday, isNoRecord_rsv) %>% 
#   purrr::map_dfr(~ parse_character(.x)) %>% 
#   cbind(., res.tsne) %>% 
#   tibble::as_data_frame()
# 
# write.table(res.tsne, file = "./result_tsne_train.csv", sep = ",", na = "", row.names = F)
# 
# res.tsne %>% 
#   dplyr::sample_n(10000) %>% 
#   ggplot(., aes(x=V1, y=V2, colour = holiday)) + 
#   geom_point()




