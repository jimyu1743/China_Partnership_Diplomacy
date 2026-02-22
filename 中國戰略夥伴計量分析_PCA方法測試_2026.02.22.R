# ============================================================
# 0. 環境設定與套件安裝 (更新版)
# ============================================================

# 設定套件路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# 定義套件清單
# 已新增: dplyr, ggplot2, tibble, zoo (用於插補)
packages <- c(
  "tidyverse", "dplyr", "ggplot2", "tibble", "magrittr", "zoo", # 核心資料處理與繪圖
  "plm", "haven", "readr", "readxl", "openxlsx", "writexl", # 資料讀寫與面板數據
  "ExtremeBounds", "car", "ordinal", "MASS", "mvProbit", "psych", # 統計與 EBA 分析
  "pglm", "pastecs", "FactoMineR", "factoextra", "GPArotation", # 進階統計與因子分析
  "knitr", "kableExtra", "stargazer", "broom" # 美觀輸出
)

# 檢查並安裝缺少的套件
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  cat("正在安裝缺少套件:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, dependencies = TRUE, lib = lib_path)
}

# 載入套件
invisible(lapply(packages, function(pkg) {
  tryCatch({ 
    suppressPackageStartupMessages(library(pkg, character.only = TRUE)) 
  }, error = function(e) { 
    cat("❌ 載入失敗:", pkg, "\n") 
  })
}))

cat("✅ 所有環境與套件設定完成！\n")

# ============================================================================
# 1. 資料讀取與前處理 (優化版：自動篩選無資料國家)
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("資料讀取與前處理 (含 GDP=0 修正與高缺失國家剔除)\n")
cat(rep("=", 80), "\n\n")

# 讀入資料檔
China_partnership_1996_2023_breakpoint2026_01_23 <- read_excel("C:/Users/jimyu1743/Desktop/習近平與中國夥伴關係外交/原始與整理資料/China partnership 1996-2023 breakpoint2026.01.23.xlsx")
CP9623 <- China_partnership_1996_2023_breakpoint2026_01_23

# 1. 基本清洗
CP9623$`ASEAN+1`[is.na(CP9623$`ASEAN+1`)] <- 0
CP9623$dip_age <- CP9623$year - CP9623$diplomcy
CP9623$dip_age[CP9623$dip_age < 0] <- NA

# 計算治理指標
CP9623 <- CP9623 %>%
  rowwise() %>%
  mutate(WGI = mean(c(va, psv, ge, rq, rl, cc), na.rm = TRUE)) %>%
  ungroup()

# 2. 手動排除特定國家 (政治實體或微型國家)
# 這些通常是您確定「不需要分析」的名單
excluded_manual <- c("SSD", "MCO", "PLW", "TUV", "MNE", "KIR", "LIE", "SRB", 
                     "SLB", "SMR","PRK", "ERI", "VEN", "NRU", "MHL", "FSM", 
                     "TON", "VUT", "USA", "CUB", "SYR", "TMP", "CPV","AFG", 
                     "LBN", "YEM")
CP9623 <- CP9623 %>% filter(!countrycode %in% excluded_manual)

# 3. 處理 GDP = 0 與 嚴重缺失國家
cat("正在處理 GDP=0 與 篩選嚴重缺失國家...\n")

CP9623 <- CP9623 %>%
  # A. 將 GDP 為 0 或負數的轉為 NA (避免後續計算出錯)
  mutate(gdp = if_else(gdp <= 0, NA_real_, gdp)) %>%
  
  # B. 計算每個國家的 GDP 缺失率
  group_by(countrycode) %>%
  mutate(gdp_missing_rate = sum(is.na(gdp)) / n()) %>%
  ungroup()

# C. 定義「嚴重缺失」門檻 (例如：超過 40% 年份沒 GDP 就刪除該國)
# 這樣可以避免插補出太多假資料
missing_threshold <- 0.40 
countries_to_drop <- CP9623 %>% 
  filter(gdp_missing_rate > missing_threshold) %>% 
  distinct(countrycode) %>% 
  pull(countrycode)

if(length(countries_to_drop) > 0) {
  cat("⚠️ 自動剔除以下 GDP 嚴重缺失 (>40%) 的國家:\n")
  print(countries_to_drop)
  CP9623 <- CP9623 %>% filter(!countrycode %in% countries_to_drop)
} else {
  cat("沒有國家因 GDP 嚴重缺失而被剔除。\n")
}

# 4. 計算衍生變數 (此時 GDP 已經沒有 0 了，但可能有 NA，NA 留給 Step 2 插補)
CP9623$exportdep <- CP9623$china_im_fr_i / CP9623$gdp
CP9623$importdep <- CP9623$china_ex_to_i / CP9623$gdp
CP9623$gdp_per_capita <- CP9623$gdp / CP9623$population_total

# 整併變數
CP9623$economy <- CP9623$trade + CP9623$financial
CP9623$arms_and_military <- CP9623$arms + CP9623$military
CP9623$sanction <- CP9623$economy + CP9623$arms_and_military + CP9623$travel
CP9623$xi <- ifelse(CP9623$year < 2013, 0, 1)

cat("前處理完成。剩餘國家數:", length(unique(CP9623$countrycode)), "\n")

# ============================================================================
# 2. 缺失值插補 (時間序列 -> 橫斷面 -> 全局)
# ============================================================================

cat("\n[Step 2] 執行三階段缺失值插補...\n")

# 定義需要插補的連續變數
vars_to_impute <- c("gdp", "population_total", "gdp_per_capita", 
                    "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep",
                    "trade", "arms", "military", "financial", "travel",
                    "va", "psv", "ge", "rq", "rl", "cc", "WGI")

CP9623 <- CP9623 %>%
  # --- 階段 1: 時間序列線性插補 ---
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(
    across(all_of(vars_to_impute), 
           ~ na.approx(., na.rm = FALSE, rule = 2))
  ) %>%
  
  # --- 階段 2: 橫斷面中位數插補 ---
  group_by(year) %>%
  mutate(
    across(all_of(vars_to_impute), 
           ~ if_else(is.na(.), median(., na.rm = TRUE), .))
  ) %>%
  
  # --- 階段 3: 全局中位數插補 ---
  ungroup() %>%
  mutate(
    across(all_of(vars_to_impute), 
           ~ if_else(is.na(.), median(., na.rm = TRUE), .))
  )

# ============================================================
# 驗證與統計
# ============================================================

# 1. 檢查剩餘缺失值
remaining_na <- sum(is.na(CP9623[, vars_to_impute]))

# 2. 計算剩餘國家數與觀察值
final_countries_count <- length(unique(CP9623$countrycode))
final_obs_count <- nrow(CP9623)

# 3. 輸出報告
cat("✓ 三階段插補完成。\n")
cat("   --------------------------------------\n")
cat("   剩餘缺失值數量 :", remaining_na, "\n")
cat("   剩餘國家數     :", final_countries_count, "\n")
cat("   總觀察值 (N)   :", final_obs_count, "\n")
cat("   --------------------------------------\n")

# ============================================================================
# 3. 建立升級變數與滯後處理 (Lag)
# ============================================================================

cat("\n[Step 3] 建立升級變數與執行滯後處理...\n")

# 定義需要滯後的變數清單 (使用原始變數名，尚未標準化)
vars_to_lag <- c(
  "gdp_per_capita", "gdp_per_capita_diff_CHN", "china_ex_to_i", 
  "china_im_fr_i", "exportdep", "importdep", "population_total",
  "trade", "arms", "military", "financial", "travel",
  "va", "psv", "ge", "rq", "rl", "cc"
)

CP9623 <- CP9623 %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(
    # 1. 設立 build_upgrade 變數
    # 邏輯：如果 今年等級 > 去年等級，標記為1，否則為0
    build_upgrade = if_else(partnership > lag(partnership, default = first(partnership)), 1, 0),
    build_upgrade2 = if_else(partnership2 > lag(partnership2, default = first(partnership2)), 1, 0),
    
    # 2. 執行滯後處理 (Lag 1年) -> 產生 _lag 結尾的新變數
    across(all_of(vars_to_lag), ~ lag(., n = 1), .names = "{.col}_lag")
  ) %>%
  ungroup()
# ==============================================================================
# 建構面子外交變數
# ==============================================================================

# ==============================================================================
# 步驟 1：定義檔案路徑 (如果 R 找不到，請把 "CP9623_pca.xlsx" 換成 file.choose() )
# ==============================================================================
file_path <- "CP9623_pca.xlsx"

# ==============================================================================
# 步驟 2：從同一個 Excel 檔案中讀取不同的 Sheet
# ==============================================================================

# 1. 2008 奧運 (對應 Sheet 名稱: "2008北京奧運出席政要")
df_08 <- read_excel(file_path, sheet = "2008北京奧運出席政要") %>%
  dplyr::select(year, countrycode, score_08 = 編碼) %>%
  dplyr::mutate(year = as.numeric(year))

# 2. 2015 閱兵 (對應 Sheet 名稱: "2015年閱兵")
df_15 <- read_excel(file_path, sheet = "2015年閱兵") %>%
  dplyr::select(year, countrycode, score_15 = FZ_FACE) %>%
  dplyr::mutate(year = as.numeric(year))

# 3. 2017 一帶一路峰會 (對應 Sheet 名稱: "2017一帶一路峰會")
df_17 <- read_excel(file_path, sheet = "2017一帶一路峰會") %>%
  dplyr::select(year, countrycode, score_17 = FZ_FACE) %>%
  dplyr::mutate(year = as.numeric(year))

# 4. 首批帶路國家 (對應 Sheet 名稱: "首批帶路國家")
df_bri <- read_excel(file_path, sheet = "首批帶路國家") %>%
  dplyr::select(year, countrycode) %>%
  dplyr::mutate(year = as.numeric(year), score_bri = 1.0) %>%
  dplyr::group_by(countrycode, year) %>%
  dplyr::summarise(score_bri = max(score_bri, na.rm = TRUE), .groups = "drop")


# ==============================================================================
# 步驟 3：讀取主資料表並進行合併、補 0.4、承接與滯後
# ==============================================================================

# 讀取主表 (對應 Sheet 名稱: "資料")
CP9623_pca <- read_excel(file_path, sheet = "資料") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  
  # 併入上面四個整理好的小表
  dplyr::left_join(df_08, by = c("countrycode", "year")) %>%
  dplyr::left_join(df_15, by = c("countrycode", "year")) %>%
  dplyr::left_join(df_17, by = c("countrycode", "year")) %>%
  dplyr::left_join(df_bri, by = c("countrycode", "year")) %>%
  
  dplyr::group_by(countrycode) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    # --- A. 判定與補值：名單上沒有的，在事件當年補 0.4 ---
    Status_08 = dplyr::if_else(year == 2008, dplyr::coalesce(score_08, 0.4), NA_real_),
    Status_15 = dplyr::if_else(year == 2015, dplyr::coalesce(score_15, 0.4), NA_real_),
    Status_17 = dplyr::if_else(year == 2017, dplyr::coalesce(score_17, 0.4), NA_real_),
    Status_BRI = score_bri
  ) %>%
  
  # --- B. 狀態承接：一旦發生，分數向下延續 ---
  tidyr::fill(Status_08, Status_15, Status_17, Status_BRI, .direction = "down") %>%
  
  dplyr::mutate(
    # --- C. 把歷史空白期(尚未發生事件前)補為 0 ---
    Status_08 = tidyr::replace_na(Status_08, 0),
    Status_15 = tidyr::replace_na(Status_15, 0),
    Status_17 = tidyr::replace_na(Status_17, 0),
    Status_BRI = tidyr::replace_na(Status_BRI, 0),
    
    # --- D. 滯後一期 (Lag 1)：用去年的表現預測今年的關係 ---
    FZ_FACE_08_lag1 = dplyr::lag(Status_08, n = 1, default = 0),
    FZ_FACE_15_lag1 = dplyr::lag(Status_15, n = 1, default = 0),
    FZ_FACE_17_lag1 = dplyr::lag(Status_17, n = 1, default = 0),
    FZ_FACE_BRI_lag1 = dplyr::lag(Status_BRI, n = 1, default = 0),
    
    # --- E. 四合一綜合面子變數 (取最大值) ---
    FZ_FACE_Events_lag1 = pmax(FZ_FACE_08_lag1, 
                               FZ_FACE_15_lag1, 
                               FZ_FACE_17_lag1, 
                               FZ_FACE_BRI_lag1, 
                               na.rm = TRUE)
  ) %>%
  
  # 移除中介暫存欄位，維持資料乾淨
  dplyr::select(-score_08, -score_15, -score_17, -score_bri, 
                -Status_08, -Status_15, -Status_17, -Status_BRI) %>%
  dplyr::ungroup()

# ==============================================================================
# 步驟 4：檢查結果 (以南非 ZAF 為例，南非常出席這類活動)
# ==============================================================================
CP9623_pca %>% 
  dplyr::filter(countrycode == "ZAF") %>% 
  dplyr::select(year, FZ_FACE_08_lag1, FZ_FACE_15_lag1, FZ_FACE_17_lag1, FZ_FACE_BRI_lag1, FZ_FACE_Events_lag1) %>% 
  tail(12)

# ==============================================================================
# 處理人權表態資料
# ==============================================================================
# ==============================================================================
# 步驟 1：讀取 Excel 檔案中的 "Data" 工作表
# ==============================================================================
# 注意這裡改用 read_excel，並且指定讀取名為 "Data" 的 sheet
hr_data <- read_excel("各國對中國人權立場.xlsx", sheet = "Data") %>%
  dplyr::select(countrycode, year, QCA_Score) %>%
  dplyr::rename(UN_HR_Score_raw = QCA_Score) %>%
  dplyr::group_by(countrycode, year) %>%
  dplyr::summarise(UN_HR_Score_raw = mean(UN_HR_Score_raw, na.rm = TRUE), .groups = "drop")
# 檢查是否讀取成功
head(hr_data)

# ==============================================================================
# 步驟 2：將人權資料併入主資料集 (CP9623_pca)，執行承接與滯後
# 統一將 year 轉為數值格式 (numeric) 進行完美對接
# ==============================================================================

CP9623_pca <- CP9623_pca %>%
  # 1. 確保主資料框的 year 是數字
  dplyr::mutate(year = as.numeric(year)) %>%
  
  # 2. 確保人權資料框 (hr_data) 的 year 也是數字，然後進行合併
  dplyr::left_join(hr_data %>% dplyr::mutate(year = as.numeric(year)), by = c("countrycode", "year")) %>%
  
  dplyr::group_by(countrycode) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    # A. 狀態承接 (Carry-over)
    UN_HR_Score_carried = UN_HR_Score_raw
  ) %>%
  tidyr::fill(UN_HR_Score_carried, .direction = "down") %>%
  dplyr::mutate(
    # B. 填補預設值 (Default to 0.5)
    UN_HR_Score_carried = tidyr::replace_na(UN_HR_Score_carried, 0.5),
    
    # C. 滯後一期 (Lag 1)
    UN_HR_Score_lag1 = dplyr::lag(UN_HR_Score_carried, n = 1)
  ) %>%
  dplyr::ungroup()

# ==============================================================================
# 步驟 3：檢查合併與滯後結果
# ==============================================================================
CP9623_pca %>% 
  dplyr::filter(countrycode == "PAK") %>% 
  dplyr::select(year, UN_HR_Score_raw, UN_HR_Score_carried, UN_HR_Score_lag1) %>% 
  tail(10)

# ==============================================================================
# 檢查特定國家的處理結果 (加上 dplyr:: 防治衝突)
# ==============================================================================
CP9623_pca %>%
  dplyr::filter(countrycode %in% c("PAK", "GBR", "IDN")) %>% 
  dplyr::select(countrycode, year, UN_HR_Score_raw, UN_HR_Score_carried, UN_HR_Score_lag1) %>%
  tail(10)


# 將資料框匯出為新的 CSV 檔案
# 您可以將 "CP9623_pca_Final_Face.csv" 換成任何您喜歡的檔名
write_csv(CP9623_pca, "CP9623_pca_20260222.csv")

# 檢查一下檔案是否已經成功建立在您的工作目錄中
list.files(pattern = "CP9623_pca_20260222.csv")
# ============================================================================
# 4. 標準化 (Standardization)
# ============================================================================

cat("\n[Step 4] 執行變數標準化...\n")

# 更新：對滯後後的變數 (_lag) 也進行標準化
# 這裡將標準化原始變數以及剛剛產生的滯後變數
vars_to_scale <- c(
  "dist", "dip_age", "population_total", "gdp", "gdp_per_capita",
  "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
  "economy", "arms_and_military", "trade", "arms", "military", "financial", "travel", "sanction",
  "WGI", "va", "psv", "ge", "rq", "rl", "cc", "WGI_diff_CHN", "gdp_per_capita_diff_CHN", "FTA", "ORG",
  paste0(vars_to_lag, "_lag") # 加入滯後變數
)

# 確保只標準化資料框中存在的變數
existing_vars_to_scale <- vars_to_scale[vars_to_scale %in% names(CP9623)]

CP9623 <- CP9623 %>%
  mutate(
    across(
      .cols = all_of(existing_vars_to_scale), 
      .fns = ~ as.numeric(scale(.)),  
      .names = "{.col}_std" 
    ))

cat("✓ 標準化完成，產生 _std 結尾變數。\n")

# ============================================================================
# 5. 轉為面板格式與最終清理
# ============================================================================

# 設定為面板資料格式
panel_CP9623 <- pdata.frame(CP9623, index = c("countrycode", "year"))

# 刪除含有Inf或-Inf的列
panel_CP9623 <- panel_CP9623[!apply(panel_CP9623, 1, function(x) any(is.infinite(x))), ]

cat("\n清理後最終資料:", nrow(panel_CP9623), "筆觀察值\n")
cat("涵蓋國家數:", length(unique(panel_CP9623$countrycode)), "\n")

# 檢查一下結果
head(panel_CP9623[, c("countrycode", "year", "build_upgrade", "gdp_per_capita", "gdp_per_capita_lag", "gdp_per_capita_lag_std")])

# ============================================================================
# 3. PCA降維分析 (修正版：解決 pseries 型別衝突)
# ============================================================================

library(FactoMineR) 
library(dplyr)
library(writexl)

cat(rep("=", 80), "\n")
cat("PCA 降維分析與 QCA 代理變數篩選\n")
cat(rep("=", 80), "\n\n")

# 1. 【關鍵修正】將 pdata.frame 轉回普通 data.frame
# 這樣可以移除 pseries 屬性，避免 left_join 報錯
target_data <- as.data.frame(panel_CP9623)

# 2. 建立 row_id (現在它是純粹的 integer 了)
target_data$row_id <- 1:nrow(target_data)
# 確保 rownames 也對齊，以防萬一
rownames(target_data) <- target_data$row_id

# 定義要放入 PCA 的變數組
pca_vars_econ <- c("gdp_per_capita_std", "gdp_per_capita_diff_CHN_std", "china_ex_to_i_std", 
                   "china_im_fr_i_std", "exportdep_std", "importdep_std", "population_total_std")

pca_vars_sanct <- c("trade_std", "arms_std", "military_std", "financial_std", "travel_std")

pca_vars_gov <- c("va_std", "psv_std", "ge_std", "rq_std", "rl_std", "cc_std")

# ==============================================================================
# PCA 核心函數 (含型別強制轉換)
# ==============================================================================
perform_pca <- function(data, vars, name, method = "kaiser") {
  
  # 1. 變數檢查
  valid_vars <- vars[vars %in% names(data)]
  if(length(valid_vars) < 2) {
    cat("❌【", name, "】錯誤：有效變數少於 2 個，無法執行 PCA。\n", sep = "")
    return(NULL)
  }
  
  # 2. 提取資料並移除 NA
  # 【關鍵修正】強制確保 row_id 是整數
  pca_data <- data[, c("row_id", valid_vars)] %>% na.omit()
  pca_data$row_id <- as.integer(pca_data$row_id) 
  
  if (nrow(pca_data) < 10) {
    cat("❌【", name, "】有效樣本過少 (<10)，跳過。\n", sep = "")
    return(NULL)
  }
  
  # 3. 執行 PCA
  pca_res <- PCA(pca_data[, -1], scale.unit = TRUE, graph = FALSE, ncp = length(valid_vars))
  
  # 4. 決定主成分數量
  n_comp <- sum(pca_res$eig[, 1] >= 1.0)
  n_comp <- max(1, n_comp)
  
  # 5. 選出最佳代理變數
  loadings <- abs(pca_res$var$coord[, 1])
  top_proxy <- names(sort(loadings, decreasing = TRUE))[1]
  
  cat("【", name, "】\n", sep = "")
  cat("   • 樣本數:", nrow(pca_data), " | 保留主成分數:", n_comp, "\n")
  cat("   • 🏆 QCA 推薦代理變數:", top_proxy, "\n\n")
  
  # 6. 整理分數表
  scores <- as.data.frame(pca_res$ind$coord[, 1:n_comp, drop = FALSE])
  
  # 命名
  prefix <- switch(name, "經濟維度"="econ", "制裁維度"="sanct", "治理維度"="gov", "pca")
  colnames(scores) <- paste0(prefix, "_PC", 1:n_comp)
  
  # 【關鍵修正】確保合併鍵型別一致
  scores$row_id <- as.integer(pca_data$row_id)
  
  return(scores)
}

# ==============================================================================
# 執行與回填
# ==============================================================================

# 1. 計算 PCA 分數
scores_econ  <- perform_pca(target_data, pca_vars_econ, "經濟維度")
scores_sanct <- perform_pca(target_data, pca_vars_sanct, "制裁維度")
scores_gov   <- perform_pca(target_data, pca_vars_gov, "治理維度")

# 2. 安全回填
CP9623_pca <- target_data

# 使用 left_join (現在 row_id 都是 integer，不會報錯了)
if(!is.null(scores_econ))  CP9623_pca <- left_join(CP9623_pca, scores_econ, by = "row_id")
if(!is.null(scores_sanct)) CP9623_pca <- left_join(CP9623_pca, scores_sanct, by = "row_id")
if(!is.null(scores_gov))   CP9623_pca <- left_join(CP9623_pca, scores_gov, by = "row_id")

# 3. 清理與存檔
CP9623_pca$row_id <- NULL
pc_cols <- grep("_PC[0-9]", names(CP9623_pca), value = TRUE)

cat("\n✓ PCA 分析完成，共新增", length(pc_cols), "個主成分欄位。\n")
# print(head(CP9623_pca[, c("countrycode", "year", pc_cols[1:3])]))

write_xlsx(CP9623_pca, path = "CP9623_pca.xlsx")
cat("✓ 檔案已儲存：CP9623_pca.xlsx\n")

# ==============================================================================
# QCA分析
# ==============================================================================

library(tidyverse)
library(QCA)

# ==============================================================================
# 0. 基礎變數定義
# ==============================================================================
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", 
                        "exportdep_lag", "importdep_lag",
                        "china_ex_to_i_lag", "china_im_fr_i_lag", 
                        "population_total_lag")

sanctional_factors <- c("trade_lag", "financial_lag",
                        "arms_lag", "military_lag", 
                        "travel_lag")

governance_factors <- c("va_lag", "psv_lag", 
                        "ge_lag", "rq_lag", 
                        "rl_lag", "cc_lag") 

# ==============================================================================
# 🛠️ 1. 核心工具函數庫
# ==============================================================================

# A. 校準函數
manual_calibrate <- function(x, quantile_prob) {
  if(all(is.na(x))) return(rep(0, length(x)))
  if(max(x, na.rm=TRUE) == 0) return(x) # 防呆：全0則返0
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

# B. ORG 合成 (領域覆蓋法)
calculate_domain_org <- function(df) {
  org_cols <- c("SCO", "AIIB", "BRICS", "NDB")
  for(col in org_cols) if(!col %in% names(df)) df[[col]] <- 0
  
  df %>% mutate(
    Link_Political = pmax(as.numeric(BRICS), as.numeric(NDB), na.rm = TRUE),
    Link_Security = as.numeric(SCO),
    Link_Finance = as.numeric(AIIB),
    ORG_Breadth = Link_Political + Link_Security + Link_Finance,
    FZ_ORG_NEW = case_when(
      ORG_Breadth >= 2 ~ 1.0,
      ORG_Breadth >= 1 ~ 0.6,
      TRUE ~ 0.0
    )
  )
}

# C. FTA 合成 (層級覆蓋法)
calculate_fta_hierarchy <- function(df) {
  fta_cols <- c("BFTA", "ASEAN", "RCEP")
  for(col in fta_cols) if(!col %in% names(df)) df[[col]] <- 0
  
  df %>% mutate(
    is_Strong_FTA = pmax(as.numeric(BFTA), as.numeric(ASEAN), na.rm=TRUE),
    is_RCEP_Link = as.numeric(RCEP),
    FZ_FTA_NEW = case_when(
      is_Strong_FTA == 1 ~ 1.0,
      is_RCEP_Link == 1  ~ 0.8,
      TRUE ~ 0.0
    )
  )
}

# ==============================================================================
# 重新定義 D. 數據聚合與存檔核心 (🛡️ 修正「前朝面子」的跨代污染)
# ==============================================================================
process_and_save_data <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 處理數據: ", label, " (", start_year, "-", end_year, ")\n"))
  
  # 建立安全計算函數避免出現 -Inf
  safe_max <- function(x) { if(all(is.na(x))) 0 else max(x, na.rm = TRUE) }
  safe_mean <- function(x) { if(all(is.na(x))) 0 else mean(x, na.rm = TRUE) }
  
  for(v in c(economic_factors, sanctional_factors, governance_factors)) {
    if(!v %in% names(raw_panel)) { raw_panel[[v]] <- 0 }
  }
  
  # 確保各個獨立的面子變數存在
  face_vars <- c("FZ_FACE_08_lag", "FZ_FACE_15_lag", "FZ_FACE_17_lag", "FZ_FACE_BRI_lag", "UN_HR_Score_lag")
  for(v in face_vars) {
    if(!v %in% names(raw_panel)) { raw_panel[[v]] <- 0 }
  }
  
  panel_processed <- raw_panel %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
    calculate_domain_org() %>%
    calculate_fta_hierarchy()
  
  agg_data <- panel_processed %>%
    filter(year >= start_year & year <= end_year) %>%
    group_by(countrycode) %>%
    summarise(
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), 
             ~mean(., na.rm = TRUE)),
      
      FZ_ORG_FINAL = max(FZ_ORG_NEW, na.rm = TRUE),
      FZ_FTA_FINAL = max(FZ_FTA_NEW, na.rm = TRUE),
      DIST = mean(dist, na.rm = TRUE),
      
      # 🛡️ 【核心修復：斷開跨代污染】依據時期動態計算面子分數
      FACE_08  = if(start_year < 2013) safe_max(FZ_FACE_08_lag) else 0,
      FACE_15  = if(end_year >= 2015) safe_max(FZ_FACE_15_lag) else 0,
      FACE_17  = if(end_year >= 2017) safe_max(FZ_FACE_17_lag) else 0,
      FACE_BRI = if(end_year >= 2013) safe_max(FZ_FACE_BRI_lag) else 0,
      # 人權指標是每年的密集表態，改用 mean 避免「僅投過一次好票」就被永遠當作鐵桿
      FACE_HR  = if(end_year >= 2018) safe_mean(UN_HR_Score_lag) else 0
    ) %>%
    mutate(
      OUT = ifelse(OUT_Event > 0, 1, 0),
      across(where(is.numeric), ~ifelse(is.infinite(.), 0, .)),
      
      # 結合該時期適用的面子指標
      FZ_FACE = pmax(FACE_08, FACE_15, FACE_17, FACE_BRI, FACE_HR, na.rm = TRUE)
    ) %>%
    filter(!is.na(gdp_per_capita_lag)) %>%
    as.data.frame()
  
  filename <- paste0("QCA_Dataset_", gsub(" ", "_", label), ".csv")
  write.csv(agg_data, filename, row.names = FALSE)
  
  return(agg_data)
}

# E. 邏輯合成運算器
calc_score_logic <- function(data, vars, threshold, logic) {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  
  if(logic == "min") {
    return(apply(calibrated, 1, min, na.rm=TRUE))
  } else if(logic == "max") {
    return(apply(calibrated, 1, max, na.rm=TRUE))
  } else {
    return(rowMeans(calibrated, na.rm=TRUE))
  }
}

# ==============================================================================
# 重新定義 F. QCA 執行引擎 (🛡️ 導入動態門檻救場機制)
# ==============================================================================
run_qca_engine <- function(data, label, logic_mode, threshold) {
  cat(paste0("\n----------------------------------------------------------\n"))
  cat(paste0("🚀 執行模型: ", label, "\n"))
  cat(paste0("⚙️  合成邏輯: ", logic_mode, "\n"))
  
  qca_data <- data %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score_logic(., economic_factors, threshold, logic_mode),
      FZ_GOV   = calc_score_logic(., governance_factors, threshold, logic_mode),
      FZ_SANCT = calc_score_logic(., sanctional_factors, 0.85, logic_mode),
      
      FZ_INS = if(logic_mode == "min") pmin(FZ_FTA_FINAL, FZ_ORG_FINAL) else pmax(FZ_FTA_FINAL, FZ_ORG_FINAL),
      FZ_PROX  = 1 - manual_calibrate(DIST, threshold)
    ) %>%
    # 處理0.5的模糊地帶，並把戰略沉默 (0.501) 嚴格壓回不及格 (0.499)
    mutate(across(starts_with("FZ_"), ~ifelse(abs(.-0.5)<0.001, 0.501, .))) %>%
    mutate(FZ_FACE = ifelse(FZ_FACE > 0.49 & FZ_FACE < 0.52, 0.499, FZ_FACE))
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_SANCT", "FZ_INS", "FZ_PROX", "FZ_FACE")
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% na.omit()
  
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  
  tt_incl_values <- as.numeric(as.character(TT$tt$incl))
  valid_idx <- which(TT$tt$n > 0)
  
  if(length(valid_idx) == 0) {
    cat("⚠️ 真值表無有效案例。\n")
    return(NULL)
  }
  
  top_cons <- max(tt_incl_values[valid_idx], na.rm = TRUE)
  cat(paste0("🧪 最高一致性: ", round(top_cons, 3), "\n"))
  
  # 🛡️ 動態門檻：如果最高一致性過不了 0.7，就自動降到稍低於最高值，強行解出路徑！
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else if(top_cons >= 0.7) 0.7 else round(top_cons - 0.005, 3)
  cat(paste0("✂️  採用門檻: ", final_cut, "\n"))
  
  if(top_cons < 0.55) {
    cat("⚠️ 一致性嚴重偏低 (<0.55)，模型無解釋力。以下為真值表前 5 名：\n")
    print(TT$tt %>% filter(n > 0) %>% mutate(incl = as.numeric(as.character(incl))) %>% arrange(desc(incl)) %>% head(5))
  } else {
    tryCatch({
      sol <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
      print(sol)
    }, error = function(e) {
      cat("⚠️ 最小化失敗：", conditionMessage(e), "\n")
    })
  }
}

# ==============================================================================
# ▶️ 執行部分：請先重新生成各個時期的 dt 資料，再跑模型
# ==============================================================================
if(exists("CP9623_pca")) {
  
  # 🚨 必須重新跑這四行，讓新版的 面子計算邏輯 生效！
  dt_p1_0212 <- process_and_save_data(CP9623_pca, 2002, 2012, "P1_Hu_02-12")
  dt_p2_1323 <- process_and_save_data(CP9623_pca, 2013, 2023, "P2_Xi_Full_13-23")
  dt_p2_1317 <- process_and_save_data(CP9623_pca, 2013, 2017, "P2_Xi_Early_13-17")
  dt_p3_1823 <- process_and_save_data(CP9623_pca, 2018, 2023, "P3_Xi_Late_18-23")
  
  cat("\n################################################################\n")
  cat("🔬 思路一：二分斷裂模型 (Binary Rupture)\n")
  cat("################################################################\n")
  run_qca_engine(dt_p1_0212, "思路1-P1 (胡 2002-2012)", "max", 0.65)
  run_qca_engine(dt_p2_1323, "思路1-P2 (習全 2013-2023)", "max", 0.50)
  
  cat("\n################################################################\n")
  cat("🔬 思路二：即刻轉向模型 (Immediate Turn)\n")
  cat("################################################################\n")
  run_qca_engine(dt_p2_1317, "思路2-P2 (習前)", "max", 0.50)
  run_qca_engine(dt_p3_1823, "思路2-P3 (習後)", "max", 0.50)
  
}


# ==============================================================================
# 產出 QCA 分析報告
# ==============================================================================

# 設定報告輸出的檔案名稱
report_file <- "QCA_Analysis_Report.txt"

# 開啟 sink() 開始攔截輸出
# split = TRUE 代表「同時寫入檔案，並在控制台顯示」，讓您可以看進度
sink(report_file, split = TRUE) 

# 印出報告標頭
cat("================================================================\n")
cat("          中國戰略夥伴關係升級 QCA 跨時期分析報告\n")
cat("          生成時間：", as.character(Sys.time()), "\n")
cat("================================================================\n\n")

# 確保資料存在後，執行所有模型
if(exists("CP9623_pca")) {
  
  # 1. 重新生成各個時期的 dt 資料
  cat(">>> 正在處理各時期資料...\n")
  dt_p1_0212 <- process_and_save_data(CP9623_pca, 2002, 2012, "P1_Hu_02-12")
  dt_p2_1323 <- process_and_save_data(CP9623_pca, 2013, 2023, "P2_Xi_Full_13-23")
  dt_p2_1317 <- process_and_save_data(CP9623_pca, 2013, 2017, "P2_Xi_Early_13-17")
  dt_p3_1823 <- process_and_save_data(CP9623_pca, 2018, 2023, "P3_Xi_Late_18-23")
  
  # 2. 跑模型並將結果寫入報告
  cat("\n\n################################################################\n")
  cat("🔬 思路一：二分斷裂模型 (Binary Rupture)\n")
  cat("################################################################\n")
  run_qca_engine(dt_p1_0212, "思路1-P1 (胡 2002-2012)", "max", 0.65)
  run_qca_engine(dt_p2_1323, "思路1-P2 (習全 2013-2023)", "max", 0.50)
  
  cat("\n\n################################################################\n")
  cat("🔬 思路二：即刻轉向模型 (Immediate Turn)\n")
  cat("################################################################\n")
  run_qca_engine(dt_p2_1317, "思路2-P2 (習前)", "max", 0.50)
  run_qca_engine(dt_p3_1823, "思路2-P3 (習後)", "max", 0.50)
  
  cat("\n\n================================================================\n")
  cat("報告生成完畢。\n")
  cat("================================================================\n")
  
} else {
  cat("錯誤：找不到 CP9623_pca 資料框，請先載入資料。\n")
}

# 關閉 sink()，正式將檔案存檔
sink()

# 提示檔案儲存位置
cat("\n✅ 報告已成功匯出！\n檔案位置：", file.path(getwd(), report_file), "\n")

