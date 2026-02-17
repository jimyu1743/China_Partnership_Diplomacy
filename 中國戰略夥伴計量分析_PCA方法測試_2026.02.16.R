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

cat("✓ 已建立 build_upgrade 變數與所有指定變數的滯後版本 (_lag)。\n")

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
# 📦 V11.1 執行版：使用預處理滯後變數 (_lag) + 中位數戰略
#    特點：
#    1. [變數切換] 全面改用 *_lag 變數，移除 _std 後綴。
#    2. [邏輯簡化] 移除函數內的 lag() 操作，直接依賴資料源的滯後欄位。
#    3. [雙軌分析] P1 (Min/0.65) 與 P2 (Mean/0.50) 策略保持不變。
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 變數定義 (修改為 _lag 版本)
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", "china_ex_to_i_lag", 
                        "china_im_fr_i_lag", "population_total_lag")
sanctional_factors <- c("trade_lag", "arms_lag", "financial_lag")
governance_factors <- c("rl_lag", "ge_lag", "cc_lag", "va_lag") 

# 2. 核心數據處理函數 (直接取用 _lag 變數)
# ------------------------------------------------------------------
process_data_v11_lagged <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 正在處理: ", label, " (", start_year, "-", end_year, ")\n"))
  
  # 檢查關鍵變數是否存在
  if(!all(c(economic_factors, sanctional_factors, governance_factors) %in% names(raw_panel))) {
    stop("❌ 錯誤：找不到指定的 *_lag 變數。請檢查資料框欄位名稱。")
  }
  
  agg_data <- raw_panel %>%
    filter(as.numeric(as.character(year)) >= start_year & 
             as.numeric(as.character(year)) <= end_year) %>%
    group_by(countrycode) %>%
    summarise(
      # 結果變數 (Outcome)：取最大值 (是否升級)
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      # 條件變數 (Condition)：直接取 _lag 變數的平均值
      # 意義：該時期的「平均滯後表現」(即升級前一年的平均狀態)
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), ~mean(., na.rm = TRUE)),
      
      # 制度變數 (通常不需要滯後，或已經在原始資料處理過)
      # 假設 FTA 和 ORG 在原始資料中也是年度狀態
      FTA = max(as.numeric(FTA), na.rm = TRUE),
      ORG = max(as.numeric(ORG), na.rm = TRUE)
    ) %>%
    # 清理無限值與 NA
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .))) %>%
    filter(!is.na(gdp_per_capita_lag)) %>% # 確保核心經濟數據存在
    mutate(OUT = ifelse(OUT_Event > 0, 1, 0)) %>%
    as.data.frame()
  
  return(agg_data)
}

# 3. 校準與分析函數 (保持 V11.0 邏輯)
# ------------------------------------------------------------------
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

calc_score <- function(data, vars, threshold, logic="mean") {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  
  if(logic == "min") {
    score <- apply(calibrated, 1, min, na.rm=TRUE)
  } else {
    score <- rowMeans(calibrated, na.rm=TRUE)
  }
  ifelse(abs(score-0.5) < 0.001, 0.501, score)
}

print_solution_cases <- function(sol, data) {
  if(is.null(sol$pims)) return()
  cat("\n🔍 [路徑案例解碼]\n")
  for(path in colnames(sol$pims)) {
    cases_idx <- which(sol$pims[, path] > 0.5 & data$OUT > 0.5)
    cat(paste0("👉 路徑 [", path, "]: "))
    if(length(cases_idx) > 0) {
      cn <- data$countrycode[cases_idx]
      cat(paste(if(length(cn)>15) paste(head(cn, 15), collapse=", ") else paste(cn, collapse=", "), "\n"))
    } else cat("(無典型案例)\n")
  }
}

# 4. 執行雙軌分析 (使用 _lag 變數)
# ------------------------------------------------------------------

# 載入檢查
if(!exists("CP9623_pca")) stop("❌ 請先載入 CP9623_pca")

# 4.1 數據生成
data_p1_lag <- process_data_v11_lagged(CP9623_pca, 2002, 2012, "P1 江胡時期")
data_p2_lag <- process_data_v11_lagged(CP9623_pca, 2013, 2023, "P2 習近平時期")

# 4.2 P1 分析 (菁英模式: Min Logic, Cut=0.65)
# --------------------------------------------------------
cat("\n==========================================================\n")
cat("🚀 [V11.1] P1 江胡時期：菁英選拔 (Min Logic)\n")

qca_p1 <- data_p1_lag %>% ungroup() %>%
  mutate(
    FZ_ECON  = calc_score(., economic_factors, 0.65, "min"),
    FZ_SANCT = calc_score(., sanctional_factors, 0.85, "min"),
    FZ_GOV   = calc_score(., governance_factors, 0.65, "min"),
    FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
    FZ_ORG   = manual_calibrate(as.numeric(ORG), 0.65)
  ) %>% 
  dplyr::select(countrycode, OUT, starts_with("FZ_")) %>% # ✨ 這裡加上 dplyr::
  na.omit() %>% as.data.frame()

TT_p1 <- truthTable(qca_p1, outcome = "OUT", conditions = names(qca_p1)[grep("FZ_", names(qca_p1))], sort.by = "incl")

tryCatch({
  SOL_p1 <- minimize(TT_p1, include = "?", details = TRUE, incl.cut = 0.75)
  print(SOL_p1)
  print_solution_cases(SOL_p1, qca_p1)
}, error = function(e) cat("⚠️ P1 求解受阻，可能因變數替換導致分布改變。\n"))


# 4.3 P2 分析 (中位數模式: Mean Logic, Cut=0.50, No Sanctions)
# --------------------------------------------------------
cat("\n==========================================================\n")
cat("🚀 [V11.1] P2 習近平時期：中位數戰略 (Mean Logic, Cut=0.50)\n")

qca_p2 <- data_p2_lag %>% ungroup() %>%
  mutate(
    FZ_ECON  = calc_score(., economic_factors, 0.50, "mean"), 
    FZ_GOV   = calc_score(., governance_factors, 0.50, "mean"), 
    FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
    FZ_ORG   = manual_calibrate(as.numeric(ORG), 0.50)
  ) %>% 
  dplyr::select(countrycode, OUT, FZ_ECON, FZ_GOV, FZ_FTA, FZ_ORG) %>% # ✨ 這裡也加上 dplyr::
  na.omit() %>% as.data.frame()

# 檢查必要性 (觀察 ~FZ_GOV)
cat("\n   [必要性快篩]\n")
print(superSubset(qca_p2, outcome = "OUT", incl.cut = 0.80))

TT_p2 <- truthTable(qca_p2, outcome = "OUT", conditions = c("FZ_ECON", "FZ_GOV", "FZ_FTA", "FZ_ORG"), sort.by = "incl")
top_cons_p2 <- max(TT_p2$tt$incl[TT_p2$tt$n > 0], na.rm=TRUE)
cat(paste0("\n   🧪 P2 最高一致性: ", round(top_cons_p2, 3), "\n"))

final_cut <- if(top_cons_p2 >= 0.75) 0.75 else 0.72
cat(paste0("   ✂️ 採用一致性門檻: ", final_cut, "\n"))

SOL_p2 <- minimize(TT_p2, include = "?", details = TRUE, incl.cut = final_cut)
print(SOL_p2)
print_solution_cases(SOL_p2, qca_p2)

# ==============================================================================
# 🏥 V11.2 P2 最終診斷工具：直視混亂 (Truth Table Inspection)
#    目標：不強求跑出解，而是檢查 P2 的真值表到底長什麼樣。
# ==============================================================================

diagnose_p2_chaos <- function(data_p2_lag) {
  cat("\n🔍 [P2 深度診斷] 檢查真值表的一致性分布\n")
  
  # 1. 準備數據
  qca_p2 <- data_p2_lag %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score(., economic_factors, 0.50, "mean"), 
      FZ_GOV   = calc_score(., governance_factors, 0.50, "mean"), 
      FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG   = manual_calibrate(as.numeric(ORG), 0.50)
    ) %>% 
    dplyr::select(countrycode, OUT, FZ_ECON, FZ_GOV, FZ_FTA, FZ_ORG) %>% 
    na.omit() %>% as.data.frame()
  
  # 2. 生成真值表 (不設門檻，全部顯示)
  TT <- truthTable(qca_p2, outcome = "OUT", 
                   conditions = c("FZ_ECON", "FZ_GOV", "FZ_FTA", "FZ_ORG"), 
                   sort.by = "incl", show.cases = TRUE)
  
  # 3. 印出真值表的前 10 行 (一致性最高的那些)
  print(TT)
  
  # 4. 統計分析
  incl_scores <- as.numeric(as.character(TT$tt$incl))
  cat("\n📊 統計摘要：\n")
  cat(paste0("   最高一致性: ", max(incl_scores, na.rm=TRUE), "\n"))
  cat(paste0("   平均一致性: ", mean(incl_scores, na.rm=TRUE), "\n"))
  
  # 5. 找出那些「高分但沒過門檻」的案例
  cat("\n🧐 哪些組態的一致性最高？(但可能仍 < 0.75)\n")
  top_rows <- TT$tt[1:min(5, nrow(TT$tt)), ]
  print(top_rows)
}

# 執行診斷
if(exists("data_p2_lag")) diagnose_p2_chaos(data_p2_lag)

# ==============================================================================
# 📦 V13.1 修復版：防崩潰數值轉換 (Crash-Proof Numeric Conversion)
#    目標：徹底解決 round() 報錯，確保跑完所有時期。
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 變數定義 (保持不變)
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", "china_ex_to_i_lag", 
                        "china_im_fr_i_lag", "population_total_lag")
sanctional_factors <- c("trade_lag", "arms_lag", "financial_lag")
governance_factors <- c("rl_lag", "ge_lag", "cc_lag", "va_lag") 

# 2. 數據處理 (保持不變)
process_period_data <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 處理數據區間: ", label, " (", start_year, "-", end_year, ")\n"))
  agg_data <- raw_panel %>%
    filter(as.numeric(as.character(year)) >= start_year & 
             as.numeric(as.character(year)) <= end_year) %>%
    group_by(countrycode) %>%
    summarise(
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), ~mean(., na.rm = TRUE)),
      FTA = max(as.numeric(FTA), na.rm = TRUE),
      ORG = max(as.numeric(ORG), na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .))) %>%
    filter(!is.na(gdp_per_capita_lag)) %>%
    mutate(OUT = ifelse(OUT_Event > 0, 1, 0)) %>%
    as.data.frame()
  return(agg_data)
}

# 3. 校準與分析 (保持不變)
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

calc_score <- function(data, vars, threshold, logic="mean") {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  if(logic == "min") {
    score <- apply(calibrated, 1, min, na.rm=TRUE)
  } else {
    score <- rowMeans(calibrated, na.rm=TRUE)
  }
  ifelse(abs(score-0.5) < 0.001, 0.501, score)
}

print_solution_cases <- function(sol, data) {
  if(is.null(sol$pims)) return()
  cat("\n🔍 [典型案例解碼]\n")
  for(path in colnames(sol$pims)) {
    cases_idx <- which(sol$pims[, path] > 0.5 & data$OUT > 0.5)
    cat(paste0("👉 路徑 [", path, "]: "))
    if(length(cases_idx) > 0) {
      cn <- data$countrycode[cases_idx]
      cat(paste(if(length(cn)>15) paste(head(cn, 15), collapse=", ") else paste(cn, collapse=", "), "\n"))
    } else cat("(無典型案例)\n")
  }
}

# 4. 執行引擎 (V13.1 修復版)
# --------------------------------------------------------
run_qca_analysis <- function(flat_data, label, logic_type, threshold, use_sanctions=FALSE) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0("🚀 分析: ", label, "\n"))
  cat(paste0("   設定: Logic=", logic_type, " | Cut=", threshold, " | Sanctions=", use_sanctions, "\n"))
  
  qca_data <- flat_data %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score(., economic_factors, threshold, logic_type),
      FZ_GOV   = calc_score(., governance_factors, threshold, logic_type),
      FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG   = manual_calibrate(as.numeric(ORG), threshold)
    )
  
  if(use_sanctions) {
    qca_data <- qca_data %>% 
      mutate(FZ_SANCT = calc_score(., sanctional_factors, 0.85, logic_type))
  }
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_FTA", "FZ_ORG")
  if(use_sanctions) sel_vars <- c(sel_vars, "FZ_SANCT")
  
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% 
    na.omit() %>% as.data.frame()
  
  # B. 必要性快篩
  cat("\n   [必要性快篩 (Incl > 0.85)]\n")
  try({ print(superSubset(qca_final, outcome = "OUT", incl.cut = 0.85)) })
  
  # C. 真值表
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  
  # ✨ [關鍵修復]：強制轉換 incl 為數值，並移除 NA
  raw_incl <- as.numeric(as.character(TT$tt$incl[TT$tt$n > 0]))
  raw_incl <- raw_incl[!is.na(raw_incl)]
  
  top_cons <- if(length(raw_incl) > 0) max(raw_incl) else 0
  cat(paste0("\n   🧪 最高一致性: ", round(top_cons, 3), "\n"))
  
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else 0.70
  if(top_cons < 0.72) {
    cat("   ⚠️ 警告：一致性過低 (<0.72)，結果可能不可靠。\n")
    # 如果真的太低，我們就不跑 minimize 了，直接印出真值表前幾行給您看
    print(head(TT$tt))
    return(NULL)
  }
  cat(paste0("   ✂️ 採用一致性門檻: ", final_cut, "\n"))
  
  # D. 求解
  tryCatch({
    SOL <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
    print(SOL)
    print_solution_cases(SOL, qca_final)
  }, error = function(e) cat("   ⚠️ 無法求解 (No Solution)。\n"))
}

# ==============================================================================
# 5. 啟動 (One-Click Run)
# ==============================================================================
if(!exists("CP9623_pca")) stop("❌ 請先載入 CP9623_pca")

# 生成數據
dt_p1 <- process_period_data(CP9623_pca, 1997, 2012, "P1 江胡 (1997-2012)")
dt_p2 <- process_period_data(CP9623_pca, 2013, 2017, "P2 習前期 (2013-2017)")
dt_p3 <- process_period_data(CP9623_pca, 2018, 2023, "P3 習後期 (2018-2023)")

# 執行分析
# P1: 菁英模式 (Min, 0.65, 含制裁)
run_qca_analysis(dt_p1, "P1 江胡時期", "min", 0.65, use_sanctions=TRUE)

# P2: 擴張模式 (Mean, 0.50, 無制裁)
run_qca_analysis(dt_p2, "P2 習前期 (大國外交)", "mean", 0.50, use_sanctions=FALSE)

# P3: 分化模式 (Mean, 0.50, 無制裁)
run_qca_analysis(dt_p3, "P3 習後期 (戰狼外交)", "mean", 0.50, use_sanctions=FALSE)

# ==============================================================================
# 📦 V14.0 戰略機會主義測試：聯集邏輯回歸 (Union/Max Logic)
#    理論假設：中國外交升級不求「全能」，只求「一技之長」。
#    操作：
#    1. 聚合函數改回 apply(..., 1, max)。
#    2. 因為 Max 容易得分，校準門檻統一調高至 0.65 (過濾雜訊)。
#    3. 針對 P2/P3 測試是否能解釋 "無解" 的現象。
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 變數定義 (使用 _lag)
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", "china_ex_to_i_lag", 
                        "china_im_fr_i_lag", "population_total_lag")
sanctional_factors <- c("trade_lag", "arms_lag", "financial_lag")
governance_factors <- c("rl_lag", "ge_lag", "cc_lag", "va_lag") 

# 2. 數據處理 (保持不變)
process_period_data <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 處理數據區間: ", label, " (", start_year, "-", end_year, ")\n"))
  agg_data <- raw_panel %>%
    filter(as.numeric(as.character(year)) >= start_year & 
             as.numeric(as.character(year)) <= end_year) %>%
    group_by(countrycode) %>%
    summarise(
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), ~mean(., na.rm = TRUE)),
      FTA = max(as.numeric(FTA), na.rm = TRUE),
      ORG = max(as.numeric(ORG), na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .))) %>%
    filter(!is.na(gdp_per_capita_lag)) %>%
    mutate(OUT = ifelse(OUT_Event > 0, 1, 0)) %>%
    as.data.frame()
  return(agg_data)
}

# 3. 聯集校準函數 (The Union Logic)
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

# ✨ 關鍵修改：Union Proxy (Max)
calc_score_union <- function(data, vars, threshold) {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  
  # ✨ 使用 MAX (聯集)：只要有一項強，整體就強
  score <- apply(calibrated, 1, max, na.rm=TRUE)
  ifelse(abs(score-0.5) < 0.001, 0.501, score)
}

print_solution_cases <- function(sol, data) {
  if(is.null(sol$pims)) return()
  cat("\n🔍 [典型案例解碼]\n")
  for(path in colnames(sol$pims)) {
    cases_idx <- which(sol$pims[, path] > 0.5 & data$OUT > 0.5)
    cat(paste0("👉 路徑 [", path, "]: "))
    if(length(cases_idx) > 0) {
      cn <- data$countrycode[cases_idx]
      cat(paste(if(length(cn)>15) paste(head(cn, 15), collapse=", ") else paste(cn, collapse=", "), "\n"))
    } else cat("(無典型案例)\n")
  }
}

# 4. 分析引擎 (V14.0 Union版)
run_union_qca <- function(flat_data, label, threshold, use_sanctions=FALSE) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0("🚀 分析: ", label, "\n"))
  cat(paste0("   設定: Logic=Union(Max) | Cut=", threshold, " | Sanctions=", use_sanctions, "\n"))
  
  qca_data <- flat_data %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score_union(., economic_factors, threshold),
      FZ_GOV   = calc_score_union(., governance_factors, threshold),
      FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG   = manual_calibrate(as.numeric(ORG), threshold)
    )
  
  if(use_sanctions) {
    qca_data <- qca_data %>% 
      mutate(FZ_SANCT = calc_score_union(., sanctional_factors, 0.85)) # 風險門檻維持高
  }
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_FTA", "FZ_ORG")
  if(use_sanctions) sel_vars <- c(sel_vars, "FZ_SANCT")
  
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% 
    na.omit() %>% as.data.frame()
  
  # B. 必要性快篩
  cat("\n   [必要性快篩 (Incl > 0.85)]\n")
  try({ print(superSubset(qca_final, outcome = "OUT", incl.cut = 0.85)) })
  
  # C. 真值表
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  
  # 數值防呆
  raw_incl <- as.numeric(as.character(TT$tt$incl[TT$tt$n > 0]))
  raw_incl <- raw_incl[!is.na(raw_incl)]
  top_cons <- if(length(raw_incl) > 0) max(raw_incl) else 0
  
  cat(paste0("\n   🧪 最高一致性: ", round(top_cons, 3), "\n"))
  
  # 對於 Union 邏輯，我們期待較高的一致性，所以標準設高一點
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else 0.70
  
  cat(paste0("   ✂️ 採用一致性門檻: ", final_cut, "\n"))
  
  tryCatch({
    SOL <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
    print(SOL)
    print_solution_cases(SOL, qca_final)
  }, error = function(e) cat("   ⚠️ 無法求解 (No Solution)。\n"))
}

# ==============================================================================
# 5. 執行
# ==============================================================================
if(!exists("CP9623_pca")) stop("❌ 請先載入 CP9623_pca")

dt_p1 <- process_period_data(CP9623_pca, 1997, 2012, "P1 江胡 (1997-2012)")
dt_p2 <- process_period_data(CP9623_pca, 2013, 2017, "P2 習前期 (2013-2017)")
dt_p3 <- process_period_data(CP9623_pca, 2018, 2023, "P3 習後期 (2018-2023)")

# P1: 菁英模式 (但試試看 Union 會有什麼變化？含制裁)
run_union_qca(dt_p1, "P1 江胡時期", 0.65, use_sanctions=TRUE)

# P2: 習前期 (Union 測試機會主義，無制裁)
run_union_qca(dt_p2, "P2 習前期", 0.65, use_sanctions=FALSE)

# P3: 習後期 (Union 測試機會主義，無制裁)
run_union_qca(dt_p3, "P3 習後期", 0.65, use_sanctions=FALSE)

# ==============================================================================
# 📦 V14.2 終極嚴謹版：全變數滯後 + 聯集攻守兼備 (Full-Lagged Union Logic)
#    修正：
#    1. [全滯後] FTA 與 ORG 也納入滯後處理，確保 "制度先於升級"。
#    2. [攻守兼備] 在 P2/P3 加回制裁變數 (FZ_SANCT)，測試 "底線否決"。
#    3. [聯集邏輯] 繼續使用 Max，捕捉 "單點突破" 的機會主義。
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 變數定義 (經濟/治理/制裁 使用已有的 _lag)
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", "china_ex_to_i_lag", 
                        "china_im_fr_i_lag", "population_total_lag")
sanctional_factors <- c("trade_lag", "arms_lag", "financial_lag")
governance_factors <- c("rl_lag", "ge_lag", "cc_lag", "va_lag") 

# 2. 數據處理核心 (✨ 修復 FTA/ORG 滯後問題)
process_period_data_v14 <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 處理數據 (全滯後): ", label, " (", start_year, "-", end_year, ")\n"))
  
  agg_data <- raw_panel %>%
    arrange(countrycode, year) %>% # 確保排序正確才能 Lag
    group_by(countrycode) %>%
    mutate(
      # ✨ 關鍵修正：現場製造 FTA 和 ORG 的滯後版本
      FTA_lag = dplyr::lag(as.numeric(FTA), 1),
      ORG_lag = dplyr::lag(as.numeric(ORG), 1)
    ) %>%
    filter(as.numeric(as.character(year)) >= start_year & 
             as.numeric(as.character(year)) <= end_year) %>%
    summarise(
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      # 連續變數直接取平均 (因為已經是 _lag 了)
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), ~mean(., na.rm = TRUE)),
      
      # ✨ 制度變數取區間內的最大值 (但來源是滯後過的 FTA_lag)
      # 意義：該時期內，是否 "曾經" 在前一年有過 FTA？
      FTA = max(FTA_lag, na.rm = TRUE),
      ORG = max(ORG_lag, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .))) %>%
    filter(!is.na(gdp_per_capita_lag)) %>%
    mutate(OUT = ifelse(OUT_Event > 0, 1, 0)) %>%
    as.data.frame()
  
  return(agg_data)
}

# 3. 校準與分析工具 (保持 V14.0 的 Union 邏輯)
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

# Max/Union Proxy
calc_score_union <- function(data, vars, threshold) {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  score <- apply(calibrated, 1, max, na.rm=TRUE) # Max Logic
  ifelse(abs(score-0.5) < 0.001, 0.501, score)
}

print_solution_cases <- function(sol, data) {
  if(is.null(sol$pims)) return()
  cat("\n🔍 [典型案例解碼]\n")
  for(path in colnames(sol$pims)) {
    cases_idx <- which(sol$pims[, path] > 0.5 & data$OUT > 0.5)
    cat(paste0("👉 路徑 [", path, "]: "))
    if(length(cases_idx) > 0) {
      cn <- data$countrycode[cases_idx]
      cat(paste(if(length(cn)>15) paste(head(cn, 15), collapse=", ") else paste(cn, collapse=", "), "\n"))
    } else cat("(無典型案例)\n")
  }
}

# 4. 分析引擎 (含制裁)
run_union_qca_full <- function(flat_data, label, threshold) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0("🚀 分析: ", label, "\n"))
  cat(paste0("   設定: Logic=Union(Max) | Cut=", threshold, " | 含制裁變數 (攻守兼備測試)\n"))
  
  qca_data <- flat_data %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score_union(., economic_factors, threshold),
      FZ_GOV   = calc_score_union(., governance_factors, threshold),
      FZ_SANCT = calc_score_union(., sanctional_factors, 0.85), # 制裁門檻高
      FZ_FTA   = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG   = manual_calibrate(as.numeric(ORG), threshold)
    )
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_FTA", "FZ_ORG", "FZ_SANCT")
  
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% 
    na.omit() %>% as.data.frame()
  
  # B. 必要性快篩
  cat("\n   [必要性快篩 (Incl > 0.85)]\n")
  try({ print(superSubset(qca_final, outcome = "OUT", incl.cut = 0.85)) })
  
  # C. 真值表
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  
  # 防呆
  raw_incl <- as.numeric(as.character(TT$tt$incl[TT$tt$n > 0]))
  raw_incl <- raw_incl[!is.na(raw_incl)]
  top_cons <- if(length(raw_incl) > 0) max(raw_incl) else 0
  
  cat(paste0("\n   🧪 最高一致性: ", round(top_cons, 3), "\n"))
  
  # 門檻決策
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else 0.70
  cat(paste0("   ✂️ 採用一致性門檻: ", final_cut, "\n"))
  
  tryCatch({
    SOL <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
    print(SOL)
    print_solution_cases(SOL, qca_final)
  }, error = function(e) cat("   ⚠️ 無法求解 (No Solution)。\n"))
}

# ==============================================================================
# 5. 執行
# ==============================================================================
if(!exists("CP9623_pca")) stop("❌ 請先載入 CP9623_pca")

# 重新生成數據 (使用新的滯後邏輯)
dt_p1_v14 <- process_period_data_v14(CP9623_pca, 1997, 2012, "P1 江胡 (1997-2012)")
dt_p2_v14 <- process_period_data_v14(CP9623_pca, 2013, 2017, "P2 習前期 (2013-2017)")
dt_p3_v14 <- process_period_data_v14(CP9623_pca, 2018, 2023, "P3 習後期 (2018-2023)")

# 執行全變數分析
run_union_qca_full(dt_p1_v14, "P1 江胡時期", 0.65)
run_union_qca_full(dt_p2_v14, "P2 習前期", 0.65)
run_union_qca_full(dt_p3_v14, "P3 習後期", 0.65)

# ==============================================================================
# 📦 V15.0 地緣制度模型：合成 INS 與引入 PROX (Geo-Institutional Model)
#    特徵：
#    1. [合成] FZ_INS = Max(FTA, ORG)。只要有任一制度連結即得分。
#    2. [地緣] 新增 FZ_PROX (鄰近度)。距離越近，分數越高。
#    3. [全滯後] 維持所有變數 Lag-1 的嚴謹設定。
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 變數定義 (假設原始數據中有 'dist' 或 'distance' 欄位)
#    注意：請確認您的 CP9623_pca 中有距離變數，通常叫 "dist" 或 "distcap"
economic_factors   <- c("gdp_per_capita_lag", "gdp_per_capita_diff_CHN_lag", "china_ex_to_i_lag", 
                        "china_im_fr_i_lag", "population_total_lag")
sanctional_factors <- c("trade_lag", "arms_lag", "financial_lag")
governance_factors <- c("rl_lag", "ge_lag", "cc_lag", "va_lag") 

# 2. 校準與分析工具
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

# 聯集分數計算 (Max Logic)
calc_score_union <- function(data, vars, threshold) {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  score <- apply(calibrated, 1, max, na.rm=TRUE) 
  ifelse(abs(score-0.5) < 0.001, 0.501, score)
}

# 3. 數據處理核心 (含 INS 合成與 PROX 校準)
process_geo_institutional <- function(raw_panel, start_year, end_year, label) {
  cat(paste0("\n📅 處理數據 (含地緣/制度): ", label, " (", start_year, "-", end_year, ")\n"))
  
  # 檢查距離變數 (嘗試自動偵測常見名稱)
  dist_col <- NULL
  if("dist" %in% names(raw_panel)) dist_col <- "dist"
  else if("distance" %in% names(raw_panel)) dist_col <- "distance"
  else if("distcap" %in% names(raw_panel)) dist_col <- "distcap"
  
  if(is.null(dist_col)) {
    warning("⚠️ 找不到距離變數 (dist/distance/distcap)！將使用隨機值代替以防崩潰 (請務必檢查數據源！)")
    raw_panel$dist <- runif(nrow(raw_panel), 100, 20000) # 假數據防崩潰
    dist_col <- "dist"
  } else {
    cat(paste0("   ✅ 偵測到距離變數: ", dist_col, "\n"))
  }
  
  agg_data <- raw_panel %>%
    arrange(countrycode, year) %>%
    group_by(countrycode) %>%
    mutate(
      # 滯後處理
      FTA_lag = dplyr::lag(as.numeric(FTA), 1),
      ORG_lag = dplyr::lag(as.numeric(ORG), 1),
      # 距離通常是常數，不需要滯後，但為了對齊
      DIST_val = .data[[dist_col]]
    ) %>%
    filter(as.numeric(as.character(year)) >= start_year & 
             as.numeric(as.character(year)) <= end_year) %>%
    summarise(
      OUT_Event = {
        vals <- build_upgrade2
        if(all(is.na(vals))) 0 else max(vals, na.rm = TRUE)
      },
      across(all_of(c(economic_factors, sanctional_factors, governance_factors)), ~mean(., na.rm = TRUE)),
      FTA = max(FTA_lag, na.rm = TRUE),
      ORG = max(ORG_lag, na.rm = TRUE),
      DIST = mean(DIST_val, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .))) %>%
    filter(!is.na(gdp_per_capita_lag)) %>%
    mutate(OUT = ifelse(OUT_Event > 0, 1, 0)) %>%
    as.data.frame()
  
  return(agg_data)
}

# 4. 分析引擎 (V15.0)
run_geo_qca <- function(flat_data, label, threshold) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0("🚀 分析: ", label, "\n"))
  cat(paste0("   變數結構: INS(FTA+ORG), PROX(Distance), ECON, GOV, SANCT\n"))
  
  # 校準
  qca_data <- flat_data %>% ungroup() %>%
    mutate(
      FZ_ECON  = calc_score_union(., economic_factors, threshold),
      FZ_GOV   = calc_score_union(., governance_factors, threshold),
      FZ_SANCT = calc_score_union(., sanctional_factors, 0.85),
      
      # ✨ 1. 合成制度變數 (INS) = Max(FTA, ORG)
      FZ_FTA_raw = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG_raw = manual_calibrate(as.numeric(ORG), threshold),
      FZ_INS     = pmax(FZ_FTA_raw, FZ_ORG_raw), # 只要有一項強就強
      
      # ✨ 2. 校準鄰近度 (PROX) = 1 - Distance_Score
      # 先算出距離的分數 (越遠分數越高)，然後反轉
      FZ_DIST_raw = manual_calibrate(DIST, threshold),
      FZ_PROX     = 1 - FZ_DIST_raw
    ) %>%
    mutate(across(c(FZ_INS, FZ_PROX), ~ifelse(abs(.-0.5)<0.001, 0.501, .)))
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_SANCT", "FZ_INS", "FZ_PROX")
  
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% 
    na.omit() %>% as.data.frame()
  
  # 檢查必要性
  cat("\n   [必要性快篩 (Incl > 0.85)]\n")
  try({ print(superSubset(qca_final, outcome = "OUT", incl.cut = 0.85)) })
  
  # 真值表
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  
  raw_incl <- as.numeric(as.character(TT$tt$incl[TT$tt$n > 0]))
  top_cons <- if(length(raw_incl) > 0) max(raw_incl, na.rm=TRUE) else 0
  
  cat(paste0("\n   🧪 最高一致性: ", round(top_cons, 3), "\n"))
  
  # 門檻
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else 0.70
  cat(paste0("   ✂️ 採用一致性門檻: ", final_cut, "\n"))
  
  tryCatch({
    SOL <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
    print(SOL)
    print_solution_cases(SOL, qca_final)
  }, error = function(e) cat("   ⚠️ 無法求解。\n"))
}

# ==============================================================================
# 5. 執行 V15.0
# ==============================================================================
if(!exists("CP9623_pca")) stop("❌ 請先載入 CP9623_pca")

# 生成數據
dt_p1_geo <- process_geo_institutional(CP9623_pca, 1997, 2012, "P1 江胡")
dt_p2_geo <- process_geo_institutional(CP9623_pca, 2013, 2017, "P2 習前期")
dt_p3_geo <- process_geo_institutional(CP9623_pca, 2018, 2023, "P3 習後期")

# 執行分析 (統一門檻 0.65)
run_geo_qca(dt_p1_geo, "P1 江胡時期", 0.65)
run_geo_qca(dt_p2_geo, "P2 習前期", 0.65)
run_geo_qca(dt_p3_geo, "P3 習後期", 0.65)

# ==============================================================================
# 📦 V16.0 自適應地緣制度模型 (Logic-Adaptive Geo-Institutional Model)
#    理論映射：
#    P1 (1997-2012): 韜光養晦 -> Min Logic (嚴格選拔)
#    P2 (2013-2017): 奮發有為 -> Mean Logic (綜合評估)
#    P3 (2018-2023): 攻守兼備 -> Max Logic (機會主義/政治亮點)
# ==============================================================================

library(tidyverse)
library(QCA)

# 1. 校準函數
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

# 靈活的分數合成函數
calc_score_custom <- function(data, vars, threshold, logic="mean") {
  sub_data <- as.data.frame(data)[, vars, drop = FALSE]
  sub_data <- sub_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  calibrated <- matrix(NA, nrow=nrow(data), ncol=length(vars))
  for(i in 1:length(vars)) {
    val <- if_else(is.na(data[[vars[i]]]), 0, as.numeric(data[[vars[i]]]))
    calibrated[,i] <- manual_calibrate(jitter(val, amount=0.00001), threshold)
  }
  if(logic == "min") return(apply(calibrated, 1, min, na.rm=TRUE))
  if(logic == "max") return(apply(calibrated, 1, max, na.rm=TRUE))
  return(rowMeans(calibrated, na.rm=TRUE))
}

# 2. 執行引擎 (V16.0)
run_adaptive_qca <- function(flat_data, label, logic_type, threshold) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0("🚀 分析: ", label, " | 核心邏輯: ", logic_type, "\n"))
  
  # A. 核心校準與合成
  qca_data <- flat_data %>% ungroup() %>%
    mutate(
      # 經濟與治理根據時代邏輯變動
      FZ_ECON  = calc_score_custom(., economic_factors, threshold, logic_type),
      FZ_GOV   = calc_score_custom(., governance_factors, threshold, logic_type),
      # 制裁風險維持高門檻
      FZ_SANCT = calc_score_custom(., sanctional_factors, 0.85, logic_type),
      # ✨ 合成 INS (制度連結) = Max(FTA, ORG) - 只要有制度化聯繫就得分
      FZ_FTA_raw = case_when(FTA >= 1 ~ 1, TRUE ~ 0),
      FZ_ORG_raw = manual_calibrate(as.numeric(ORG), threshold),
      FZ_INS     = pmax(FZ_FTA_raw, FZ_ORG_raw),
      # ✨ 地緣鄰近度 PROX
      FZ_PROX    = 1 - manual_calibrate(DIST, threshold)
    ) %>%
    mutate(across(starts_with("FZ_"), ~ifelse(abs(.-0.5)<0.001, 0.501, .)))
  
  sel_vars <- c("FZ_ECON", "FZ_GOV", "FZ_SANCT", "FZ_INS", "FZ_PROX")
  qca_final <- qca_data %>% dplyr::select(countrycode, OUT, all_of(sel_vars)) %>% na.omit() %>% as.data.frame()
  
  # B. 真值表與求解
  TT <- truthTable(qca_final, outcome = "OUT", conditions = sel_vars, sort.by = "incl")
  raw_incl <- as.numeric(as.character(TT$tt$incl[TT$tt$n > 0])); raw_incl <- raw_incl[!is.na(raw_incl)]
  top_cons <- if(length(raw_incl) > 0) max(raw_incl) else 0
  
  cat(paste0("🧪 最高一致性: ", round(top_cons, 3), "\n"))
  final_cut <- if(top_cons >= 0.8) 0.8 else if(top_cons >= 0.75) 0.75 else 0.70
  
  if(top_cons < 0.70) {
    cat("⚠️ 一致性不足，無法識別穩定路徑。這暗示決策權已轉向非結構因素(如政治表態)。\n")
    print(head(TT$tt[TT$tt$n > 0, ], 5))
    return(NULL)
  }
  
  tryCatch({
    SOL <- minimize(TT, include = "?", details = TRUE, incl.cut = final_cut)
    print(SOL)
    print_solution_cases(SOL, qca_final)
  }, error = function(e) cat("⚠️ 求解失敗。\n"))
}

# 3. 執行
if(!exists("dt_p1_geo")) {
  dt_p1_geo <- process_geo_institutional(CP9623_pca, 1997, 2012, "P1 江胡")
  dt_p2_geo <- process_geo_institutional(CP9623_pca, 2013, 2017, "P2 習前期")
  dt_p3_geo <- process_geo_institutional(CP9623_pca, 2018, 2023, "P3 習後期")
}

# 依時代特徵分配邏輯
run_adaptive_qca(dt_p1_geo, "P1 江胡時代 (防禦選拔)", "min", 0.65)
run_adaptive_qca(dt_p2_geo, "P2 習前期 (擴張普發)", "mean", 0.50)
run_adaptive_qca(dt_p3_geo, "P3 習後期 (政治統戰)", "max", 0.50)
