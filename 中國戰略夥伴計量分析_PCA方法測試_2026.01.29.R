# ============================================================
# 0. 環境設定與套件安裝 (更新版)
# ============================================================

# 設定套件路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# 定義套件清單
# 已新增: dplyr, ggplot2, tibble (雖然包含在 tidyverse 中，但明確列出以確保載入順序)
packages <- c(
  "tidyverse", "dplyr", "ggplot2", "tibble", "magrittr", # 核心資料處理與繪圖
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
    # cat("已載入:", pkg, "\n") # 若想看到載入過程可取消註解
  }, error = function(e) { 
    cat("❌ 載入失敗:", pkg, "\n") 
  })
}))

cat("✅ 所有環境與套件設定完成！\n")

# ============================================================================
# 1. 資料讀取與前處理
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("資料讀取與前處理\n")
cat(rep("=", 80), "\n\n")

# 讀入資料檔
China_partnership_1996_2023_breakpoint2026_01_23 <- read_excel("C:/Users/jimyu1743/Desktop/習近平與中國夥伴關係外交/原始與整理資料/China partnership 1996-2023 breakpoint2026.01.23.xlsx")
CP9623 <- China_partnership_1996_2023_breakpoint2026_01_23

cat("原始資料:", nrow(CP9623), "筆觀察值\n")

# 處理東協+1的遺漏值
CP9623$`ASEAN_CHN`[is.na(CP9623$`ASEAN_CHN`)] <- 0

# 計算建交年齡，並將負數計算結果變成遺漏值
CP9623$dip_age <- CP9623$year - CP9623$diplomcy
CP9623$dip_age[CP9623$dip_age < 0] <- NA

# 計算治理指標(WGI)，用六個次變數加總平均成一個指標
CP9623 <- CP9623 %>%
  rowwise() %>%
  mutate(WGI = mean(c(va, psv, ge, rq, rl, cc), na.rm = TRUE)) %>%
  ungroup()

# 檢查2001年後有缺失值的國家
countries_missing_after_2001 <- CP9623 %>% 
  filter(year > 2001) %>%
  group_by(countrycode, countryname) %>%
  summarise(
    missing_count = sum(is.na(across(everything()))),
    missing_rows = sum(rowSums(is.na(across(everything()))) > 0),
    total_rows = n(),
    missing_percentage = (missing_rows / total_rows) * 100,
    .groups = "drop"
  ) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

cat("2001年後有缺失值的國家數:", nrow(countries_missing_after_2001), "\n")

# 刪除沒有特定資料的國家
excluded_countries <- c("SSD", "MCO", "PLW", "TUV", "MNE", "KIR", "LIE", "SRB", 
                        "SLB", "SMR","PRK", "ERI", "VEN", "NRU", "MHL", "FSM", 
                        "TON", "VUT", "USA", "CUB", "SYR", "TMP", "CPV","AFG", 
                        "LBN", "YEM")
CP9623 <- CP9623[!CP9623$countrycode %in% excluded_countries, ]

# 刪除gdp為0的觀測值
CP9623 <- CP9623[CP9623$gdp != 0, ]

# 計算對中國貿易依賴變數
CP9623$exportdep <- CP9623$china_im_fr_i / CP9623$gdp
CP9623$importdep <- CP9623$china_ex_to_i / CP9623$gdp

# 整併變數
CP9623$economy <- CP9623$trade + CP9623$financial
CP9623$arms_and_military <- CP9623$arms + CP9623$military
CP9623$sanction <- CP9623$economy + CP9623$arms_and_military + CP9623$travel
# 加入習近平時期變數
CP9623$xi <- ifelse(CP9623$year < 2013, 0, 1)

# 標準化指定變數
CP9623 <- CP9623 %>%
  mutate(
    across(
      .cols = c("dist", "dip_age", "population_total", "gdp", "gdp_per_capita",
                "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
                "economy","arms_and_military",
                "trade", "arms", "military", "financial", "travel","sanction",
                "WGI","va", "psv", "ge", "rq", "rl", "cc", "WGI","WGI_diff_CHN",
                "gdp_per_capita_diff_CHN","FTA","ORG"), 
      .fns = ~ as.numeric(scale(.)),  
      .names = "{.col}_std" 
    ))

# 計算人均GDP
CP9623$gdp_per_capita <- CP9623$gdp / CP9623$population_total

# 設定為面板資料格式
panel_CP9623 <- pdata.frame(CP9623, index = c("countrycode", "year"))

# 刪除含有Inf或-Inf的列
panel_CP9623 <- panel_CP9623[!apply(panel_CP9623, 1, function(x) any(is.infinite(x))), ]

cat("清理後資料:", nrow(panel_CP9623), "筆觀察值\n\n")

# ============================================================================
# 2. 缺失值插補
# ============================================================================

# 定義變數群組 (只定義一次！)
economic_factors   <- c("gdp_per_capita_std", "gdp_per_capita_diff_CHN", "china_ex_to_i_std", 
                        "china_im_fr_i_std", "exportdep_std", "importdep_std", "population_total_std")
sanctional_factors <- c("trade_std", "arms_std", "military_std", "financial", "travel_std")
governance_factors <- c("va_std", "psv_std", "ge_std", "rq_std", "rl_std", "cc_std")
all_vars <- c(economic_factors, sanctional_factors, governance_factors)

# 使用 zoo::na.approx 優化時間序列插補
panel_imputed <- panel_CP9623 %>%
  group_by(countrycode) %>%
  mutate(across(all_of(all_vars), ~ {
    if(sum(!is.na(.)) >= 2) na.approx(., rule = 2) else .
  })) %>%
  ungroup()

# 缺失值分析
overall_missing <- sapply(panel_CP9623[, all_vars], function(x) sum(is.na(x)))
cat("總缺失值數:", sum(overall_missing), "\n")

# 三階段插補
panel_imputed <- panel_CP9623

# 階段1: 時間序列線性插補
cat("階段1: 時間序列線性插補\n")
for (country in unique(panel_imputed$countrycode)) {
  country_idx <- which(panel_imputed$countrycode == country)
  
  for (var in all_vars) {
    country_data <- panel_imputed[country_idx, ]
    
    if (any(is.na(country_data[[var]])) && sum(!is.na(country_data[[var]])) >= 2) {
      order_idx <- order(country_data$year)
      country_data_sorted <- country_data[order_idx, ]
      original_idx <- country_idx[order_idx]
      non_na_idx <- !is.na(country_data_sorted[[var]])
      
      interpolated <- approx(
        x = country_data_sorted$year[non_na_idx],
        y = country_data_sorted[[var]][non_na_idx],
        xout = country_data_sorted$year,
        method = "linear",
        rule = 2
      )$y
      
      na_positions <- which(is.na(country_data_sorted[[var]]))
      if (length(na_positions) > 0) {
        panel_imputed[original_idx[na_positions], var] <- interpolated[na_positions]
      }
    }
  }
}

# 階段2: 橫斷面中位數插補
cat("階段2: 橫斷面中位數插補\n")
years <- c(1996, 2013, 2023)
for (year in years) {
  year_idx <- which(panel_imputed$year == year)
  for (var in all_vars) {
    year_median <- median(panel_imputed[year_idx, var], na.rm = TRUE)
    missing_idx <- year_idx[is.na(panel_imputed[year_idx, var])]
    if (length(missing_idx) > 0 && !is.na(year_median)) {
      panel_imputed[missing_idx, var] <- year_median
    }
  }
}

# 階段3: 全局中位數插補
remaining <- sum(sapply(panel_imputed[, all_vars], function(x) sum(is.na(x))))
if (remaining > 0) {
  cat("階段3: 全局中位數插補 (剩餘", remaining, "個缺失值)\n")
  for (var in all_vars) {
    global_median <- median(panel_imputed[[var]], na.rm = TRUE)
    missing_idx <- which(is.na(panel_imputed[[var]]))
    if (length(missing_idx) > 0 && !is.na(global_median)) {
      panel_imputed[missing_idx, var] <- global_median
    }
  }
}

# 清理與驗證
panel_common <- panel_imputed %>%
  filter(!is.na(countrycode) & !is.na(year))

final_missing <- sum(sapply(panel_common[, all_vars], function(x) sum(is.na(x))))
complete_rate <- round(sum(complete.cases(panel_common[, all_vars])) / 
                         nrow(panel_common) * 100, 2)

cat("\n✓ 插補完成!\n")
cat("  總樣本:", nrow(panel_common), "觀察值\n")
cat("  涵蓋國家:", length(unique(panel_common$countrycode)), "國\n")
cat("  剩餘缺失值:", final_missing, "\n")
cat("  完整率:", complete_rate, "%\n\n")

# ============================================================================
# 3. PCA降維分析
# ============================================================================

library(FactoMineR) # 確保載入 PCA 套件
library(dplyr)
library(writexl)

cat(rep("=", 80), "\n")
cat("PCA 降維分析與 QCA 代理變數篩選\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# PCA 函數 (含自動選將功能)
# ==============================================================================
perform_pca <- function(data, vars, name, method = "kaiser", manual_n = NULL) {
  
  # 1. 變數防呆：確保變數都在資料集中
  vars <- vars[vars %in% names(data)]
  if(length(vars) == 0) {
    cat("❌【", name, "】錯誤：找不到指定變數，跳過。\n", sep = "")
    return(NULL)
  }
  
  # 2. 資料清洗：移除缺漏值 (注意：這會導致樣本減少)
  clean_data <- data[, vars] %>% na.omit()
  
  if (nrow(clean_data) < 5) {
    cat("⚠️【", name, "】有效樣本不足 5 筆，跳過分析。\n", sep = "")
    return(NULL)
  }
  
  # 3. 執行 PCA (使用 FactoMineR)
  # ncp 設定為變數總數，確保我們能計算所有特徵值以便篩選
  pca_result <- PCA(clean_data, scale.unit = TRUE, graph = FALSE, ncp = ncol(clean_data))
  
  # 4. 決定主成分數量 (保留原本邏輯)
  if (!is.null(manual_n)) {
    n_comp <- manual_n
  } else if (method == "kaiser") {
    n_comp <- sum(pca_result$eig[, 1] >= 1.0)
  } else if (method == "variance80") {
    # 修正為找累積變異 >= 80% 的第一個位置
    n_comp <- which(pca_result$eig[, 3] >= 80)[1]
  }
  
  # 保底機制：至少 1 個，最多不超過變數數
  n_comp <- max(1, min(n_comp, ncol(clean_data), na.rm = TRUE))
  
  # 5. 【關鍵新增】提取最佳代理變數 (Proxy Selection)
  # 抓取第一主成分 (Dim.1) 的 Loadings
  loadings <- abs(pca_result$var$coord[, 1])
  top_proxy <- names(sort(loadings, decreasing = TRUE))[1]
  top_loading_val <- round(max(loadings), 3)
  
  # 6. 輸出戰情摘要
  cat("【", name, "】\n", sep = "")
  cat("   • 樣本數:", nrow(clean_data), "| 變數數:", length(vars), "\n")
  cat("   • 建議主成分數:", n_comp, "| 累積變異解釋力:", round(pca_result$eig[n_comp, 3], 1), "%\n")
  cat("   • 🏆 QCA 最佳代理變數:", top_proxy, "(Loading =", top_loading_val, ")\n") 
  cat("     (若需進行 QCA，請直接使用此變數代表整個維度)\n\n")
  
  # 7. 回傳結果 (修復 list 賦值 bug)
  return(list(
    pca_obj = pca_result, 
    n_components = n_comp, 
    available_vars = vars,
    valid_rows = rownames(clean_data), # 修正：這裡必須用 = 不能用 <-
    top_proxy = top_proxy
  ))
}

# ==============================================================================
# 執行 PCA 分析
# ==============================================================================
# 假設 economic_factors, sanctional_factors, governance_factors 已在環境中定義
pca_economic <- perform_pca(panel_common, economic_factors, "經濟維度")
pca_sanctional <- perform_pca(panel_common, sanctional_factors, "制裁維度")
pca_governance <- perform_pca(panel_common, governance_factors, "治理維度")

# ==============================================================================
# 定義回填函數 (將 PC 分數併回原始資料)
# ==============================================================================
add_pca_scores <- function(data, pca_result_list, prefix) {
  if (is.null(pca_result_list)) return(data)
  
  # 提取需要的資訊
  n_pc <- pca_result_list$n_components
  valid_rows <- pca_result_list$valid_rows # 那些沒有缺漏值的列名
  
  # 確保資料有 rownames 以便對齊 (如果原始資料沒有，就用順序)
  if(is.null(rownames(data))) rownames(data) <- 1:nrow(data)
  
  # 準備要填入的座標分數
  # FactoMineR 的分數存在 $ind$coord 裡
  scores <- pca_result_list$pca_obj$ind$coord[, 1:n_pc, drop = FALSE]
  
  for (i in 1:n_pc) {
    col_name <- paste0(prefix, "_PC", i)
    # 初始化全為 NA
    data[[col_name]] <- NA_real_
    
    # 【關鍵修正】利用 rowname 精準回填，避免錯位
    # 只有在 valid_rows 裡面的列才會有分數
    match_idx <- match(valid_rows, rownames(data))
    
    # 防呆：確保長度一致
    if(length(match_idx) == nrow(scores)) {
      data[[col_name]][match_idx] <- scores[, i]
    } else {
      warning(paste(prefix, "回填時發生索引長度不一致，請檢查資料列名。"))
    }
  }
  
  cat("✓", prefix, ": 已加入", n_pc, "個主成分至資料表\n")
  return(data)
}

# ==============================================================================
# 執行回填與存檔
# ==============================================================================
CP9623_pca <- panel_common %>%
  add_pca_scores(pca_economic, "econ") %>%
  add_pca_scores(pca_sanctional, "sanct") %>%
  add_pca_scores(pca_governance, "gov")

# 檢查結果
pc_cols <- grep("^(econ|sanct|gov)_PC[0-9]", names(CP9623_pca), value = TRUE)
cat("\n統計總結：共加入", length(pc_cols), "個主成分欄位。\n")
# print(head(CP9623_pca[, pc_cols])) # 預覽一下

# 儲存最終結果
write_xlsx(CP9623_pca, path = "CP9623_pca.xlsx")
cat("檔案已儲存：CP9623_pca.xlsx\n")

# ==============================================================================
# QCA分析_PCA聯集合成
# ==============================================================================

# 0. 載入套件
library(tidyverse)
library(FactoMineR)
library(QCA) 

# 1. 資料源確認
if (!exists("CP9623_pca")) stop("❌ 錯誤：找不到 'CP9623_pca'。請先執行前面的資料讀取與插補代碼。")

# 2. 定義變數 (去除 _std，使用原始名稱)
# ------------------------------------------------------------------------------
economic_factors   <- c("gdp_per_capita", "gdp_per_capita_diff_CHN", "china_ex_to_i", 
                        "china_im_fr_i", "exportdep", "importdep", "population_total")
sanctional_factors <- c("trade", "arms", "military", "financial", "travel")
governance_factors <- c("va", "psv", "ge", "rq", "rl", "cc") 

has_dip <- "dip_age" %in% names(CP9623_pca)

# 3. 核心函數：手動校準與 PCA 合成
# ------------------------------------------------------------------------------
manual_calibrate <- function(x, quantile_prob) {
  rank_score <- ecdf(x)(x)
  k <- log(0.5) / log(quantile_prob)
  return(rank_score ^ k)
}

create_pca_union_proxy <- function(data, vars, label, prob_threshold = 0.75) {
  sub_data <- data[, vars]
  n_select <- min(3, length(vars))
  
  if(length(vars) > 1) {
    # PCA 自動標準化
    pca <- PCA(sub_data, scale.unit = TRUE, graph = FALSE, ncp = 1)
    loadings <- abs(pca$var$coord[, 1])
    top_vars <- names(sort(loadings, decreasing = TRUE)[1:n_select])
  } else { top_vars <- vars }
  
  cat(paste0("\n🏆 [", label, "] 選取原始變數：", paste(top_vars, collapse=" | "), "\n"))
  
  calibrated_matrix <- matrix(NA, nrow=nrow(data), ncol=length(top_vars))
  for(i in 1:length(top_vars)) {
    raw <- if_else(is.na(data[[top_vars[i]]]), 0, as.numeric(data[[top_vars[i]]]))
    x_jit <- jitter(raw, amount=0.00001) 
    calibrated_matrix[,i] <- manual_calibrate(x_jit, prob_threshold)
  }
  union_score <- apply(calibrated_matrix, 1, max, na.rm=TRUE)
  ifelse(abs(union_score-0.5) < 0.001, 0.501, union_score)
}

clean_qca_data <- function(df) {
  d <- as.data.frame(df); d[] <- lapply(d, as.numeric); return(d)
}

# 4. 數據前處理 Pipeline
# ------------------------------------------------------------------------------
cat("\n🔄 [Step 1] 數據準備 (使用原始變數 Lag t-1)...\n")

panel_clean <- as.data.frame(CP9623_pca)
if(!"countrycode" %in% names(panel_clean)) {
  if("country" %in% names(panel_clean)) panel_clean <- panel_clean %>% rename(countrycode = country)
  else if("CNAME" %in% names(panel_clean)) panel_clean <- panel_clean %>% rename(countrycode = CNAME)
}

vars_to_lag <- c(economic_factors, sanctional_factors, governance_factors, "FTA", "ORG")
if(has_dip) vars_to_lag <- c(vars_to_lag, "dip_age")

CP_Lagged <- panel_clean %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(across(all_of(vars_to_lag), ~dplyr::lag(., 1), .names = "{.col}_lag")) %>%
  ungroup() %>%
  filter(!is.na(get(paste0(economic_factors[1], "_lag"))))

# 變數名 (無 _std)
econ_lag_vars  <- paste0(economic_factors, "_lag")
sanct_lag_vars <- paste0(sanctional_factors, "_lag")
gov_lag_vars   <- paste0(governance_factors, "_lag")

cat("\n🚀 [Step 2] 計算模糊集分數...\n")

CP_FZ_Base <- CP_Lagged %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    
    FZ_ECON = create_pca_union_proxy(., econ_lag_vars, "經濟力", 0.70),
    FZ_SANCT = create_pca_union_proxy(., sanct_lag_vars, "風險度", 0.90),
    FZ_GOV = create_pca_union_proxy(., gov_lag_vars, "治理力", 0.70),
    
    FZ_POL = if(has_dip) manual_calibrate(as.numeric(dip_age_lag), 0.50) else 0,
    FZ_FTA = case_when(as.numeric(FTA_lag) >= 2 ~ 1, as.numeric(FTA_lag) >= 1 ~ 0.6, TRUE ~ 0),
    FZ_ORG = manual_calibrate(as.numeric(ORG_lag), 0.70)
  ) %>%
  mutate(across(starts_with("FZ_"), ~as.numeric(ifelse(abs(.-0.5)<0.001, 0.501, .))))

# 5. 分流指定 OUT (P1 vs P2)
# ------------------------------------------------------------------------------
cat("\n✂️ [Step 3] 資料切割與 OUT 定義...\n")

cols_base <- c("year_num", "partnership", "FZ_ECON", "FZ_SANCT", "FZ_GOV", "FZ_FTA", "FZ_ORG")
if(has_dip) cols_base <- c(cols_base, "FZ_POL")
cols_qca <- c("OUT", setdiff(cols_base, c("year_num", "partnership")))

# === P1 (江胡) ===
data_p1 <- CP_FZ_Base %>%
  filter(year_num <= 2012) %>%
  mutate(OUT = if_else(as.numeric(as.character(partnership)) >= 1, 1, 0)) %>% 
  dplyr::select(all_of(cols_qca)) %>%
  na.omit() %>%
  clean_qca_data()

# === P2 (習近平) ===
data_p2 <- CP_FZ_Base %>%
  filter(year_num >= 2013) %>%
  mutate(OUT = if_else(as.numeric(as.character(partnership)) >= 2, 1, 0)) %>% 
  dplyr::select(all_of(cols_qca)) %>%
  na.omit() %>%
  clean_qca_data()

cat(paste0("\n✅ 資料準備就緒:\n"))
cat(paste0("   📊 P1 (1996-2012) OUT>=1 | 樣本數: ", nrow(data_p1), " | OUT=1 佔比: ", round(mean(data_p1$OUT)*100, 2), "%\n"))
cat(paste0("   📊 P2 (2013-2023) OUT>=2 | 樣本數: ", nrow(data_p2), " | OUT=1 佔比: ", round(mean(data_p2$OUT)*100, 2), "%\n"))

# 6. 執行 QCA 分析
# ------------------------------------------------------------------------------
run_analysis_final <- function(qca_data, raw_data_source, label) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n", sep="")
  cat("👉 [", label, "] QCA 深度解析\n", sep="")
  cat(paste(rep("=", 60), collapse=""), "\n", sep="")
  
  conds <- names(qca_data)[-1]
  
  # 必要性
  tryCatch({
    cat("🔍 必要性檢定 (Consistency > 0.85):\n")
    print(superSubset(qca_data, outcome = "OUT", conditions = conds, relation = "necessity", incl.cut = 0.85))
  }, error=function(e) cat("   (無顯著必要條件)\n"))
  
  # 充分性
  cat("\n📊 充分性解算 (Truth Table):\n")
  for(cut in c(0.80, 0.75)) {
    cat(paste0("\n🧪 嘗試門檻 incl.cut = ", cut, "...\n"))
    TT <- truthTable(qca_data, outcome = "OUT", conditions = conds, incl.cut = cut, n.cut = 1, sort.by = "incl", show.cases = TRUE)
    
    if(sum(TT$tt$OUT == 1) > 0) {
      SOL <- minimize(TT, include = "?", details = TRUE, row.dom = FALSE)
      cat("✅ 成功找到路徑！\n")
      print(SOL$IC$incl.cov)
      
      cat("\n🔍 [關鍵組態與國家名單]:\n")
      winning_rows <- TT$tt[TT$tt$OUT == 1, ]
      
      for(i in 1:nrow(winning_rows)) {
        incl_val <- as.numeric(winning_rows[i, "incl"]) # 確保數值型別
        cat(paste0("   --------------------------------------------------\n"))
        cat(paste0("   [Path ", i, "] Consistency: ", round(incl_val, 3), "\n"))
        print(winning_rows[i, conds])
        
        # 案例反查
        case_ids_str <- winning_rows[i, "cases"]
        if (!is.na(case_ids_str) && case_ids_str != "") {
          ids <- as.numeric(unlist(strsplit(as.character(case_ids_str), ",")))
          
          # 重建對照表 (從原始含 ID 的資料中提取對應行)
          # 這裡我們用 raw_data_source 進行對應，務必確保 raw_data_source 是經過同樣篩選的
          lookup_table <- raw_data_source %>% 
            dplyr::select(countrycode) %>% 
            na.omit()
          
          # 提取國家
          if(max(ids) <= nrow(lookup_table)) {
            found_countries <- lookup_table[ids, ]
            cat(paste0("   🌍 國家: ", paste(unique(found_countries), collapse = ", "), "\n"))
          }
        }
      }
      return(invisible(NULL))
    }
  }
  cat("❌ 無解\n")
}

# 準備對照用的原始資料 (含 ID)
raw_p1 <- CP_FZ_Base %>% filter(year_num <= 2012) %>% 
  mutate(OUT = if_else(as.numeric(as.character(partnership)) >= 1, 1, 0)) %>% 
  dplyr::select(countrycode, all_of(names(data_p1))) %>% na.omit()

raw_p2 <- CP_FZ_Base %>% filter(year_num >= 2013) %>% 
  mutate(OUT = if_else(as.numeric(as.character(partnership)) >= 2, 1, 0)) %>% 
  dplyr::select(countrycode, all_of(names(data_p2))) %>% na.omit()

# 執行分析
run_analysis_final(data_p1, raw_p1, "P1 江胡時期 (Partnership >= 1)")
run_analysis_final(data_p2, raw_p2, "P2 習近平時期 (Partnership >= 2)")
# ==============================================================================
# 📦 V21.1: 真值表深度診斷 (Truth Table Inspection)
#    目的：當 minimize 為 NULL 時，直接查看是哪些組態導致了 OUT=1
# ==============================================================================

# 我們專注於 P2 (習近平時期)
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("👉 [P2 深度診斷] 查看通過 0.75 門檻的具體組態\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# 1. 建立真值表 (不隱藏細節)
TT_P2_Check <- truthTable(data_p2, outcome = "OUT", conditions = cols[-1],
                          incl.cut = 0.75, n.cut = 1, sort.by = "incl")

# 2. 提取「一致性 > 0.75」的行 (即被認定為充分條件的行)
winning_rows <- TT_P2_Check$tt[TT_P2_Check$tt$OUT == 1, ]

# 3. 印出這些關鍵組態
print(winning_rows)

# 4. 解讀輔助
cat("\n💡 如何解讀上表：\n")
cat("1. 每一行代表一種「國家類型」 (1=高, 0=低)。\n")
cat("2. 這是電腦認為『足以成為戰略夥伴』的組合。\n")
cat("3. 記下這些組合的特徵（例如：是否都是 ORG=1？）。\n")
cat("4. 'cases' 欄位顯示了具體的國家代碼，可以用來做案例分析。\n")

# 5. 案例反查 (Mapping Cases)
# 如果您想知道 cases 裡的數字是哪些國家，請執行下面這段：
if(nrow(winning_rows) > 0) {
  cat("\n🔍 [案例反查] 看看這些類型具體是哪些國家：\n")
  
  # 抓取第一行 winning row 的案例
  first_row_cases <- winning_rows$cases[1] 
  
  # 處理字串 (移除空白, 分割)
  case_indices <- as.numeric(unlist(strsplit(as.character(first_row_cases), ",")))
  
  # 對應回原始資料 (注意：這裡只是範例，需確保 row name 對齊)
  # 我們用 rownames 來對應
  target_countries <- rownames(data_p2)[match(case_indices, rownames(data_p2))]
  # 如果 rownames 不是 ID，我們嘗試直接用 data_p2 的 countrycode (如果還保留的話)
  # 由於 data_p2 經過 clean_qca_data 清洗可能沒了 ID，我們用原始 CP_Final_Dataset 對應
  
  # 替代方案：直接列出該類型的特徵
  cat("👉 排名第一的組態 (Row 1) 特徵：\n")
  print(winning_rows[1, 1:5]) 
}

# ==============================================================================
# 📦 V27.0 P1 補完計畫：視覺化無解的現象 & 存檔
#    適用於 P1 (江胡時期)
# ==============================================================================

library(ggplot2)
library(ggrepel)
library(writexl)

# 1. 準備 P1 繪圖資料 (視覺化「無結構」特徵)
# ------------------------------------------------------------------------------
# 因為 P1 沒有單一成功路徑，我們畫「最佳結構條件 vs 結果」圖
# 邏輯：取所有結構性條件的最大值 (Max)，看是否與結果有關
plot_data_p1 <- raw_p1 %>%
  mutate(
    # 定義「結構性得分」：取 經濟、FTA、組織 的最大值
    # 代表該國在任一結構性領域的最強表現
    Structural_Score = pmax(FZ_ECON, FZ_FTA, FZ_ORG),
    
    # 結果：P1 的標準 (Partnership >= 1)
    Outcome_Score = OUT
  ) 

# 2. 繪製 P1 的 XY 散佈圖 (展示雜亂無章)
# ------------------------------------------------------------------------------
cat("\n🎨 正在繪製 P1 (江胡時期) 的非結構化特徵圖...\n")

p1_plot <- ggplot(plot_data_p1, aes(x = Structural_Score, y = Outcome_Score)) +
  geom_jitter(width = 0.02, height = 0.05, alpha = 0.6, color = "darkgreen", size = 2) +
  geom_smooth(method = "loess", color = "orange", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  
  labs(
    title = "江胡時期 (P1) 外交夥伴選擇的散佈特徵",
    subtitle = "特徵：高結構得分未必帶來夥伴關係 (右下角)，低分也可能成為夥伴 (左上角)",
    x = "最佳結構性條件強度 (Max of Econ/FTA/Org)",
    y = "實際夥伴地位 (0=無, 1=普通夥伴)",
    caption = "註：數據分佈呈現高度離散，缺乏明確的線性或集合論規律 (No Solution)。"
  ) +
  theme_minimal()

# 顯示圖表
print(p1_plot)

# 3. 儲存 P1 結果
# ------------------------------------------------------------------------------
cat("\n💾 正在儲存 P1 分析結果...\n")

# A. 儲存圖片
ggsave("QCA_XY_Plot_P1_NoSolution.png", plot = p1_plot, width = 8, height = 6, dpi = 300)
cat("   ✅ P1 散佈圖已存為 'QCA_XY_Plot_P1_NoSolution.png'\n")

# B. 儲存 P1 真值表 (Excel)
# 重新計算真值表
TT_P1_Export <- truthTable(data_p1, outcome = "OUT", conditions = names(data_p1)[-1],
                           incl.cut = 0.75, n.cut = 1, sort.by = "incl")

tt_p1_df <- as.data.frame(TT_P1_Export$tt)

# 轉換案例 ID 為國家名稱
ids_to_names_p1 <- function(ids_str) {
  if(is.na(ids_str) || ids_str == "") return("")
  ids <- as.numeric(unlist(strsplit(as.character(ids_str), ",")))
  tryCatch({
    countries <- raw_p1[ids, ]$countrycode
    return(paste(unique(countries), collapse = ", "))
  }, error = function(e) return(ids_str))
}

tt_p1_df$Countries <- sapply(tt_p1_df$cases, ids_to_names_p1)

write_xlsx(tt_p1_df, "QCA_TruthTable_P1_Results.xlsx")
cat("   ✅ P1 真值表已存為 'QCA_TruthTable_P1_Results.xlsx'\n")

# C. 儲存 P1 文字報告
sink("QCA_Analysis_Report_P1.txt")
cat("=== QCA 分析報告 (江胡時期 P1) ===\n")
cat("標準：Partnership >= 1\n\n")
cat("--- 必要性檢定 ---\n")
print(superSubset(data_p1, outcome = "OUT", conditions = names(data_p1)[-1], relation = "necessity", incl.cut = 0.85))
cat("\n--- 充分性解算 (嘗試 0.75) ---\n")
print(TT_P1_Export)
cat("\n結論：在此門檻下無解 (No Solution)，顯示外交策略缺乏一致的結構性路徑。\n")
sink()
cat("   ✅ P1 文字報告已存為 'QCA_Analysis_Report_P1.txt'\n")
# ==============================================================================
# 📦 V26.0 視覺化與存檔模組 (Visualization & Export)
#    適用於 P2 (習近平時期) 的成功路徑
# ==============================================================================

library(ggplot2)
library(ggrepel) # 用於避免標籤重疊
library(writexl) # 用於輸出 Excel

# 1. 準備繪圖資料 (使用 raw_p2 以確保有國家代碼)
# ------------------------------------------------------------------------------
# 我們要畫的路徑是：FZ_SANCT * ~FZ_GOV * FZ_FTA
# 邏輯：這三個條件的"交集" (min) 就是這條路徑的隸屬分數

plot_data <- raw_p2 %>%
  mutate(
    # 計算路徑分數 (Intersection / Min)
    # 注意：~FZ_GOV 在數學上等於 1 - FZ_GOV
    Path_Score = pmin(FZ_SANCT, (1 - FZ_GOV), FZ_FTA),
    
    # 結果分數
    Outcome_Score = OUT,
    
    # 判斷是否為一致性案例 (X <= Y) -> 在對角線上方
    Consistent = Path_Score <= Outcome_Score
  ) %>%
  # 只篩選路徑分數 > 0.5 的國家來標示 (避免圖太亂)
  filter(Path_Score > 0.5)

# 2. 繪製 XY 散佈圖 (XY Plot)
# ------------------------------------------------------------------------------
cat("\n🎨 正在繪製 XY 散佈圖...\n")

p <- ggplot(plot_data, aes(x = Path_Score, y = Outcome_Score)) +
  # 1. 繪製對角線 (充分性邊界)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  
  # 2. 繪製點 (區分一致與不一致)
  geom_point(aes(color = Consistent), size = 3, alpha = 0.8) +
  
  # 3. 標上國家代碼 (使用 ggrepel 自動避開重疊)
  geom_text_repel(aes(label = countrycode), size = 3.5, box.padding = 0.3) +
  
  # 4. 美化圖表
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("矛盾案例 (X>Y)", "一致案例 (X<=Y)")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "習近平時期戰略夥伴路徑的充分性分析",
    subtitle = "路徑：受制裁 (SANCT) * 低治理 (~GOV) * 制度連結 (FTA) -> 戰略夥伴 (OUT)",
    x = "路徑隸屬度 (Solution Membership)",
    y = "戰略夥伴隸屬度 (Outcome Membership)",
    caption = "註：藍點位於對角線上方，代表完全符合充分條件邏輯；紅點為矛盾案例。"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 顯示圖表
print(p)

# 3. 儲存結果 (Export Results)
# ------------------------------------------------------------------------------
cat("\n💾 正在儲存分析結果...\n")

# A. 儲存圖片
ggsave("QCA_XY_Plot_P2.png", plot = p, width = 8, height = 6, dpi = 300)
cat("   ✅ 圖片已存為 'QCA_XY_Plot_P2.png'\n")

# B. 儲存真值表 (Truth Table)
# 我們重新跑一次 P2 的 Truth Table 抓取完整資料
TT_Export <- truthTable(data_p2, outcome = "OUT", conditions = names(data_p2)[-1],
                        incl.cut = 0.75, n.cut = 1, sort.by = "incl")

# 將 Truth Table 轉為 Data Frame 並加入案例名單
tt_df <- as.data.frame(TT_Export$tt)

# 這裡我們手動把 case ID 換成國家名，方便您閱讀
# 定義一個簡單的轉換函數
ids_to_names <- function(ids_str) {
  if(is.na(ids_str) || ids_str == "") return("")
  ids <- as.numeric(unlist(strsplit(as.character(ids_str), ",")))
  # 對應回 raw_p2 的國家代碼
  # 注意：raw_p2 必須和 data_p2 的行號對應。
  # 由於之前 data_p2 經過 na.omit，我們用 row.names 對應最保險
  # 但因為 QCA output 的 cases 是行號，我們直接用 raw_p2 (假設兩者行數一致)
  # 為了保險，我們只處理有值的
  tryCatch({
    countries <- raw_p2[ids, ]$countrycode
    return(paste(unique(countries), collapse = ", "))
  }, error = function(e) return(ids_str))
}

# 應用轉換 (這步可能會慢一點點)
tt_df$Countries <- sapply(tt_df$cases, ids_to_names)

# 輸出 Excel
write_xlsx(tt_df, "QCA_TruthTable_P2_Results.xlsx")
cat("   ✅ 真值表已存為 'QCA_TruthTable_P2_Results.xlsx'\n")

# C. 儲存文字報告 (將 Console 的分析結果存成 TXT)
sink("QCA_Analysis_Report.txt")
cat("=== QCA 分析報告 (習近平時期 P2) ===\n\n")
cat("路徑: FZ_SANCT * ~FZ_GOV * FZ_FTA -> OUT\n\n")
cat("--- 必要性檢定 ---\n")
print(superSubset(data_p2, outcome = "OUT", conditions = names(data_p2)[-1], relation = "necessity", incl.cut = 0.85))
cat("\n--- 最小化解 (Solution) ---\n")
# 重新執行一次 minimize 以獲取輸出
TT_Temp <- truthTable(data_p2, outcome = "OUT", conditions = names(data_p2)[-1], incl.cut = 0.75, n.cut = 1)
SOL_Temp <- minimize(TT_Temp, include = "?", details = TRUE, row.dom = FALSE)
print(SOL_Temp)
sink() # 結束錄製
cat("   ✅ 文字報告已存為 'QCA_Analysis_Report.txt'\n")
