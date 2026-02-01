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

cat(rep("=", 80), "\n")
cat("缺失值插補\n")
cat(rep("=", 80), "\n\n")

# 定義三類變數
economic_factors <- c("gdp_per_capita_std", "gdp_per_capita_diff_CHN","china_ex_to_i_std", "china_im_fr_i_std", 
                      "exportdep_std", "importdep_std", "population_total_std")

sanctional_factors <- c("trade_std", "arms_std", "military_std",
                        "financial", "travel_std")

governance_factors <- c("va_std", "psv_std", "ge_std",
                        "rq_std", "rl_std", "cc_std")

all_vars <- c(economic_factors, sanctional_factors, governance_factors)

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
# 📦 V15.0 策略調整版：菁英門檻放寬 + 政治變數引入
# ==============================================================================

# 0. 環境準備 (不再糾結 shiny，直接用手動校準)
library(tidyverse)
library(FactoMineR)
library(QCA)

# 1. 資料源確認
if (!exists("CP9623_pca")) stop("❌ 錯誤：請先讀取 CP9623_pca 資料。")

# 2. 定義變數
economic_factors   <- c("gdp_per_capita", "china_ex_to_i", "china_im_fr_i") 
sanctional_factors <- c("financial", "trade", "travel")
# 治理變數表現太差，這次我們移除它，改用政治變數

# 3. 手動校準函數 (Logit-like)
manual_calibrate_elite <- function(x, quantile_prob) {
  # quantile_prob: 設定哪一個分位數對應到 0.5
  rank_score <- ecdf(x)(x)
  # 公式：q^k = 0.5 -> k = log(0.5)/log(q)
  k <- log(0.5) / log(quantile_prob)
  fuzzy_score <- rank_score ^ k
  return(fuzzy_score)
}

# 4. 變數生成器 (調整門檻)
create_proxy_variable <- function(data, vars, label, prob_threshold = 0.75) {
  # 注意：這裡將門檻從 0.9 (Top 10%) 放寬到 0.75 (Top 25%)
  clean_data <- data[, vars] %>% na.omit()
  n_vars <- min(3, length(vars))
  
  if(nrow(clean_data) > 10) {
    pca <- PCA(clean_data, scale.unit = TRUE, graph = FALSE, ncp = 1)
    loadings <- abs(pca$var$coord[, 1])
    top_vars <- names(sort(loadings, decreasing = TRUE)[1:n_vars])
  } else { top_vars <- vars[1:n_vars] }
  
  valid_vars <- top_vars[top_vars %in% names(data)]
  cat(paste0("\n🏆 [", label, "] 門檻放寬至 Top 25%，納入：", paste(valid_vars, collapse="|"), "\n"))
  
  calibrated_matrix <- matrix(NA, nrow=nrow(data), ncol=length(valid_vars))
  for(i in 1:length(valid_vars)) {
    raw <- if_else(is.na(data[[valid_vars[i]]]), 0, as.numeric(data[[valid_vars[i]]]))
    x_jit <- jitter(raw, amount=0.00001)
    # 使用手動函數
    calibrated_matrix[,i] <- manual_calibrate_elite(x_jit, prob_threshold) 
  }
  union_score <- apply(calibrated_matrix, 1, max, na.rm=TRUE)
  ifelse(abs(union_score-0.5)<0.001, 0.501, union_score)
}

# 5. 數據處理 Pipeline
# ------------------------------------------------------------------------------
cat("\n🔄 [Step 1] 數據準備...\n")

panel_clean <- as.data.frame(CP9623_pca)
if(!"countrycode" %in% names(panel_clean)) {
  if("country" %in% names(panel_clean)) panel_clean$countrycode <- panel_clean$country
  else if("CNAME" %in% names(panel_clean)) panel_clean$countrycode <- panel_clean$CNAME
}

# 滯後處理 (包含建交年限 dip_age)
# 假設建交年限是 dip_age 或类似變數，若無則自動忽略
has_dip <- "dip_age" %in% names(panel_clean)
raw_vars_to_lag <- c(economic_factors, sanctional_factors, "FTA", "ORG")
if(has_dip) raw_vars_to_lag <- c(raw_vars_to_lag, "dip_age")

CP_Lagged <- panel_clean %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(across(all_of(raw_vars_to_lag), ~dplyr::lag(., 1), .names = "{.col}_lag")) %>%
  ungroup() %>%
  filter(!is.na(get(paste0(economic_factors[1], "_lag"))))

econ_lag   <- paste0(economic_factors, "_lag")
sanct_lag  <- paste0(sanctional_factors, "_lag")

cat("🚀 [Step 2] 生成變數 (含政治變數)...\n")

CP_Final_Dataset <- CP_Lagged %>%
  mutate(
    # 1. 經濟力 (放寬門檻至 Top 25%)
    FZ_ECON = create_proxy_variable(., econ_lag, "經濟力", 0.75),
    
    # 2. 風險度 (維持高門檻 Top 10%，因為只有極少數國家被制裁)
    FZ_SANCT = create_proxy_variable(., sanct_lag, "風險度", 0.90),
    
    # 3. 政治忠誠 (建交年限)
    FZ_POL = if(has_dip) {
      manual_calibrate_elite(as.numeric(dip_age_lag), 0.50) # 只要高於中位數就算老朋友
    } else { 0 },
    
    # 4. 制度變數
    FZ_FTA = case_when(as.numeric(FTA_lag) >= 2 ~ 1, as.numeric(FTA_lag) >= 1 ~ 0.6, TRUE ~ 0),
    FZ_ORG = manual_calibrate_elite(as.numeric(ORG_lag), 0.75),
    
    # 結果
    OUT_L3 = if_else(as.numeric(as.character(partnership)) >= 3, 1, 0),
    year_num = as.numeric(as.character(year))
  ) %>%
  mutate(across(starts_with("FZ_"), ~as.numeric(ifelse(abs(.-0.5)<0.001, 0.501, .))))

# 切割 P2
cols <- c("OUT_L3", "FZ_ECON", "FZ_SANCT", "FZ_FTA", "FZ_ORG")
if(has_dip) cols <- c(cols, "FZ_POL")

data_p2 <- CP_Final_Dataset %>% 
  filter(year_num >= 2013) %>% 
  dplyr::select(all_of(cols)) %>% 
  na.omit() %>% 
  as.data.frame()

# 6. 分析
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse=""), "\n", sep="")
cat("👉 [P2 最終分析] 政治+經濟+制度模型\n")
cat(paste(rep("=", 50), collapse=""), "\n", sep="")

# 充分性解算
TT_P2 <- truthTable(data_p2, outcome = "OUT_L3", conditions = cols[-1], incl.cut = 0.75, n.cut = 1, sort.by = "incl", show.cases = TRUE)
print(head(TT_P2$tt, 10))

SOL_P2 <- tryCatch({
  minimize(TT_P2, include = "?", details = TRUE)
}, error = function(e) {
  cat("\n❌ 依然無解。這意味著習近平的外交升級具有高度隨機性。\n")
  return(NULL)
})

if(!is.null(SOL_P2)) {
  cat("\n✅ [解算成功] 路徑結果：\n")
  print(SOL_P2$IC$incl.cov)
}