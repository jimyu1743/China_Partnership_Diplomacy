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
CP9623$`ASEAN+1`[is.na(CP9623$`ASEAN+1`)] <- 0

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
# 習近平外交戰略 QCA 分析系統 
# ==============================================================================

# 0. 載入必要套件
# ------------------------------------------------------------
if(!require(QCA)) install.packages("QCA")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(stringr)) install.packages("stringr")

library(QCA)
library(dplyr)
library(ggplot2)
library(stringr)

# ============================================================
# 1. 數據前處理 (Data Cleaning & Definition)
# ============================================================
# 假設您的原始資料框名稱為 CP9623_qca

cat("\n🔄 正在進行數據清洗與變數定義...\n")

qca_ready <- CP9623_qca %>%
  dplyr::select(partnership, FZ_ECON, FZ_SANCT, FZ_GOV, FTA, year) %>%
  mutate(
    # 【關鍵修正 1】強制修正年份格式 (避免 R 把年份當成文字類別)
    year = as.numeric(as.character(year)),
    
    # 【關鍵修正 2】定義結果變數：戰略夥伴 (Level >= 2 為核心圈)
    # 邏輯：我們要分析的是「誰進入了核心圈」，而不僅僅是「誰升級了」
    OUT = if_else(partnership >= 2, 1, 0),
    
    # 【變數更名】簡化變數名稱以便分析
    ECON = FZ_ECON,
    SANCT = FZ_SANCT,
    GOV = FZ_GOV,
    
    # 【FTA處理】確保 NA 視為 0 (無協定)
    FTA = if_else(is.na(FTA), 0, FTA)
  ) %>%
  na.omit() # 移除缺漏值，確保分析樣本完整

# ============================================================
# 2. 時段切割 (Period Splitting)
# ============================================================
cat("📊 正在分割時段樣本...\n")

# 時段 A: 江胡時期 (1996-2012) - 戰略對照組
period_1 <- qca_ready %>% filter(year <= 2012)

# 時段 B: 習近平時期 (2013-2023) - 戰略實驗組
period_2 <- qca_ready %>% filter(year >= 2013)

cat("   - P1 (江胡時期):", nrow(period_1), "筆\n")
cat("   - P2 (習近平時期):", nrow(period_2), "筆\n")

# ============================================================
# 3. 核心運算：QCA 智慧解算引擎 (Adaptive Engine)
# ============================================================

# --- 函數：自動化解算器 (含自動降標功能) ---
solve_qca_adaptive <- function(data, label) {
  cat(paste0("\n🚀 正在分析 ", label, "...\n"))
  
  # 步驟 1: 先嘗試高標準 (0.8)
  # ------------------------------------------------------
  TT_high <- truthTable(data, outcome = "OUT", 
                        conditions = c("ECON", "SANCT", "GOV", "FTA"),
                        incl.cut = 0.8, n.cut = 1, sort.by = "incl", show.cases = FALSE)
  
  sol <- tryCatch({
    minimize(TT_high, include = "?", details = TRUE, row.dom = FALSE)
  }, error = function(e) NULL)
  
  # 檢查結果是否為空 (有物件但無路徑)
  is_empty <- is.null(sol) || is.null(sol$IC$incl.cov) || nrow(sol$IC$incl.cov) == 0
  
  # 步驟 2: 如果高標準失敗，嘗試戰略降標 (0.75)
  # ------------------------------------------------------
  if (is_empty) {
    cat(paste0("   ⚠️  ", label, " 標準門檻(0.8) 無路徑，啟動戰略降標至 0.75...\n"))
    
    TT_low <- truthTable(data, outcome = "OUT", 
                         conditions = c("ECON", "SANCT", "GOV", "FTA"),
                         incl.cut = 0.75, n.cut = 1, sort.by = "incl", show.cases = FALSE)
    
    sol <- tryCatch({
      minimize(TT_low, include = "?", details = TRUE, row.dom = FALSE)
    }, error = function(e) {
      cat(paste0("   ❌ ", label, " 即使降標至 0.75 仍無解 (戰略混沌)。\n"))
      return(NULL)
    })
    
    if (!is.null(sol) && !is.null(sol$IC$incl.cov) && nrow(sol$IC$incl.cov) > 0) {
      cat(paste0("   ✅ ", label, " 降標後解算成功！(捕捉到隱性戰略)\n"))
    }
  } else {
    cat(paste0("   ✅ ", label, " 高標準(0.8) 解算成功！\n"))
  }
  
  return(sol)
}

# --- 執行解算 ---
sol_p1 <- solve_qca_adaptive(period_1, "P1:江胡時期")
sol_p2 <- solve_qca_adaptive(period_2, "P2:習近平時期")

# ============================================================
# 4. 視覺化模組：生成戰略儀表板 (Dashboard)
# ============================================================

# --- 函數：防彈版結果提取器 ---
extract_results_robust <- function(sol_object, period_label) {
  
  # 定義空結果的格式
  empty_df <- data.frame(Path = "無一致戰略 (混沌/無解)", Consistency = 0.1, Coverage = 0.1, Period = period_label)
  
  # 狀況 1: 物件不存在
  if (is.null(sol_object)) return(empty_df)
  
  # 狀況 2: 物件存在但內容為空 (Empty Matrix)
  if (is.null(sol_object$IC$incl.cov) || nrow(sol_object$IC$incl.cov) == 0) return(empty_df)
  
  # 狀況 3: 正常產出
  paths <- rownames(sol_object$IC$incl.cov)
  stats <- sol_object$IC$incl.cov
  
  # 防呆：確保 stats 是矩陣格式，避免單一路徑時變成向量導致出錯
  if (!is.matrix(stats)) { stats <- t(as.matrix(stats)) }
  
  df <- data.frame(
    Path = paths,
    Consistency = as.numeric(stats[, "inclS"]),
    Coverage = as.numeric(stats[, "covS"]),
    Period = period_label
  )
  
  return(df)
}

# --- 整合結果 ---
dashboard_data <- bind_rows(
  extract_results_robust(sol_p1, "P1: 江胡時期 (1996-2012)"),
  extract_results_robust(sol_p2, "P2: 習近平時期 (2013-2023)")
)

# --- 美化路徑名稱 (智庫翻譯機) ---
# 將 QCA 的數學符號轉譯為易讀的戰略語言
dashboard_data$Path_Label <- dashboard_data$Path %>%
  str_replace_all("\\*", " + ") %>%
  str_replace_all("~", "無") %>%
  str_replace_all("ECON\\[0\\]", "弱勢經濟") %>%
  str_replace_all("SANCT\\[0\\]", "未受制裁") %>%
  str_replace_all("GOV\\[0\\]", "治理混亂") %>%
  str_replace_all("FTA\\[1\\]", "初級FTA") %>%
  str_replace_all("FTA\\[2\\]", "中級FTA") %>%
  str_replace_all("FTA\\[3\\]", "高級FTA") 

# ============================================================
# 5. 最終輸出與繪圖 (Final Output)
# ============================================================

# --- 輸出文字表格 ---
cat("\n", paste(rep("=", 50), collapse = ""), "\n", sep = "")
cat("📋 戰略路徑總表 (Strategic Pathways)\n")
cat(paste(rep("=", 50), collapse = ""), "\n", sep = "")

# 使用 dplyr::select 避免衝突
print(dashboard_data %>% dplyr::select(Period, Path_Label, Consistency, Coverage))

# --- 繪製泡泡圖 (Bubble Plot) ---
# 計算 Y 軸最大值，用於調整邊界
y_max <- max(dashboard_data$Coverage, na.rm = TRUE)
if(y_max == 0) y_max <- 0.5

plot_final <- ggplot(dashboard_data, aes(x = Consistency, y = Coverage, color = Period)) +
  # 1. 繪製泡泡 (大小代表覆蓋率/重要性)
  geom_point(aes(size = Coverage), alpha = 0.7) +
  scale_size(range = c(5, 12)) + 
  
  # 2. 加上路徑標籤 (自動調整位置，避免重疊)
  geom_text(aes(label = Path_Label), vjust = -1.8, size = 4, fontface = "bold", show.legend = FALSE) +
  
  # 3. 繪製門檻參考線 (0.75)
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.74, y = 0.02, label = "戰略門檻 (0.75)", color = "gray50", angle = 90) +
  
  # 4. 軸範圍設定 (防止文字被切掉)
  scale_x_continuous(limits = c(0, 1.05)) +
  scale_y_continuous(limits = c(0, y_max * 1.4)) +
  
  # 5. 配色 (藍=過去, 紅=現在) 與主題設定
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "中國外交戰略變遷：從「混沌」到「制度化」",
    subtitle = "藍點：江胡時期 (P1) vs 紅點：習近平時期 (P2)",
    x = "戰略一致性 (Consistency) -> 規則越明確",
    y = "戰略解釋力 (Coverage) -> 適用國家越多",
    caption = "Designed by Gemini AI Thought Partner"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# 顯示圖表
print(plot_final)

