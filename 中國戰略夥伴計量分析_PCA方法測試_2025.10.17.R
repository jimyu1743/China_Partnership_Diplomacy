# 設定並建立套件安裝路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))

cat("套件安裝路徑:", lib_path, "\n\n")

# 定義所有需要的套件
packages <- c("tidyverse", "gapminder", "magrittr", "plm", "ExtremeBounds", 
              "car", "ordinal", "openxlsx", "writexl", "haven", "pastecs", 
              "FactoMineR", "factoextra", "MASS", "mvProbit", "psych", 
              "pglm", "panelWranglR", "readr", "readxl", "stats", 
              "GPArotation", "usdm")

# 步驟 1: 檢查並安裝缺少的套件
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("正在安裝:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  }
}

# 步驟 2: 載入所有套件
for (pkg in packages) {
  library(pkg, character.only = TRUE)
  cat("已載入:", pkg, "\n")
}

# 讀入資料檔
China_partnership_1996_2023_breakpoint_2025_03_05 <- read_excel("China partnership 1996-2023 breakpoint 2025.03.24.2.xlsx", sheet = "資料")
CP9623 <- China_partnership_1996_2023_breakpoint_2025_03_24_02

# 處理東協+1的遺漏值
CP9623$`ASEAN+1`[is.na(CP9623$`ASEAN+1`)] <- 0

# 計算建交年齡,並將負數計算結果變成遺漏值
CP9623$dip_age <- CP9623$year - CP9623$diplomcy
CP9623$dip_age[CP9623$dip_age < 0] <- NA

# 計算治理指標(WGI),並用六個次變數加總平均成一個指標
CP9623 <- CP9623 %>%
  rowwise() %>%
  mutate(WGI = mean(c(va, psv, ge, rq, rl, cc), na.rm = TRUE)) %>%
  ungroup()

# 檢查哪些國家在2001年後有缺失值,將這些國家找出
countries_missing_after_2001 <- CP9623 %>% 
  filter(year > 2001) %>%
  group_by(countrycode, countryname) %>%
  summarise(
    missing_count = sum(is.na(across(everything()))),
    missing_rows = sum(rowSums(is.na(across(everything()))) > 0),
    total_rows = n(),
    missing_percentage = (missing_rows / total_rows) * 100,
    .groups = "drop"  # 這會解除所有分組
  ) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))
view(countries_missing_after_2001)
countries_missing_after_2001$countrycode

# 刪除沒有特定資料的國家
excluded_countries <- c("SSD", "MCO", "PLW", "TUV", "MNE", "KIR", "LIE", "SRB", "SLB", "SMR","PRK", "ERI", "VEN",
                        "NRU", "MHL", "FSM", "TON", "VUT", "USA", "CUB", "SYR", "TMP", "CPV","AFG", "LBN", "YEM")
CP9623 <- CP9623[!CP9623$countrycode %in% excluded_countries, ]

# 刪除gdp為0的觀測值
CP9623 <- CP9623[CP9623$gdp != 0, ]

# 計算對中國貿易依賴變數
CP9623$exportdep <- CP9623$china_im_fr_i / CP9623$gdp
CP9623$importdep <- CP9623$china_ex_to_i / CP9623$gdp

# 依照countrycode分組,計算partnership的差分,並將結果存入新變數
CP9623 <- CP9623 %>%
  group_by(countrycode) %>%
  mutate(partnership_diff = c(NA, diff(partnership))) %>%
  ungroup()

CP9623 <- CP9623 %>%
  group_by(countrycode) %>%
  mutate(partnership2_diff = c(NA, diff(partnership2))) %>%
  ungroup()

# 將下列變數整併成新變數
CP9623$economy <- CP9623$trade + CP9623$financial
CP9623$arms_and_military <- CP9623$arms + CP9623$military

# 加入變數xi
CP9623$xi <- ifelse(CP9623$year < 2013, 0, 1)

# 將資料框架設定為面板資料格式
panel_CP9623 <- pdata.frame(CP9623, index = c("countrycode", "year"))

# 刪除遺漏值
panel_CP9623 <- na.omit(panel_CP9623)

# 刪除資料框中含有 Inf 或 -Inf 的列
panel_CP9623 <- panel_CP9623[!apply(panel_CP9623, 1, function(x) any(is.infinite(x))), ]

# 將連續變數標準化,並用新變數名稱加入
panel_CP9623 <- panel_CP9623 %>%
  mutate(
    across(
      .cols = c("dist", "dip_age", "population_total", "gdp", 
                "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
                "trade", "arms", "military", "financial", "travel",
                "va", "psv", "ge", "rq", "rl", "cc", "WGI"), 
      .fns = ~ as.numeric(scale(.)),  
      .names = "{.col}_std" 
    )
  )

# 將資料依時間落後一年,避免出現內生性問題
panel_CP9623 <- panel_lag(data = panel_CP9623,
                          cross.section = c("countrycode","countryname"),
                          time.variable = "year",
                          lags = 1,
                          variables.selected = c("population_total", "gdp", 
                                                 "china_ex_to_i", "china_im_fr_i", "exportdep",
                                                 "importdep","WGI_std", "population_total_std","economy","arms_and_military","gdp_std", 
                                                 "china_ex_to_i_std", "china_im_fr_i_std", "exportdep_std",
                                                 "importdep_std","partnership","partnership2","partnership_diff","partnership2_diff",
                                                 "trade_std", "arms_std", "military_std", "financial_std", "travel_std",
                                                 "va_std", "psv_std", "ge_std", "rq_std", "rl_std", "cc_std"),
                          keep.original = TRUE)

# 刪除遺漏值
panel_CP9623 <- na.omit(panel_CP9623)

# ============================================================================
# 國家分類分析 - PCA與分群完整流程
# ============================================================================

library(FactoMineR)
library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)
library(knitr)

# ============================================================================
# 前置設定：定義變數與參數
# ============================================================================

# 定義三類自變數
economic_factors <- c("gdp_std", "china_ex_to_i_std", "china_im_fr_i_std", 
                      "exportdep_std", "importdep_std", "population_total_std")

structural_factors <- c("trade_std", "arms_std", "military_std",
                        "financial", "travel_std")

governance_factors <- c("va_std", "psv_std", "ge_std",
                        "rq_std", "rl_std", "cc_std")

# 分析年度
years <- c(1996, 2013, 2023)

# 識別國家欄位（請根據實際資料調整）
country_id_col <- "countrycode"  

# ============================================================================
# 步驟0: 資料驗證
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("步驟0: 資料驗證\n")
cat(rep("=", 80), "\n\n")

## 檢查資料框是否存在
if (!exists("panel_CP9623")) {
  stop("錯誤: 找不到資料框 'panel_CP9623'。請先載入資料！")
}

# 顯示資料結構
cat("資料框基本資訊:\n")
cat("- 觀察值數量:", nrow(panel_CP9623), "\n")
cat("- 變數數量:", ncol(panel_CP9623), "\n")

# 修正年度範圍的顯示方式
if (is.factor(panel_CP9623$year)) {
  # 如果是因子，顯示層級範圍
  year_levels <- levels(panel_CP9623$year)
  cat("- 年度範圍:", paste(year_levels[1], "-", year_levels[length(year_levels)]), "\n\n")
} else {
  # 如果是數值，使用 range
  cat("- 年度範圍:", paste(range(panel_CP9623$year, na.rm = TRUE), collapse = " - "), "\n\n")
}

# 檢查國家識別欄位
if (!country_id_col %in% names(panel_CP9623)) {
  cat("警告: 找不到國家識別欄位 '", country_id_col, "'\n", sep = "")
  cat("可用欄位:", paste(names(panel_CP9623)[1:min(20, ncol(panel_CP9623))], collapse = ", "), "\n")
  stop("請修正 country_id_col 變數")
}

cat("✓ 國家識別欄位:", country_id_col, "\n")
cat("  唯一國家數:", length(unique(panel_CP9623[[country_id_col]])), "\n\n")

# 檢查變數完整性
all_vars <- c(economic_factors, structural_factors, governance_factors)
missing_vars <- all_vars[!all_vars %in% names(panel_CP9623)]

if (length(missing_vars) > 0) {
  cat("警告: 以下變數不存在於資料中:\n")
  cat(paste(missing_vars, collapse = ", "), "\n\n")
}

available_vars <- all_vars[all_vars %in% names(panel_CP9623)]
cat("✓ 可用變數數量:", length(available_vars), "/", length(all_vars), "\n\n")

# ============================================================================
# 步驟1: 對三類變數執行PCA（使用全時期資料）
# ============================================================================

cat(rep("=", 80), "\n")
cat("步驟1: PCA分析（全時期資料）\n")
cat(rep("=", 80), "\n\n")

# PCA分析函數
perform_full_pca <- function(data, vars, dimension_name) {
  # 檢查變數可用性
  available_vars <- vars[vars %in% names(data)]
  if (length(available_vars) == 0) {
    cat("錯誤:", dimension_name, "- 沒有可用變數\n")
    return(NULL)
  }
  
  # 移除缺失值
  clean_data <- data[, available_vars] %>% na.omit()
  
  if (nrow(clean_data) < 10) {
    cat("警告:", dimension_name, "- 樣本數不足 (n =", nrow(clean_data), ")\n")
    return(NULL)
  }
  
  # 執行PCA
  pca_result <- PCA(clean_data, scale.unit = TRUE, graph = FALSE)
  
  # Kaiser法則: 特徵值 >= 1
  n_components <- sum(pca_result$eig[, 1] >= 1.0)
  
  # 顯示結果
  cat("【", dimension_name, "】\n", sep = "")
  cat("- 有效樣本數:", nrow(clean_data), "\n")
  cat("- 可用變數:", length(available_vars), "/", length(vars), "\n")
  cat("- 保留主成分數 (Kaiser法則):", n_components, "\n")
  cat("- 累積解釋變異:", round(sum(pca_result$eig[1:n_components, 2]), 2), "%\n\n")
  
  # 顯示特徵值表
  cat("  前5個主成分特徵值:\n")
  eig_table <- data.frame(
    PC = paste0("PC", 1:min(5, nrow(pca_result$eig))),
    Eigenvalue = round(pca_result$eig[1:min(5, nrow(pca_result$eig)), 1], 3),
    Variance = round(pca_result$eig[1:min(5, nrow(pca_result$eig)), 2], 2),
    Cumulative = round(pca_result$eig[1:min(5, nrow(pca_result$eig)), 3], 2)
  )
  print(kable(eig_table))
  cat("\n")
  
  return(list(
    pca_obj = pca_result,
    n_components = n_components,
    available_vars = available_vars
  ))
}

# 執行三個維度的PCA
pca_economic <- perform_full_pca(panel_CP9623, economic_factors, "經濟維度")
pca_structural <- perform_full_pca(panel_CP9623, structural_factors, "結構維度")
pca_governance <- perform_full_pca(panel_CP9623, governance_factors, "治理維度")

# ============================================================================
# 步驟2: 將主成分加入資料框
# ============================================================================

cat(rep("=", 80), "\n")
cat("步驟2: 將主成分加入資料框\n")
cat(rep("=", 80), "\n\n")

# 複製原始資料
panel_with_pca <- panel_CP9623

# 加入主成分的函數
add_pca_scores <- function(data, pca_result, prefix) {
  if (is.null(pca_result)) {
    cat("跳過", prefix, "維度（PCA結果為空）\n")
    return(data)
  }
  
  vars <- pca_result$available_vars
  n_pc <- pca_result$n_components
  
  # 找出完整案例的索引
  clean_idx <- complete.cases(data[, vars])
  
  # 加入主成分分數
  for (i in 1:n_pc) {
    col_name <- paste0(prefix, "_PC", i)
    data[[col_name]] <- NA
    data[[col_name]][clean_idx] <- pca_result$pca_obj$ind$coord[, i]
  }
  
  cat("✓", prefix, "維度: 新增", n_pc, "個主成分 (PC1-PC", n_pc, ")\n", sep = "")
  return(data)
}

# 依序加入三個維度的主成分
panel_with_pca <- add_pca_scores(panel_with_pca, pca_economic, "econ")
panel_with_pca <- add_pca_scores(panel_with_pca, pca_structural, "struct")
panel_with_pca <- add_pca_scores(panel_with_pca, pca_governance, "gov")

cat("\n主成分已成功加入資料框！\n\n")

# ============================================================================
# 步驟3: 決定最佳分群數
# ============================================================================

cat(rep("=", 80), "\n")
cat("步驟3: 決定最佳分群數\n")
cat(rep("=", 80), "\n\n")

# 收集所有主成分欄位
pc_cols <- grep("^(econ|struct|gov)_PC[0-9]", names(panel_with_pca), value = TRUE)
cat("用於分群的主成分:", paste(pc_cols, collapse = ", "), "\n\n")

# 使用1996年資料測試最佳k值
year_1996 <- panel_with_pca %>% filter(year == 1996)
test_data <- as.matrix(year_1996[, pc_cols, drop = FALSE])
test_data <- test_data[complete.cases(test_data), ]

cat("測試資料 (1996年):\n")
cat("- 樣本數:", nrow(test_data), "\n")
cat("- 主成分數:", ncol(test_data), "\n\n")

if (nrow(test_data) < 4) {
  cat("警告: 樣本數太少，使用預設 k = 3\n")
  optimal_k <- 3
} else {
  # 測試k值範圍
  max_k <- min(8, floor(nrow(test_data) / 5))
  
  cat("測試分群數範圍: 2 到", max_k, "\n")
  cat(rep("-", 50), "\n")
  
  sil_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    set.seed(123)
    km_test <- kmeans(test_data, centers = k, nstart = 25)
    sil_test <- silhouette(km_test$cluster, dist(test_data))
    sil_scores[k - 1] <- mean(sil_test[, 3])
    cat(sprintf("k = %d | 輪廓係數 = %.3f", k, sil_scores[k - 1]))
    if (k == which.max(sil_scores) + 1) cat(" ← 最佳")
    cat("\n")
  }
  
  optimal_k <- which.max(sil_scores) + 1
  cat(rep("-", 50), "\n")
  cat("✓ 最佳分群數: k =", optimal_k, 
      "(輪廓係數 =", round(max(sil_scores), 3), ")\n\n")
}

# ============================================================================
# 步驟4: 對三個年度進行分群分析
# ============================================================================

cat(rep("=", 80), "\n")
cat("步驟4: 分群分析 (", paste(years, collapse = ", "), ")\n", sep = "")
cat(rep("=", 80), "\n")

# 分群分析函數
cluster_by_year <- function(data, target_year, pc_cols, country_col, k = 3) {
  cat("\n", rep("-", 60), "\n", sep = "")
  cat("【", target_year, "年分群分析】\n", sep = "")
  cat(rep("-", 60), "\n")
  
  # 篩選年度資料
  year_data <- data %>% filter(year == target_year)
  
  # 準備分群資料（修正 select 語法）
  cols_to_select <- c(country_col, pc_cols)
  cluster_data <- year_data[, cols_to_select]
  cluster_data <- na.omit(cluster_data)
  
  cat("有效樣本數:", nrow(cluster_data), "\n")
  
  if (nrow(cluster_data) < k) {
    cat("錯誤: 樣本數不足以分成", k, "群\n")
    return(NULL)
  }
  
  # 準備矩陣並設定行名稱為國家代碼
  X <- as.matrix(cluster_data[, pc_cols])
  rownames(X) <- as.character(cluster_data[[country_col]])
  
  # K-means分群
  set.seed(123)
  km <- kmeans(X, centers = k, nstart = 50, iter.max = 200)
  
  # 計算輪廓係數
  sil <- silhouette(km$cluster, dist(X))
  
  # 顯示分群品質
  cat("分群數:", k, "\n")
  cat("平均輪廓係數:", round(mean(sil[, 3]), 3), "\n")
  cat("組間變異比例:", round(km$betweenss / km$totss * 100, 2), "%\n")
  cat("各群樣本數:", paste(table(km$cluster), collapse = ", "), "\n")
  
  # 視覺化
  p <- fviz_cluster(km, data = X, 
                    geom = "point",
                    ellipse.type = "convex",
                    main = paste(target_year, "年國家分群"),
                    ggtheme = theme_minimal())
  print(p)
  
  return(list(
    year = target_year,
    country_id = cluster_data[[country_col]],
    clusters = km$cluster,
    centers = km$centers,
    silhouette = mean(sil[, 3]),
    n_countries = nrow(cluster_data)
  ))
}

# 對三個年度執行分群
clustering_results <- list()

for (year in years) {
  result <- cluster_by_year(panel_with_pca, year, pc_cols, country_id_col, k = optimal_k)
  if (!is.null(result)) {
    clustering_results[[as.character(year)]] <- result
  }
}

# ============================================================================
# 步驟5: 各年度各群的自變數特徵分析
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("步驟5: 各群國家的自變數特徵分析\n")
cat(rep("=", 80), "\n")

# 分析各群特徵
analyze_cluster_features <- function(data, cluster_result, country_col) {
  target_year <- cluster_result$year
  
  # 合併分群資訊
  year_data <- data %>% 
    filter(year == target_year) %>%
    mutate(cluster = NA_integer_)
  
  # 將 country_col 轉換為一般向量（避免 pseries 問題）
  country_vector <- as.character(year_data[[country_col]])
  
  # 將分群結果對應回資料
  for (i in seq_along(cluster_result$country_id)) {
    country_code <- as.character(cluster_result$country_id[i])
    cluster_num <- cluster_result$clusters[i]
    year_data$cluster[country_vector == country_code] <- cluster_num
  }
  
  # 移除未分群的觀察值
  year_data <- year_data %>% filter(!is.na(cluster))
  
  # 整理所有自變數
  all_factors <- c(economic_factors, structural_factors, governance_factors)
  available_factors <- all_factors[all_factors %in% names(year_data)]
  
  # 計算各群的中位數
  cluster_profile <- year_data %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(all_of(available_factors), 
             ~median(., na.rm = TRUE),
             .names = "{.col}")
    ) %>%
    arrange(cluster)
  
  cat("\n【", target_year, "年 - 各群自變數中位數】\n", sep = "")
  print(kable(cluster_profile, digits = 3))
  
  return(cluster_profile)
}

# 分析三個年度
cluster_profiles <- list()
for (year in years) {
  if (as.character(year) %in% names(clustering_results)) {
    cluster_profiles[[as.character(year)]] <- 
      analyze_cluster_features(panel_with_pca, 
                               clustering_results[[as.character(year)]], 
                               country_id_col)
  }
}
# ============================================================================
# 步驟6: 儲存結果
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("步驟6: 儲存分析結果\n")
cat(rep("=", 80), "\n\n")

final_results <- list(
  # PCA資訊
  pca_economic = pca_economic,
  pca_structural = pca_structural,
  pca_governance = pca_governance,
  
  # 帶有主成分的資料
  data_with_pca = panel_with_pca,
  
  # 分群結果
  optimal_k = optimal_k,
  clustering_results = clustering_results,
  
  # 各群特徵
  cluster_profiles = cluster_profiles
)

saveRDS(final_results, "pca_clustering_analysis.rds")
cat("✓ 完整結果已儲存至: pca_clustering_analysis.rds\n\n")

# 顯示摘要
cat(rep("=", 80), "\n")
cat("分析完成摘要\n")
cat(rep("=", 80), "\n")
cat("主成分數量:\n")
cat("  - 經濟維度:", ifelse(is.null(pca_economic), 0, pca_economic$n_components), "\n")
cat("  - 結構維度:", ifelse(is.null(pca_structural), 0, pca_structural$n_components), "\n")
cat("  - 治理維度:", ifelse(is.null(pca_governance), 0, pca_governance$n_components), "\n")
cat("\n分群數:", optimal_k, "\n")
cat("分析年度:", paste(years, collapse = ", "), "\n")
cat("\n✓ 分析流程完畢！\n")

# ============================================================================
# PCA 分群分析結果視覺化程式碼
# ============================================================================

# 載入必要套件
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(knitr)
library(kableExtra)
library(gridExtra)
library(plotly)

# 設定圖表輸出資料夾
output_dir <- "output_figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ============================================================================
# 第一部分：PCA 解釋變異量圖表
# ============================================================================

cat("正在生成 PCA 解釋變異量圖表...\n")

# 設定圖表參數
png(file.path(output_dir, "01_pca_variance_explained.png"), 
    width = 1800, height = 600, res = 120)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))

# 經濟維度
fviz_eig(pca_clustering_analysis$pca_economic$pca_obj, 
         main = "經濟維度 PCA 解釋變異量",
         addlabels = TRUE,
         ylim = c(0, 50),
         barfill = "#00AFBB",
         barcolor = "#00AFBB")

# 結構維度
fviz_eig(pca_clustering_analysis$pca_structural$pca_obj, 
         main = "結構維度 PCA 解釋變異量",
         addlabels = TRUE,
         ylim = c(0, 50),
         barfill = "#E7B800",
         barcolor = "#E7B800")

# 治理維度
fviz_eig(pca_clustering_analysis$pca_governance$pca_obj, 
         main = "治理維度 PCA 解釋變異量",
         addlabels = TRUE,
         ylim = c(0, 100),
         barfill = "#FC4E07",
         barcolor = "#FC4E07")

dev.off()

# ============================================================================
# 第二部分：變數貢獻度圖
# ============================================================================

cat("正在生成變數貢獻度圖表...\n")

# 經濟維度變數貢獻
png(file.path(output_dir, "02_economic_var_contrib.png"), 
    width = 1200, height = 800, res = 120)
fviz_contrib(pca_clustering_analysis$pca_economic$pca_obj, 
             choice = "var", axes = 1:2,
             title = "經濟變數對 PC1-PC2 的貢獻",
             fill = "#00AFBB",
             color = "#00AFBB")
dev.off()

# 結構維度變數貢獻
png(file.path(output_dir, "03_structural_var_contrib.png"), 
    width = 1200, height = 800, res = 120)
fviz_contrib(pca_clustering_analysis$pca_structural$pca_obj, 
             choice = "var", axes = 1:2,
             title = "結構變數對 PC1-PC2 的貢獻",
             fill = "#E7B800",
             color = "#E7B800")
dev.off()

# 治理維度變數貢獻
png(file.path(output_dir, "04_governance_var_contrib.png"), 
    width = 1200, height = 800, res = 120)
fviz_contrib(pca_clustering_analysis$pca_governance$pca_obj, 
             choice = "var", axes = 1,
             title = "治理變數對 PC1 的貢獻",
             fill = "#FC4E07",
             color = "#FC4E07")
dev.off()

# ============================================================================
# 第三部分：變數相關圖 (Correlation Circle)
# ============================================================================

cat("正在生成變數相關圖...\n")

# 經濟維度
png(file.path(output_dir, "05_economic_var_circle.png"), 
    width = 1000, height = 1000, res = 120)
fviz_pca_var(pca_clustering_analysis$pca_economic$pca_obj,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "經濟維度變數相關圖")
dev.off()

# 結構維度
png(file.path(output_dir, "06_structural_var_circle.png"), 
    width = 1000, height = 1000, res = 120)
fviz_pca_var(pca_clustering_analysis$pca_structural$pca_obj,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "結構維度變數相關圖")
dev.off()

# 治理維度（只有一個PC，繪製條形圖）
png(file.path(output_dir, "07_governance_var_circle.png"), 
    width = 1000, height = 800, res = 120)
fviz_pca_var(pca_clustering_analysis$pca_governance$pca_obj,
             axes = c(1, 1),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "治理維度變數相關")
dev.off()

# ============================================================================
# 第四部分：分群結果散佈圖（三個年度）
# ============================================================================

cat("正在生成分群散佈圖...\n")

# 定義繪圖函數
plot_cluster_scatter <- function(year, dimension = "economic") {
  year_char <- as.character(year)
  
  # 取得該年度資料
  year_data <- pca_clustering_analysis$data_with_pca %>%
    filter(year == !!year)
  
  # 合併分群結果
  clusters <- pca_clustering_analysis$clustering_results[[year_char]]
  year_data$cluster <- NA
  country_vector <- as.character(year_data$countrycode)
  
  for (i in seq_along(clusters$country_id)) {
    country_code <- as.character(clusters$country_id[i])
    cluster_num <- clusters$clusters[i]
    year_data$cluster[country_vector == country_code] <- cluster_num
  }
  
  year_data <- year_data %>% filter(!is.na(cluster))
  
  # 根據維度選擇變數
  if (dimension == "economic") {
    pc1 <- "econ_PC1"
    pc2 <- "econ_PC2"
    title <- paste(year, "年經濟維度分群 (PC1 vs PC2)")
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A")
  } else if (dimension == "structural") {
    pc1 <- "struct_PC1"
    pc2 <- "struct_PC2"
    title <- paste(year, "年結構維度分群 (PC1 vs PC2)")
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A")
  } else {
    pc1 <- "gov_PC1"
    pc2 <- "econ_PC1"  # 治理只有一個PC，用經濟PC1作為第二軸
    title <- paste(year, "年治理維度分群 (PC1 vs 經濟PC1)")
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A")
  }
  
  # 繪製散佈圖
  p <- ggplot(year_data, aes(x = .data[[pc1]], y = .data[[pc2]], 
                             color = factor(cluster))) +
    geom_point(size = 3, alpha = 0.6) +
    stat_ellipse(level = 0.95, linewidth = 1) +
    scale_color_manual(values = colors) +
    labs(title = title,
         x = gsub("_", " ", pc1),
         y = gsub("_", " ", pc2),
         color = "群組",
         subtitle = paste("輪廓係數:", 
                          round(clusters$silhouette, 3),
                          "| 國家數:", clusters$n_countries)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(p)
}

# 繪製三個年度的經濟維度分群
years <- c(1996, 2013, 2023)

for (year in years) {
  # 經濟維度
  p <- plot_cluster_scatter(year, "economic")
  ggsave(file.path(output_dir, 
                   paste0("08_cluster_economic_", year, ".png")),
         plot = p, width = 10, height = 8, dpi = 120)
  
  # 結構維度
  p <- plot_cluster_scatter(year, "structural")
  ggsave(file.path(output_dir, 
                   paste0("09_cluster_structural_", year, ".png")),
         plot = p, width = 10, height = 8, dpi = 120)
  
  # 治理維度
  p <- plot_cluster_scatter(year, "governance")
  ggsave(file.path(output_dir, 
                   paste0("10_cluster_governance_", year, ".png")),
         plot = p, width = 10, height = 8, dpi = 120)
}

# ============================================================================
# 第五部分：三維度整合散佈圖
# ============================================================================

cat("正在生成三維度整合散佈圖...\n")

for (year in years) {
  year_char <- as.character(year)
  
  # 準備資料
  year_data <- pca_clustering_analysis$data_with_pca %>%
    filter(year == !!year)
  
  clusters <- pca_clustering_analysis$clustering_results[[year_char]]
  year_data$cluster <- NA
  country_vector <- as.character(year_data$countrycode)
  
  for (i in seq_along(clusters$country_id)) {
    country_code <- as.character(clusters$country_id[i])
    cluster_num <- clusters$clusters[i]
    year_data$cluster[country_vector == country_code] <- cluster_num
  }
  
  year_data <- year_data %>% filter(!is.na(cluster))
  
  # 3D散佈圖
  p <- plot_ly(year_data,
               x = ~econ_PC1, y = ~struct_PC1, z = ~gov_PC1,
               color = ~factor(cluster),
               colors = c("#E41A1C", "#377EB8", "#4DAF4A"),
               text = ~paste("國家:", countryname, 
                             "<br>群組:", cluster),
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 5)) %>%
    layout(title = paste(year, "年三維度 PCA 分群結果"),
           scene = list(
             xaxis = list(title = "經濟 PC1"),
             yaxis = list(title = "結構 PC1"),
             zaxis = list(title = "治理 PC1")
           ))
  
  # 儲存為 HTML
  htmlwidgets::saveWidget(p, 
                          file.path(output_dir, 
                                    paste0("11_cluster_3d_", year, ".html")))
}

# ============================================================================
# 第六部分：分群特徵熱圖
# ============================================================================

cat("正在生成分群特徵熱圖...\n")

# 準備熱圖函數
plot_cluster_heatmap <- function(year) {
  year_char <- as.character(year)
  profile <- pca_clustering_analysis$cluster_profiles[[year_char]]
  
  # 選擇要顯示的變數（排除 n）
  vars_to_plot <- names(profile)[!names(profile) %in% c("cluster", "n")]
  
  # 轉換為矩陣
  profile_matrix <- as.matrix(profile[, vars_to_plot])
  rownames(profile_matrix) <- paste("群組", profile$cluster)
  
  # 標準化（每個變數）
  profile_scaled <- scale(profile_matrix)
  
  # 繪製熱圖
  png(file.path(output_dir, paste0("12_cluster_heatmap_", year, ".png")), 
      width = 1400, height = 800, res = 120)
  
  heatmap(t(profile_scaled),
          Colv = NA,
          Rowv = NA,
          scale = "none",
          col = colorRampPalette(c("blue", "white", "red"))(50),
          main = paste(year, "年各群組特徵熱圖（標準化）"),
          xlab = "群組",
          ylab = "變數",
          margins = c(8, 15),
          cexRow = 0.8,
          cexCol = 1.2)
  
  dev.off()
}

for (year in years) {
  plot_cluster_heatmap(year)
}

# ============================================================================
# 第七部分：分群規模變化圖
# ============================================================================

cat("正在生成分群規模變化圖...\n")

# 準備資料
cluster_size_data <- data.frame(
  year = integer(),
  cluster = integer(),
  n = integer()
)

for (year in years) {
  year_char <- as.character(year)
  profile <- pca_clustering_analysis$cluster_profiles[[year_char]]
  
  temp_data <- data.frame(
    year = year,
    cluster = as.numeric(as.character(profile$cluster)),
    n = profile$n
  )
  
  cluster_size_data <- rbind(cluster_size_data, temp_data)
}

# 繪製折線圖
p <- ggplot(cluster_size_data, aes(x = year, y = n, 
                                   color = factor(cluster),
                                   group = cluster)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  labs(title = "各群組國家數量變化 (1996-2023)",
       x = "年份",
       y = "國家數量",
       color = "群組") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(output_dir, "13_cluster_size_trend.png"),
       plot = p, width = 10, height = 6, dpi = 120)

# ============================================================================
# 第八部分：變數載荷表格
# ============================================================================

cat("正在生成變數載荷表格...\n")

# 經濟維度
econ_loadings <- as.data.frame(
  pca_clustering_analysis$pca_economic$pca_obj$var$coord[, 1:2]
)
econ_loadings$變數 <- rownames(econ_loadings)
econ_loadings <- econ_loadings[, c("變數", "Dim.1", "Dim.2")]
colnames(econ_loadings) <- c("變數", "PC1", "PC2")

write.csv(econ_loadings, 
          file.path(output_dir, "14_economic_loadings.csv"),
          row.names = FALSE)

# 結構維度
struct_loadings <- as.data.frame(
  pca_clustering_analysis$pca_structural$pca_obj$var$coord[, 1:2]
)
struct_loadings$變數 <- rownames(struct_loadings)
struct_loadings <- struct_loadings[, c("變數", "Dim.1", "Dim.2")]
colnames(struct_loadings) <- c("變數", "PC1", "PC2")

write.csv(struct_loadings, 
          file.path(output_dir, "15_structural_loadings.csv"),
          row.names = FALSE)

# 治理維度
gov_loadings <- as.data.frame(
  pca_clustering_analysis$pca_governance$pca_obj$var$coord[, 1, drop = FALSE]
)
gov_loadings$變數 <- rownames(gov_loadings)
gov_loadings <- gov_loadings[, c("變數", "Dim.1")]
colnames(gov_loadings) <- c("變數", "PC1")

write.csv(gov_loadings, 
          file.path(output_dir, "16_governance_loadings.csv"),
          row.names = FALSE)

# ============================================================================
# 第九部分：分群特徵比較表
# ============================================================================

cat("正在生成分群特徵比較表...\n")

for (year in years) {
  year_char <- as.character(year)
  profile <- pca_clustering_analysis$cluster_profiles[[year_char]]
  
  # 轉換為資料框
  profile_df <- as.data.frame(profile)
  
  # 四捨五入
  profile_df[, -c(1, 2)] <- round(profile_df[, -c(1, 2)], 3)
  
  # 儲存為 CSV
  write.csv(profile_df, 
            file.path(output_dir, 
                      paste0("17_cluster_profile_", year, ".csv")),
            row.names = FALSE)
  
  # 產生 HTML 表格
  html_table <- kable(profile_df, 
                      format = "html",
                      caption = paste(year, "年各群組特徵（中位數）")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE)
  
  save_kable(html_table, 
             file.path(output_dir, 
                       paste0("18_cluster_profile_", year, ".html")))
}

# ============================================================================
# 第十部分：產生摘要報告
# ============================================================================

cat("正在生成摘要報告...\n")

# 建立摘要文件
summary_text <- paste0(
  "PCA 分群分析結果摘要\n",
  "==========================================\n\n",
  "分析日期: ", Sys.Date(), "\n\n",
  "一、PCA 降維結果\n",
  "  - 經濟維度: 使用 ", 
  pca_clustering_analysis$pca_economic$n_components, " 個主成分\n",
  "  - 結構維度: 使用 ", 
  pca_clustering_analysis$pca_structural$n_components, " 個主成分\n",
  "  - 治理維度: 使用 ", 
  pca_clustering_analysis$pca_governance$n_components, " 個主成分\n\n",
  "二、最佳分群數: ", pca_clustering_analysis$optimal_k, "\n\n",
  "三、各年度分群品質（輪廓係數）\n"
)

for (year in years) {
  year_char <- as.character(year)
  silhouette <- pca_clustering_analysis$clustering_results[[year_char]]$silhouette
  n_countries <- pca_clustering_analysis$clustering_results[[year_char]]$n_countries
  
  summary_text <- paste0(summary_text,
                         "  - ", year, "年: ", 
                         round(silhouette, 4), 
                         " (", n_countries, " 個國家)\n")
}

summary_text <- paste0(summary_text, "\n",
                       "四、圖表清單\n",
                       "  01-07: PCA 基礎分析圖表\n",
                       "  08-10: 分群散佈圖（經濟、結構、治理維度）\n",
                       "  11: 三維度互動式圖表 (HTML)\n",
                       "  12: 分群特徵熱圖\n",
                       "  13: 分群規模變化圖\n",
                       "  14-16: 變數載荷表格 (CSV)\n",
                       "  17-18: 分群特徵比較表 (CSV & HTML)\n")

# 儲存摘要
writeLines(summary_text, 
           file.path(output_dir, "00_SUMMARY.txt"))

# ============================================================================
# 完成
# ============================================================================

cat("\n==========================================\n")
cat("所有圖表已成功產製！\n")
cat("輸出資料夾:", output_dir, "\n")
cat("==========================================\n\n")

# 列出所有產製的檔案
files <- list.files(output_dir)
cat("產製檔案清單:\n")
for (i in seq_along(files)) {
  cat(sprintf("%2d. %s\n", i, files[i]))
}


# ==========================================================================================================
# 使用線性迴歸模型設定模式,並進行多元共線性檢測
model_for_testing <- lm(partnership_shift_1 ~ xi + dip_age_std + dist_std + population_total_std_shift_1 + 
                          gdp_std_shift_1 + gdp_per_capita_std_shift_1 + 
                          china_ex_to_i_std_shift_1 + china_im_fr_i_std_shift_1 + 
                          exportdep_std_shift_1 + importdep_std_shift_1 + 
                          WGI + economy_shift_1 + arms_and_military_shift_1,
                        data = panel_CP9623)
car::vif(model_for_testing)

model_for_testing_2 <- lm(partnership_diff ~ xi + dip_age_std + dist_std + population_total_std_shift_1 + 
                            gdp_std_shift_1 +
                            china_ex_to_i_std_shift_1 + china_im_fr_i_std_shift_1 + 
                            exportdep_std_shift_1 + importdep_std_shift_1 + 
                            va_std_shift_1 + pv_std_shift_1 + 
                            ge_std_shift_1 + rl_std_shift_1 +
                            rq_std_shift_1 + cc_std_shift_1 + economy_shift_1 + arms_and_military_shift_1,
                          data = panel_CP9623)
car::vif(model_for_testing_2)

# 根據多元共線性檢測結果逐步移除高VIF變數
threshold <- 7
while(any(car::vif(model_for_testing) > threshold)) {
  vif_values <- car::vif(model_for_testing)
  highest_vif <- names(vif_values)[which.max(vif_values)]
  remaining_vars <- setdiff(names(model_for_testing$coefficients), c("(Intercept)", highest_vif))
  formula <- as.formula(paste("partnership ~", paste(remaining_vars, collapse = " + ")))
  model_for_testing <- lm(formula, data = panel_CP9623)
  print(car::vif(model_for_testing))
}
summary(model_for_testing)

threshold <- 7

# 將變數進行順序性轉換
panel_CP9623$partnership <- factor(panel_CP9623$partnership, ordered = TRUE)
panel_CP9623$partnership_diff <- factor(panel_CP9623$partnership_diff, ordered = TRUE)
panel_CP9623$xi <- factor(panel_CP9623$xi, ordered = TRUE)
panel_CP9623$economy <- factor(panel_CP9623$economy, ordered = TRUE)
panel_CP9623$arms_and_military <- factor(panel_CP9623$arms_and_military, ordered = TRUE)
panel_CP9623$travel <- factor(panel_CP9623$travel, ordered = TRUE)

# pglm: 所有自變數標準化並落後一年,但dist_std只落後
pglm_for_panel_CP9623 <- pglm(partnership2 ~ xi + age_std_shift_1 + dist_std_shift_1 + population_total_std_shift_1 + 
                                gdp_per_capita_shift_1 + 
                                china_im_fr_i_gdp_std_shift_1 + 
                                china_ex_to_i_gdp_std_shift_1 + 
                                WGI_std_shift_1 + 
                                ORG_shift_1 + BFTA_shift_1 + RCEP_shift_1 + WTO_shift_1,
                              panel_CP9623, effect = "twoways", model = "random", 
                              family = ordinal("probit"), R=6, panel="countrycode")
summary(pglm_for_panel_CP9623)

# pglm: 依變數做差分,所有自變數標準化且所有變數落後一年
pglm_for_panel_CP9623_diff <- pglm(partnership2_diff ~ xi + age_std_shift_1 + dist_std_shift_1 + population_total_std_shift_1 + 
                                     gdp_per_capita_shift_1 + 
                                     china_im_fr_i_gdp_std_shift_1 + 
                                     china_ex_to_i_gdp_std_shift_1 + 
                                     WGI_std_shift_1 + 
                                     ORG_shift_1 + BFTA_shift_1 + RCEP_shift_1 + WTO_shift_1,
                                   panel_CP9623, effect = "twoways", model = "random", 
                                   family = ordinal("probit"), R=6, panel="countrycode")
summary(pglm_for_panel_CP9623_diff)

# pglm: 以xi作為調節變項
pglm_for_panel_CP9623_xi <- pglm(partnership2 ~ xi*(age_std_shift_1 + dist_std_shift_1 + population_total_std_shift_1 + 
                                                      gdp_per_capita_shift_1 + 
                                                      china_im_fr_i_gdp_std_shift_1 + 
                                                      china_ex_to_i_gdp_std_shift_1 + 
                                                      WGI_std_shift_1 + 
                                                      ORG_shift_1 + BFTA_shift_1 + RCEP_shift_1 + WTO_shift_1), 
                                 panel_CP9623, effect = "twoways", model = "random",
                                 family = ordinal("probit"), R=6, panel="countrycode")
summary(pglm_for_panel_CP9623_xi)

# 執行EBA
sophisticated.eba <- eba(
  formula = partnership ~ xi | age + dist_std + population_total_std_shift_1 + 
    gdp_std_shift_1 +
    China_ex_to_i_std_shift_1 + China_im_fr_i_std_shift_1 + 
    exportdep_std_shift_1 + importdep_std_shift_1 + 
    VA_DCHN_std_shift_1 + PSV_DCHN_std_shift_1 + 
    GE_DCHN_std_shift_1 + RL_DCHN_std_shift_1 +
    WGI_DCHN_std_shift_1 + economy_shift_1 + arms_and_military_shift_1, 
  data = panel_CP0522, k=0:3, level = 0.95,
  vif = 7, 
  reg.fun = pglm, family = ordinal("logit"), R=6, effect= "twoways", print.level = 3, 