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

# 自動檢查、安裝並載入套件
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("正在安裝:", pkg, "\n")
    install.packages(pkg, lib = lib_path, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("\n所有套件已就緒！\n")

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
country_id_col <- "ccode"  # 可能是 "country", "iso3c" 等

# ============================================================================
# 步驟0: 資料驗證
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("步驟0: 資料驗證\n")
cat(rep("=", 80), "\n\n")

# 檢查資料框是否存在
if (!exists("panel_CP9623")) {
  stop("錯誤: 找不到資料框 'panel_CP9623'。請先載入資料！")
}

# 顯示資料結構
cat("資料框基本資訊:\n")
cat("- 觀察值數量:", nrow(panel_CP9623), "\n")
cat("- 變數數量:", ncol(panel_CP9623), "\n")
cat("- 年度範圍:", paste(range(panel_CP9623$year, na.rm = TRUE), collapse = " - "), "\n\n")

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
  
  # 將分群結果對應回資料
  for (i in seq_along(cluster_result$country_id)) {
    country_code <- cluster_result$country_id[i]
    cluster_num <- cluster_result$clusters[i]
    year_data$cluster[year_data[[country_col]] == country_code] <- cluster_num
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