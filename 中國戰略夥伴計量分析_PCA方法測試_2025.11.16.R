# ============================================================================
# 套件管理
# ============================================================================

# 設定套件路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# 定義套件清單
packages <- c(
  # 核心套件
  "tidyverse", "magrittr",
  # 資料處理
  "plm", "haven", "readr", "readxl", "openxlsx", "writexl",
  # 統計分析
  "ExtremeBounds", "car", "ordinal", "MASS", "mvProbit", "psych", 
  "pglm", "pastecs",
  # 因素分析
  "FactoMineR", "factoextra", "GPArotation",
  # 工具
  "usdm", "knitr", "kableExtra", "gridExtra", "gapminder"
)

# 檢查並安裝缺少的套件
cat("檢查套件...\n")
missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  cat("安裝", length(missing), "個套件:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, dependencies = TRUE, lib = lib_path)
} else {
  cat("所有套件已安裝\n")
}

# 載入套件
cat("\n載入套件...\n")
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

cat("完成！已載入", length(packages), "個套件\n\n")

# =================================================

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
# 前置設定
# ============================================================================

# 定義三類自變數
economic_factors <- c("gdp_std", "china_ex_to_i_std", "china_im_fr_i_std", 
                      "exportdep_std", "importdep_std", "population_total_std")

sanctional_factors <- c("trade_std", "arms_std", "military_std",
                        "financial", "travel_std")

governance_factors <- c("va_std", "psv_std", "ge_std",
                        "rq_std", "rl_std", "cc_std")

years <- c(1996, 2013, 2023)
country_id_col <- "countrycode"
outcome_vars <- c("partnership", "partnership2")

# ============================================================================
# 步驟0: 資料驗證與缺失值插補
# ============================================================================
cat("\n步驟0: 資料驗證與插補\n", rep("=", 80), "\n")

# 檢查資料
if (!exists("panel_CP9623")) stop("找不到資料框 'panel_CP9623'")
cat("原始資料 - 觀察值:", nrow(panel_CP9623), "| 國家數:", 
    length(unique(panel_CP9623[[country_id_col]])), "\n\n")

# 定義所有變數
all_vars <- c(economic_factors, sanctional_factors, governance_factors)

# 缺失值初步分析
cat("缺失值分析:\n", rep("-", 80), "\n")
overall_missing <- sapply(panel_CP9623[, all_vars], function(x) sum(is.na(x)))
cat("總缺失值數:", sum(overall_missing), "\n")
if (sum(overall_missing) > 0) {
  cat("缺失分佈:", paste(names(overall_missing[overall_missing > 0]), 
                     collapse = ", "), "\n\n")
}

# ============================================================================
# 插補執行（三階段）
# ============================================================================
panel_imputed <- panel_CP9623

# 階段1: 時間序列線性插補
cat("階段1: 時間序列線性插補\n")
for (country in unique(panel_imputed[[country_id_col]])) {
  country_idx <- which(panel_imputed[[country_id_col]] == country)
  
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

# 階段3: 全局中位數插補（處理異常值）
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

# ============================================================================
# 清理與驗證
# ============================================================================
cat("\n清理異常觀察值:\n", rep("-", 80), "\n")
cat("插補後觀察值數:", nrow(panel_imputed), "\n")

# 移除 countrycode 或 year 為 NA 的觀察值
panel_common <- panel_imputed %>%
  filter(!is.na(!!sym(country_id_col)) & !is.na(year))

cat("清理後觀察值數:", nrow(panel_common), 
    "(-", nrow(panel_imputed) - nrow(panel_common), ")\n")
cat("最終國家數:", length(unique(panel_common[[country_id_col]])), "國\n")

# 最終驗證
final_missing <- sum(sapply(panel_common[, all_vars], function(x) sum(is.na(x))))
complete_rate <- round(sum(complete.cases(panel_common[, all_vars])) / 
                         nrow(panel_common) * 100, 2)

cat("\n", rep("=", 80), "\n")
cat("✓ 插補完成!\n")
cat("  總樣本:", nrow(panel_common), "觀察值\n")
cat("  涵蓋國家:", length(unique(panel_common[[country_id_col]])), "國\n")
cat("  剩餘缺失值:", final_missing, "\n")
cat("  完整率:", complete_rate, "%\n")
cat(rep("=", 80), "\n\n")
# ============================================================================
# 中國夥伴外交決策多樣化分析
# 研究問題：1996、2013、2023年，各夥伴層級內的國家特徵是否愈趨多樣化？
# ============================================================================

library(FactoMineR)
library(dplyr)
library(cluster)
library(knitr)
library(tidyr)
library(ggplot2)

# ============================================================================
# 步驟0: 資料準備與變數確認
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("中國夥伴外交多樣化分析\n")
cat(rep("=", 80), "\n\n")

# 確認必要變數存在
required_objects <- c("CP9623", "economic_factors", "sanctional_factors", 
                      "governance_factors", "outcome_vars")
missing <- required_objects[!sapply(required_objects, exists)]
if (length(missing) > 0) {
  stop("缺少必要物件: ", paste(missing, collapse = ", "))
}

# 設定分析參數
analysis_years <- c(1996, 2013, 2023)
country_id_col <- "ccode"  # 根據您的資料調整
partnership_col <- "partnership_level"  # 夥伴關係層級欄位名稱

cat("分析年份:", paste(analysis_years, collapse = ", "), "\n")
cat("資料筆數:", nrow(CP9623), "\n")
cat("經濟變數:", length(economic_factors), "個\n")
cat("制裁變數:", length(sanctional_factors), "個\n")
cat("治理變數:", length(governance_factors), "個\n\n")

# ============================================================================
# 步驟1: PCA降維
# ============================================================================

cat("步驟1: PCA降維\n", rep("-", 80), "\n")

perform_pca <- function(data, vars, name, method = "kaiser", manual_n = NULL) {
  clean_data <- data[, vars] %>% na.omit()
  
  if (nrow(clean_data) < 5) {
    cat("【", name, "】樣本不足，跳過\n", sep = "")
    return(NULL)
  }
  
  pca_result <- PCA(clean_data, scale.unit = TRUE, graph = FALSE)
  
  # 決定主成分數量
  if (!is.null(manual_n)) {
    n_comp <- manual_n
  } else if (method == "kaiser") {
    n_comp <- sum(pca_result$eig[, 1] >= 1.0)
  } else if (method == "variance70") {
    n_comp <- which(pca_result$eig[, 3] >= 70)[1]
  }
  
  n_comp <- max(1, min(n_comp, ncol(clean_data)))
  
  cat("【", name, "】", sep = "")
  cat("樣本:", nrow(clean_data), "| 變數:", length(vars), 
      "| 主成分:", n_comp, 
      "| 累積變異:", round(pca_result$eig[n_comp, 3], 1), "%\n")
  
  return(list(
    pca_obj = pca_result, 
    n_components = n_comp, 
    available_vars = vars,
    valid_rows = as.integer(rownames(clean_data))
  ))
}

# 執行PCA（使用全部資料）
pca_economic <- perform_pca(CP9623, economic_factors, "經濟維度", manual_n = 3)
pca_sanctional <- perform_pca(CP9623, sanctional_factors, "制裁維度")
pca_governance <- perform_pca(CP9623, governance_factors, "治理維度")

# ============================================================================
# 步驟2: 將主成分加入原始資料
# ============================================================================

cat("\n步驟2: 加入主成分\n", rep("-", 80), "\n")

add_pca_scores <- function(data, pca_result, prefix) {
  if (is.null(pca_result)) return(data)
  
  vars <- pca_result$available_vars
  n_pc <- pca_result$n_components
  clean_idx <- complete.cases(data[, vars])
  
  for (i in 1:n_pc) {
    col_name <- paste0(prefix, "_PC", i)
    data[[col_name]] <- NA_real_
    data[[col_name]][clean_idx] <- pca_result$pca_obj$ind$coord[, i]
  }
  
  cat("✓", prefix, ":", n_pc, "個主成分已加入\n")
  return(data)
}

CP9623_pca <- CP9623 %>%
  add_pca_scores(pca_economic, "econ") %>%
  add_pca_scores(pca_sanctional, "sanct") %>%
  add_pca_scores(pca_governance, "gov")

# 取得所有主成分欄位名稱
pc_cols <- grep("^(econ|sanct|gov)_PC[0-9]", names(CP9623_pca), value = TRUE)
cat("\n主成分總數:", length(pc_cols), "\n")

# ============================================================================
# 步驟3: 各年度各層級分群分析
# ============================================================================

cat("\n步驟3: 各年度各層級分群分析\n", rep("-", 80), "\n")

cluster_by_year_level <- function(data, year, level, pc_cols, k_range = 2:5) {
  cat("\n【", year, "年 - ", level, "層級】\n", sep = "")
  
  # 篩選該年該層級的資料
  subset_data <- data %>%
    filter(year == !!year, 
           !!sym(partnership_col) == !!level) %>%
    select(all_of(c(country_id_col, pc_cols))) %>%
    na.omit()
  
  n_countries <- nrow(subset_data)
  
  if (n_countries < 10) {
    cat("  樣本數不足 (n=", n_countries, ")，跳過分群\n", sep = "")
    return(NULL)
  }
  
  # 準備分群資料
  X <- as.matrix(subset_data[, pc_cols])
  rownames(X) <- as.character(subset_data[[country_id_col]])
  
  # 決定最佳k值
  max_k <- min(max(k_range), floor(n_countries / 5))
  if (max_k < 2) {
    cat("  無法進行分群 (max_k < 2)\n")
    return(NULL)
  }
  
  sil_scores <- numeric(max_k - 1)
  for (k in 2:max_k) {
    set.seed(123)
    km <- kmeans(X, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(X))
    sil_scores[k - 1] <- mean(sil[, 3])
  }
  
  optimal_k <- which.max(sil_scores) + 1
  
  # 最終分群
  set.seed(123)
  km_final <- kmeans(X, centers = optimal_k, nstart = 50)
  sil_final <- silhouette(km_final$cluster, dist(X))
  
  cat("  國家數:", n_countries, "| 最佳k:", optimal_k, 
      "| 輪廓係數:", round(mean(sil_final[, 3]), 3), "\n")
  cat("  各群樣本數:", paste(table(km_final$cluster), collapse = ", "), "\n")
  
  return(list(
    year = year,
    level = level,
    n_countries = n_countries,
    optimal_k = optimal_k,
    country_id = subset_data[[country_id_col]],
    clusters = km_final$cluster,
    silhouette = mean(sil_final[, 3]),
    bss_tss = km_final$betweenss / km_final$totss,
    cluster_sizes = as.vector(table(km_final$cluster))
  ))
}

# 取得所有夥伴層級
partnership_levels <- unique(CP9623[[partnership_col]])
partnership_levels <- partnership_levels[!is.na(partnership_levels)]

cat("\n夥伴關係層級:", paste(partnership_levels, collapse = ", "), "\n")

# 對每年每層級進行分群
clustering_results <- list()

for (year in analysis_years) {
  for (level in partnership_levels) {
    key <- paste0("Y", year, "_L", level)
    clustering_results[[key]] <- cluster_by_year_level(
      CP9623_pca, year, level, pc_cols
    )
  }
}

# 移除NULL結果
clustering_results <- clustering_results[!sapply(clustering_results, is.null)]

# ============================================================================
# 步驟4: 計算多樣化指標
# ============================================================================

cat("\n步驟4: 多樣化指標計算\n", rep("-", 80), "\n")

calculate_diversity_metrics <- function(cluster_result) {
  if (is.null(cluster_result)) return(NULL)
  
  k <- cluster_result$optimal_k
  sizes <- cluster_result$cluster_sizes
  n <- cluster_result$n_countries
  
  # 指標1: 分群數（k值）
  num_clusters <- k
  
  # 指標2: 標準化熵（0-1，越高越多樣）
  props <- sizes / n
  entropy <- -sum(props * log(props))
  max_entropy <- log(k)
  normalized_entropy <- entropy / max_entropy
  
  # 指標3: Simpson多樣性指數（0-1，越高越多樣）
  simpson <- 1 - sum(props^2)
  
  # 指標4: 有效群數（Effective number of clusters）
  effective_k <- exp(entropy)
  
  # 指標5: 變異係數（群大小的離散程度）
  cv <- sd(sizes) / mean(sizes)
  
  return(list(
    year = cluster_result$year,
    level = cluster_result$level,
    n_countries = n,
    num_clusters = num_clusters,
    silhouette = cluster_result$silhouette,
    bss_tss = cluster_result$bss_tss,
    normalized_entropy = normalized_entropy,
    simpson_index = simpson,
    effective_k = effective_k,
    cv_cluster_size = cv
  ))
}

diversity_metrics <- lapply(clustering_results, calculate_diversity_metrics)
diversity_df <- do.call(rbind, lapply(diversity_metrics, as.data.frame))

# 顯示結果
cat("\n多樣化指標摘要：\n")
diversity_summary <- diversity_df %>%
  arrange(level, year) %>%
  select(year, level, n_countries, num_clusters, 
         normalized_entropy, simpson_index, effective_k)

print(kable(diversity_summary, digits = 3))

# ============================================================================
# 步驟5: 趨勢分析與視覺化
# ============================================================================

cat("\n步驟5: 趨勢分析\n", rep("-", 80), "\n")

# 計算各層級的多樣化趨勢
diversity_trends <- diversity_df %>%
  group_by(level) %>%
  arrange(year) %>%
  summarise(
    entropy_1996 = normalized_entropy[year == 1996],
    entropy_2023 = normalized_entropy[year == 2023],
    entropy_change = entropy_2023 - entropy_1996,
    simpson_1996 = simpson_index[year == 1996],
    simpson_2023 = simpson_index[year == 2023],
    simpson_change = simpson_2023 - simpson_1996,
    k_1996 = num_clusters[year == 1996],
    k_2023 = num_clusters[year == 2023],
    k_change = k_2023 - k_1996,
    .groups = "drop"
  )

cat("\n各層級多樣化趨勢（1996→2023）：\n")
print(kable(diversity_trends, digits = 3))

# 整體趨勢
overall_trend <- diversity_df %>%
  group_by(year) %>%
  summarise(
    mean_entropy = mean(normalized_entropy, na.rm = TRUE),
    mean_simpson = mean(simpson_index, na.rm = TRUE),
    mean_k = mean(num_clusters, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n整體平均趨勢：\n")
print(kable(overall_trend, digits = 3))

# ============================================================================
# 步驟6: 儲存結果
# ============================================================================

cat("\n步驟6: 儲存結果\n", rep("-", 80), "\n")

final_results <- list(
  # 基本資訊
  analysis_years = analysis_years,
  partnership_levels = partnership_levels,
  
  # PCA結果
  pca_economic = pca_economic,
  pca_sanctional = pca_sanctional,
  pca_governance = pca_governance,
  
  # 完整資料
  data_with_pca = CP9623_pca,
  
  # 分群結果
  clustering_results = clustering_results,
  
  # 多樣化分析
  diversity_metrics = diversity_df,
  diversity_trends = diversity_trends,
  overall_trend = overall_trend
)

saveRDS(final_results, "partnership_diversity_analysis.rds")
cat("✓ 結果已儲存至: partnership_diversity_analysis.rds\n\n")

# ============================================================================
# 結論摘要
# ============================================================================

cat(rep("=", 80), "\n")
cat("分析完成\n")
cat(rep("=", 80), "\n\n")

cat("研究問題：中國夥伴外交決策是否隨時間愈趨多樣化？\n\n")

cat("主要發現：\n")
for (i in 1:nrow(overall_trend)) {
  year <- overall_trend$year[i]
  cat(sprintf("%d年 - 平均熵: %.3f | 平均Simpson指數: %.3f | 平均分群數: %.1f\n",
              year, overall_trend$mean_entropy[i], 
              overall_trend$mean_simpson[i], overall_trend$mean_k[i]))
}

entropy_change_pct <- (overall_trend$mean_entropy[3] - overall_trend$mean_entropy[1]) / 
  overall_trend$mean_entropy[1] * 100

cat(sprintf("\n1996→2023 多樣化變化: %+.1f%%\n", entropy_change_pct))

if (entropy_change_pct > 10) {
  cat("結論：多樣化程度顯著增加 ✓\n")
} else if (entropy_change_pct < -10) {
  cat("結論：多樣化程度顯著減少\n")
} else {
  cat("結論：多樣化程度相對穩定\n")
}

cat(rep("=", 80), "\n")
# ============================================================================
# PCA 分群分析結果視覺化程式碼（簡化版）
# ============================================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(knitr)
library(kableExtra)
library(gridExtra)

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(knitr)
library(kableExtra)
library(gridExtra)
library(cluster)

# 設定圖表輸出資料夾
output_dir <- "output_figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 關閉所有現有圖形設備
graphics.off()

cat("\n", rep("=", 80), "\n")
cat("開始產製視覺化圖表\n")
cat(rep("=", 80), "\n\n")

# ============================================================================
# 1. PCA 解釋變異量圖
# ============================================================================
cat("1. 產製 PCA 解釋變異量圖...\n")

p1 <- fviz_eig(final_results$pca_economic$pca_obj, 
               main = "經濟維度",
               addlabels = TRUE, ylim = c(0, 50),
               barfill = "#00AFBB", barcolor = "#00AFBB")

p2 <- fviz_eig(final_results$pca_sanctional$pca_obj,
               main = "制裁維度",
               addlabels = TRUE, ylim = c(0, 50),
               barfill = "#E7B800", barcolor = "#E7B800")

p3 <- fviz_eig(final_results$pca_governance$pca_obj, 
               main = "治理維度",
               addlabels = TRUE, ylim = c(0, 50),
               barfill = "#FC4E07", barcolor = "#FC4E07")

tryCatch({
  png(file.path(output_dir, "01_pca_variance_explained.png"), 
      width = 1800, height = 600, res = 120)
  suppressWarnings(grid.arrange(p1, p2, p3, ncol = 3))
}, finally = {
  dev.off()
  Sys.sleep(0.5)
})

cat("✓ 已儲存: 01_pca_variance_explained.png\n\n")

# ============================================================================
# 2. 變數相關圖
# ============================================================================
cat("2. 產製變數相關圖...\n")

p1 <- fviz_pca_var(final_results$pca_economic$pca_obj,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, title = "經濟維度")

p2 <- fviz_pca_var(final_results$pca_sanctional$pca_obj,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, title = "制裁維度")

p3 <- fviz_pca_var(final_results$pca_governance$pca_obj,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, title = "治理維度")

tryCatch({
  png(file.path(output_dir, "02_variable_correlation.png"), 
      width = 1800, height = 600, res = 120)
  suppressWarnings(grid.arrange(p1, p2, p3, ncol = 3))
}, finally = {
  dev.off()
  Sys.sleep(0.5)
})

cat("✓ 已儲存: 02_variable_correlation.png\n\n")

# ============================================================================
# 3. 分群散佈圖
# ============================================================================

cat("3. 產製分群散佈圖...\n")

plot_yearly_clusters_3d <- function(year) {
  year_char <- as.character(year)
  
  year_data_raw <- dplyr::filter(
    final_results$data_with_pca_clusters,
    .data$year == year, !is.na(.data$cluster)
  )
  
  # 找出治理維度欄位：優先常見候選，若找不到再用關鍵字模糊匹配
  candidates <- c("pca_governance", "governance_PC1", "gov_PC1", "govern_PC1")
  governance_col <- candidates[candidates %in% names(year_data_raw)]
  if (length(governance_col) == 0) {
    # 模糊搜尋：名稱同時包含 "govern" 與 "pc1"（不分大小寫）
    nm <- names(year_data_raw)
    has_govern <- grepl("govern", nm, ignore.case = TRUE)
    has_pc1    <- grepl("pc1",    nm, ignore.case = TRUE)
    governance_col <- nm[has_govern & has_pc1]
  }
  if (length(governance_col) == 0) {
    stop("找不到治理維度欄位，請確認資料中的欄位名稱（例如 pca_governance / governance_PC1）。現有欄位：\n",
         paste(names(year_data_raw), collapse = ", "))
  }
  governance_col <- governance_col[1]  # 取第一個匹配
  
  # 建立繪圖資料，並過濾缺值
  year_data <- data.frame(
    econ_PC1  = as.numeric(year_data_raw$econ_PC1),
    sanct_PC1 = as.numeric(year_data_raw$sanct_PC1),
    gov_PC1   = as.numeric(year_data_raw[[governance_col]]),
    cluster   = as.factor(year_data_raw$cluster),
    check.names = FALSE
  )
  year_data <- year_data[stats::complete.cases(year_data), , drop = FALSE]
  
  if (nrow(year_data) == 0) {
    return(plotly::layout(
      plotly::plotly_empty(type = "scatter3d"),
      title = paste0(year, "年國家分群結果（無可繪資料）")
    ))
  }
  
  clusters <- final_results$clustering_results[[year_char]]
  subtitle_text <- if (!is.null(clusters)) {
    paste("輪廓係數:", round(clusters$silhouette, 3), "| 國家數:", clusters$n_countries)
  } else {
    "（無可用分群摘要）"
  }
  
  p <- plotly::plot_ly(
    data = year_data,
    x = ~econ_PC1, y = ~sanct_PC1, z = ~gov_PC1,
    color = ~cluster,
    colors = RColorBrewer::brewer.pal(8, "Set1"),
    type = "scatter3d", mode = "markers",
    marker = list(size = 4, opacity = 0.8)
  )
  
  p <- plotly::layout(
    p,
    title = paste0(year, "年國家分群結果<br><sup>", subtitle_text, "</sup>"),
    scene = list(
      xaxis = list(title = "經濟維度 PC1"),
      yaxis = list(title = "制裁維度 PC1"),
      zaxis = list(title = paste0("治理維度 PC1（欄位：", governance_col, "）")),
      aspectmode = "cube"
    ),
    legend = list(orientation = "h")
  )
  
  return(p)
}
years <- final_results$analysis_years
plots3d <- lapply(years, plot_yearly_clusters_3d)

# 個別年份各存一個 HTML
for (i in seq_along(years)) {
  htmlwidgets::saveWidget(
    plots3d[[i]],
    file = file.path(output_dir, paste0("03_clustering_results_3d_", years[i], ".html")),
    selfcontained = TRUE
  )
}

# 合併成一個頁面（橫向排列）
combo <- subplot(plots3d, nrows = 1, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.02)
htmlwidgets::saveWidget(
  combo,
  file = file.path(output_dir, "03_clustering_results_3d_all.html"),
  selfcontained = TRUE
)

cat("✓ 已儲存: 03_clustering_results.png\n\n")

cat("所有圖表產製完成！\n")
cat(rep("=", 80), "\n")

cat("4. 產製群組特徵熱圖...\n")
for (year in years) {
  year_char <- as.character(year)
  profile <- final_results$cluster_profiles[[year_char]]
  
  # 選擇主要變數（排除 n 和應變數）
  main_vars <- c(economic_factors, sanctional_factors, governance_factors)
  available_vars <- main_vars[main_vars %in% names(profile)]
  
  # 準備矩陣
  profile_matrix <- as.matrix(profile[, available_vars, drop = FALSE])
  rownames(profile_matrix) <- paste("群組", profile$cluster)
  
  # 標準化
  profile_scaled <- scale(profile_matrix)
  
  # 繪製熱圖
  png(file.path(output_dir, paste0("04_heatmap_", year, ".png")),
      width = 1400, height = 800, res = 120)
  
  heatmap(t(profile_scaled),
          Colv = NA, Rowv = NA,
          scale = "none",
          col = colorRampPalette(c("blue", "white", "red"))(50),
          main = paste(year, "年各群組特徵（標準化）"),
          xlab = "群組", ylab = "變數",
          margins = c(8, 15),
          cexRow = 0.8, cexCol = 1.2)
  
  dev.off()
}

cat("5. 產製應變數變化圖...\n")

if (!is.null(final_results$outcome_trends)) {
  cat("outcome_trends 的欄位：", names(final_results$outcome_trends), "\n")
  
  trends_data <- final_results$outcome_trends %>%
    dplyr::mutate(
      year = as.numeric(.data$year),
      cluster = as.numeric(.data$cluster)
    )
  
  if ("Partnership_mean" %in% names(trends_data)) {
    p1 <- ggplot(trends_data,
                 aes(x = year, y = Partnership_mean,
                     color = factor(cluster), group = cluster)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Partnership 平均值變化",
           x = "年度", y = "Partnership 平均值",
           color = "群組") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    png(file.path(output_dir, "05_outcome_trends.png"),
        width = 800, height = 600, res = 120)
    print(p1)
    dev.off()
  } else if ("Partnership" %in% names(trends_data)) {
    trends_summary <- trends_data %>%
      dplyr::group_by(year, cluster) %>%
      dplyr::summarise(
        Partnership_mean  = mean(.data$Partnership,  na.rm = TRUE),
        Partnership2_mean = mean(.data$Partnership2, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    p1 <- ggplot(trends_summary,
                 aes(x = year, y = Partnership_mean,
                     color = factor(cluster), group = cluster)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Partnership 平均值變化",
           x = "年度", y = "Partnership 平均值",
           color = "群組") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    p2 <- ggplot(trends_summary,
                 aes(x = year, y = Partnership2_mean,
                     color = factor(cluster), group = cluster)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Partnership2 平均值變化",
           x = "年度", y = "Partnership2 平均值",
           color = "群組") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    png(file.path(output_dir, "05_outcome_trends.png"),
        width = 1600, height = 600, res = 120)
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    dev.off()
  }
}

cat("6. 產製群組規模變化圖...\n")

# 修正 select 函數衝突
cluster_size <- final_results$outcome_trends %>%
  dplyr::select(year, cluster, n) %>%
  distinct()

# 或者使用基礎 R
# cluster_size <- unique(final_results$outcome_trends[, c("year", "cluster", "n")])

# ============================================================================
# 第七部分：匯出統計表格
# ============================================================================

cat("7. 匯出統計表格...\n")

# 各年度群組特徵
for (year in years) {
  year_char <- as.character(year)
  profile <- final_results$cluster_profiles[[year_char]]
  
  # 四捨五入
  profile_rounded <- profile %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # 儲存 CSV
  write.csv(profile_rounded, 
            file.path(output_dir, paste0("table_profile_", year, ".csv")),
            row.names = FALSE)
}

# 國家分群追蹤表
write.csv(final_results$country_cluster_tracking,
          file.path(output_dir, "table_country_tracking.csv"),
          row.names = FALSE)

# 應變數趨勢表
if (!is.null(final_results$outcome_trends)) {
  write.csv(final_results$outcome_trends,
            file.path(output_dir, "table_outcome_trends.csv"),
            row.names = FALSE)
}

# ============================================================================
# 第八部分：產製摘要報告
# ============================================================================

cat("8. 產製摘要報告...\n")
summary_text <- paste0(
  "PCA 分群分析結果摘要\n",
  rep("=", 60), "\n\n",
  "分析日期: ", Sys.Date(), "\n",
  "分析年度: ", paste(years, collapse = ", "), "\n",
  "共同國家數: ", length(final_results$common_countries), "\n\n",
  
  "一、PCA 降維結果\n",
  "  經濟維度: ", final_results$pca_economic$n_components, " 個主成分 (",
  round(sum(final_results$pca_economic$pca_obj$eig[1:final_results$pca_economic$n_components, 2]), 1), "%)\n",
  "  制裁維度: ", final_results$pca_sanctional$n_components, " 個主成分 (",
  round(sum(final_results$pca_sanctional$pca_obj$eig[1:final_results$pca_sanctional$n_components, 2]), 1), "%)\n",
  "  治理維度: ", final_results$pca_governance$n_components, " 個主成分 (",
  round(sum(final_results$pca_governance$pca_obj$eig[1:final_results$pca_governance$n_components, 2]), 1), "%)\n\n",
  
  "二、最佳分群數: ", final_results$optimal_k, "\n\n",
  
  "三、各年度分群品質\n"
)
for (year in years) {
  year_char <- as.character(year)
  result <- final_results$clustering_results[[year_char]]
  summary_text <- paste0(summary_text,
                         "  ", year, " 年: 輪廓係數 = ", 
                         round(result$silhouette, 3), 
                         " | 國家數 = ", result$n_countries, "\n")
}
summary_text <- paste0(summary_text, "\n",
                       "四、產製圖表清單\n",
                       "  01. PCA 解釋變異量圖\n",
                       "  02. 變數相關圖\n",
                       "  03. 三年度分群散佈圖\n",
                       "  04. 各年度群組特徵熱圖\n",
                       "  05. 應變數變化趨勢圖\n",
                       "  06. 群組規模變化圖\n",
                       "  table_*.csv: 統計表格\n")
writeLines(summary_text, file.path(output_dir, "00_SUMMARY.txt"))

# ============================================================================
# 完成
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("✓ 視覺化完成！\n")
cat("輸出資料夾:", output_dir, "\n")
cat(rep("=", 80), "\n\n")

# 列出檔案
files <- list.files(output_dir)
cat("產製檔案:", length(files), "個\n")
for (file in files) {
  cat("  -", file, "\n")
}
# ============================================================================
# Partnership 分析 - 完整版
# ============================================================================

# 步驟1: 載入必要套件
library(tidyverse)
library(knitr)
library(kableExtra)
library(gridExtra)

# 步驟2: 設定工作目錄
setwd("D:/R_workspace")  # 改成您的實際路徑

# ===== 【缺少的部分1】載入 PCA 分析結果 =====
pca_results <- readRDS("pca_clustering_analysis_A.rds")
cat("✓ 已載入 PCA 分析結果\n")

# ===== 【缺少的部分2】設定輸出資料夾 =====
output_dir <- "partnership_analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("✓ 已建立輸出資料夾:", output_dir, "\n")
} else {
  cat("✓ 輸出資料夾已存在:", output_dir, "\n")
}

# ============================================================================
# 步驟3: 準備包含集群標籤的資料
# ============================================================================

cat("\n步驟3: 準備資料...\n")

# 建立完整的資料框，包含所有年份的集群標籤
data_with_clusters <- pca_results$data_with_pca

# 為每個指定年份添加集群標籤
for (year in c(1996, 2013, 2023)) {
  year_char <- as.character(year)
  
  if (year_char %in% names(pca_results$clustering_results)) {
    cluster_info <- pca_results$clustering_results[[year_char]]
    
    # 建立集群標籤欄位
    cluster_col <- paste0("cluster_", year)
    data_with_clusters[[cluster_col]] <- NA
    
    # 將集群標籤對應到資料
    for (i in seq_along(cluster_info$country_id)) {
      country_code <- as.character(cluster_info$country_id[i])
      cluster_num <- cluster_info$clusters[i]
      
      # 為該國家在該年份的資料添加集群標籤
      data_with_clusters[[cluster_col]][
        data_with_clusters$countrycode == country_code & 
          data_with_clusters$year == year
      ] <- cluster_num
    }
  }
}

cat("✓ 已準備集群標籤\n")

# ============================================================================
# 步驟4: 分析各集群的 Partnership 描述統計
# ============================================================================

cat("\n步驟4: 計算描述統計...\n")
analyze_partnership_by_cluster <- function(data, year) {
  # 篩選該年份的數據
  year_data <- data %>%
    filter(Year == year)
  
  # 計算每個 cluster 的合作統計
  cluster_stats <- year_data %>%
    group_by(cluster) %>%
    summarise(
      total_partnerships = n(),
      avg_citations = mean(Citation, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # 計算合作類型分布
  partnership_types <- year_data %>%
    group_by(cluster, Partnership_Type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(cluster) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # 將合作類型數據轉為寬格式(避免重複的 cluster 欄位)
  partnership_wide <- partnership_types %>%
    select(cluster, Partnership_Type, percentage) %>%
    pivot_wider(
      names_from = Partnership_Type,
      values_from = percentage,
      values_fill = 0,
      names_prefix = "pct_"
    )
  
  # 合併統計數據
  result <- cluster_stats %>%
    left_join(partnership_wide, by = "cluster")  # 明確指定 by 參數
  
  return(result)
}

# 對三個年份進行分析
partnership_stats <- list()
for (year in c(1996, 2013, 2023)) {
  partnership_stats[[as.character(year)]] <- 
    analyze_partnership_by_cluster(data_with_clusters, year)
}

cat("\n")
cat(rep("=", 70), "\n")
cat("✓ 分析完成！\n")
cat("結果已儲存至:", output_dir, "\n")
cat(rep("=", 70), "\n")

# 檢查產生的檔案
cat("\n產生的檔案:\n")
files <- list.files(output_dir, pattern = "partnership_stats")
for (f in files) {
  cat("  -", f, "\n")
}
# ============================================================================
# 步驟5: 統計檢定（集群間差異）
# ============================================================================

statistical_tests <- function(data, year, var_name = "partnership") {
  cluster_col <- paste0("cluster_", year)
  
  year_data <- data %>%
    filter(year == !!year, !is.na(.data[[cluster_col]])) %>%
    mutate(cluster = factor(.data[[cluster_col]]))
  
  if (!var_name %in% names(year_data)) {
    return(NULL)
  }
  
  # Kruskal-Wallis 檢定（非參數）
  kw_test <- kruskal.test(as.numeric(year_data[[var_name]]) ~ cluster, 
                          data = year_data)
  
  # ANOVA 檢定（參數）
  anova_test <- aov(as.numeric(year_data[[var_name]]) ~ cluster, 
                    data = year_data)
  anova_summary <- summary(anova_test)
  
  # 事後比較（Pairwise Wilcoxon test）
  pairwise_test <- pairwise.wilcox.test(
    as.numeric(year_data[[var_name]]), 
    year_data$cluster,
    p.adjust.method = "bonferroni"
  )
  
  cat("\n", year, "年", var_name, "統計檢定結果:\n")
  cat("\nKruskal-Wallis 檢定:\n")
  print(kw_test)
  cat("\nANOVA 檢定:\n")
  print(anova_summary)
  cat("\n事後比較 (Pairwise Wilcoxon):\n")
  print(pairwise_test)
  
  # 儲存結果
  sink(file.path(output_dir, 
                 paste0("statistical_tests_", var_name, "_", year, ".txt")))
  cat(year, "年", var_name, "統計檢定結果\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")  # 【修正處】
  cat("Kruskal-Wallis 檢定:\n")
  print(kw_test)
  cat("\n\nANOVA 檢定:\n")
  print(anova_summary)
  cat("\n\n事後比較 (Pairwise Wilcoxon):\n")
  print(pairwise_test)
  sink()
  
  return(list(
    kruskal_wallis = kw_test,
    anova = anova_summary,
    pairwise = pairwise_test
  ))
}

# 對三個年份進行統計檢定
test_results <- list()
for (year in c(1996, 2013, 2023)) {
  if ("partnership" %in% names(data_with_clusters)) {
    test_results[[paste0(year, "_partnership")]] <- 
      statistical_tests(data_with_clusters, year, "partnership")
  }
  
  if ("partnership2" %in% names(data_with_clusters)) {
    test_results[[paste0(year, "_partnership2")]] <- 
      statistical_tests(data_with_clusters, year, "partnership2")
  }
}

cat("\n統計檢定完成！\n")

# ============================================================================
# 步驟6: 列出各集群的國家名單及其 Partnership 值
# ============================================================================

cat("\n步驟6: 整理各集群國家名單...\n")

create_cluster_country_list <- function(data, year, var_name = "partnership") {
  cluster_col <- paste0("cluster_", year)
  
  # 先篩選和重新命名，再排序
  country_list <- data %>%
    filter(year == !!year, !is.na(.data[[cluster_col]])) %>%
    select(countrycode, countryname, 
           cluster = all_of(cluster_col),  # 修正這裡
           partnership_value = all_of(var_name)) %>%
    arrange(cluster, desc(partnership_value))
  
  # 儲存完整名單
  write.csv(country_list, 
            file.path(output_dir, 
                      paste0("country_list_", var_name, "_", year, ".csv")),
            row.names = FALSE)
  
  # 按集群分組顯示
  cat("\n", year, "年各集群國家及", var_name, "值:\n")
  for (cl in sort(unique(country_list$cluster))) {
    cluster_countries <- country_list %>% filter(cluster == cl)
    cat("\n集群", cl, "(n =", nrow(cluster_countries), "):\n")
    print(kable(cluster_countries, digits = 3) %>%
            kable_styling(bootstrap_options = c("striped", "hover")))
  }
  
  return(country_list)
}
# ============================================================================
# 步驟7: 產生總結報告
# ============================================================================

cat("\n步驟7: 產生總結報告...\n")

summary_report <- paste0(
  "各集群 Partnership 表現分析報告\n",
  "=" %>% rep(60) %>% paste(collapse = ""), "\n\n",
  "分析日期: ", Sys.Date(), "\n",
  "分析年份: 1996, 2013, 2023\n\n",
  "主要發現:\n",
  "1. 描述統計結果已儲存至 partnership_stats_[year].csv\n",
  "2. 視覺化圖表已儲存至 partnership_dist_[var]_[year].png\n",
  "3. 時間趨勢圖已儲存至 partnership_trend_[var].png\n",
  "4. 統計檢定結果已儲存至 statistical_tests_[var]_[year].txt\n",
  "5. 各集群國家名單已儲存至 country_list_[var]_[year].csv\n\n",
  "所有輸出檔案位於: ", output_dir, " 資料夾\n"
)

writeLines(summary_report, 
           file.path(output_dir, "00_ANALYSIS_SUMMARY.txt"))

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("分析完成！\n")
cat("輸出資料夾:", output_dir, "\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# 列出產生的檔案
cat("產生的檔案:\n")
files <- list.files(output_dir)
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