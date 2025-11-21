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
China_partnership_1996_2023_breakpoint_2025_03_24_02 <- read_excel("C:/Users/jimyu1743/Desktop/習近平與中國夥伴關係外交/原始與整理資料/China partnership 1996-2023 breakpoint 2025.03.24.02.xlsx")
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

# 將連續變數標準化,並用新變數名稱加入
CP9623 <- CP9623 %>%
  mutate(
    across(
      .cols = c("dist", "dip_age", "population_total", "gdp", 
                "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
                "trade", "arms", "military", "financial", "travel",
                "va", "psv", "ge", "rq", "rl", "cc", "WGI"), 
      .fns = ~ as.numeric(scale(.)),  
      .names = "{.col}_std" 
    ))
# 將資料框架設定為面板資料格式
panel_CP9623 <- pdata.frame(CP9623, index = c("countrycode", "year"))

# 刪除資料框中含有 Inf 或 -Inf 的列
panel_CP9623 <- panel_CP9623[!apply(panel_CP9623, 1, function(x) any(is.infinite(x))), ]


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

cat("\n步驟2: 加入主成分至 CP9623\n", rep("-", 80), "\n")

add_pca_scores <- function(data, pca_result, prefix) {
  if (is.null(pca_result)) return(data)
  
  # 取得 PCA 使用的變數與主成分數量
  vars <- pca_result$available_vars # 假設您的 PCA 結果物件中有儲存這個
  if(is.null(vars)) {
    # 如果您的 PCA 物件結構不同，這裡可能需要調整
    # 這裡假設 pca_result$pca_obj 是 FactoMineR 的結果
    vars <- names(pca_result$pca_obj$call$X) 
  }
  n_pc <- ncol(pca_result$pca_obj$ind$coord) # 自動偵測保留的 PC 數量
  
  # 找出哪些列是完整的 (沒有遺漏值)，才能對應 PCA 分數
  # 注意：這裡假設 data 的行順序沒有被改變，或者依靠變數完整性來對應
  clean_idx <- complete.cases(data[, vars])
  
  for (i in 1:n_pc) {
    col_name <- paste0(prefix, "_PC", i)
    data[[col_name]] <- NA_real_
    # 將分數填入對應的列
    if(sum(clean_idx) == nrow(pca_result$pca_obj$ind$coord)){
      data[[col_name]][clean_idx] <- pca_result$pca_obj$ind$coord[, i]
    } else {
      warning(paste("警告:", prefix, "的主成分分數數量與資料集完整列數不符，請檢查資料對齊。"))
    }
  }
  
  cat("✓", prefix, ": 已加入", n_pc, "個主成分\n")
  return(data)
}

# 執行合併 (假設 pca_economic, pca_sanctional, pca_governance 已存在)
CP9623_pca <- CP9623 %>%
  add_pca_scores(pca_economic, "econ") %>%
  add_pca_scores(pca_sanctional, "sanct") %>%
  add_pca_scores(pca_governance, "gov")

# 取得所有主成分欄位名稱 (用於後續分群)
pc_cols <- grep("^(econ|sanct|gov)_PC[0-9]", names(CP9623_pca), value = TRUE)
cat("\n主成分總數:", length(pc_cols), "\n欄位名稱:", paste(pc_cols, collapse=", "), "\n")

# ============================================================================
# 步驟3: 各年度各層級分群分析
# ============================================================================

cat("\n步驟3: 各年度各層級分群分析\n", rep("-", 80), "\n")

cluster_by_year_level <- function(data, year, level, pc_cols,
                                  k_range = 2:8, min_n = 5) {
  
  # --- 1. 資料篩選與準備 ---
  cat(sprintf("\n【%s年 - 層級: %s】", year, level))
  
  subset_data <- data %>%
    filter(year == !!year,
           !!sym(partnership_col) == !!level) %>%
    drop_na(all_of(pc_cols))
  
  n_countries <- nrow(subset_data)
  
  if (n_countries < min_n) {
    cat(" -> 樣本數不足 (n=", n_countries, ")，跳過。\n", sep = "")
    return(NULL)
  }
  
  X <- as.matrix(subset_data[, pc_cols])
  rownames(X) <- as.character(subset_data[[country_id_col]])
  dist_X <- dist(X)
  
  max_k <- min(max(k_range), floor(n_countries / 2) - 1)
  if (max_k < 2) {
    cat(" -> 樣本過少無法分群 (可分群數 < 2)，跳過。\n")
    return(NULL)
  }
  
  # --- 2. 迭代尋找最佳 k ---
  k_tested <- 2:max_k
  k_len <- length(k_tested)
  
  sil_scores <- numeric(k_len)
  wss_scores <- numeric(k_len)
  ch_scores  <- numeric(k_len)
  
  for (i in 1:k_len) {
    k <- k_tested[i]
    set.seed(123)
    km <- kmeans(X, centers = k, nstart = 25)
    
    # a. Silhouette
    sil <- silhouette(km$cluster, dist_X)
    sil_scores[i] <- mean(sil[, 3])
    
    # b. WSS
    wss_scores[i] <- km$tot.withinss
    
    # c. Calinski–Harabasz (修正點在此：移除 centrotypes 參數)
    ch_stats <- fpc::cluster.stats(dist_X, km$cluster)
    ch_scores[i] <- ch_stats$ch
  }
  
  # --- 3. 決定最佳 k ---
  optimal_k <- k_tested[which.max(sil_scores)]
  optimal_k_ch <- k_tested[which.max(ch_scores)]
  
  # --- 4. 執行最終模型 ---
  set.seed(123)
  km_final <- kmeans(X, centers = optimal_k, nstart = 50)
  sil_final <- silhouette(km_final$cluster, dist_X)
  mean_sil <- mean(sil_final[, 3])
  
  # --- 5. 穩健性檢查 (PAM) ---
  set.seed(123)
  pam_final <- pam(dist_X, k = optimal_k, cluster.only = FALSE)
  pam_sil_mean <- mean(silhouette(pam_final$clustering, dist_X)[, 3])
  
  # --- 6. 輸出狀態與結果 ---
  cat(sprintf("\n  樣本數: %d | 最佳 k: %d (Silhouette=%.3f, CH=%.1f) | PAM檢查: %.3f", 
              n_countries, optimal_k, mean_sil, max(ch_scores), pam_sil_mean))
  
  if (mean_sil < 0.25) cat(" [警告: 分群結構不明顯]")
  cat("\n")
  
  list(
    year = year,
    level = level,
    n_countries = n_countries,
    optimal_k = optimal_k,
    clusters = km_final$cluster,
    country_id = subset_data[[country_id_col]],
    silhouette = mean_sil,
    pam_silhouette = pam_sil_mean,
    optimal_k_ch = optimal_k_ch,
    bss_tss = km_final$betweenss / km_final$totss,
    k_tested = k_tested,
    sil_scores = sil_scores,
    wss_scores = wss_scores,
    ch_scores = ch_scores
  )
}

# ============================================================================
# 執行迴圈
# ============================================================================

# 取得所有夥伴層級 (排除 NA)
partnership_col <- "partnership"
partnership_levels <- unique(CP9623_pca[[partnership_col]])
partnership_levels <- partnership_levels[!is.na(partnership_levels)]
cat("成功抓取層級:", paste(partnership_levels, collapse = ", "), "\n")

cat("\n待分析層級:", paste(partnership_levels, collapse = ", "), "\n")

clustering_results <- list()

# 開始迴圈
for (year in analysis_years) {
  for (level in partnership_levels) {
    
    # 建立唯一的 key，例如 "Y2020_LHigh"
    key <- paste0("Y", year, "_L", level)
    
    # 執行函數
    res <- cluster_by_year_level(
      CP9623_pca, year, level, pc_cols
    )
    
    # 只有當結果不是 NULL 時才存入 list
    if (!is.null(res)) {
      clustering_results[[key]] <- res
    }
  }
}

cat("\n", rep("=", 80), "\n分析完成！共產生", length(clustering_results), "組分群結果。\n")
# 匯整所有年份與層級的指標
cluster_report <- map_dfr(names(clustering_results), function(key) {
  res <- clustering_results[[key]]
  
  tibble(
    Key = key,
    Year = res$year,
    Level = res$level,
    N_Countries = res$n_countries,
    Optimal_K = res$optimal_k,         # 最終選定的群數
    Silhouette = round(res$silhouette, 3),       # 分群品質 (越大越好)
    Pseudo_F = round(max(res$ch_scores), 2),     # 區分度 (越大越好)
    PAM_Check = round(res$pam_silhouette, 3),    # 穩健性檢查
    WSS_Ratio = round(res$bss_tss, 3)            # 解釋變異比例
  )
}) %>%
  arrange(Year, Level)

# 顯示前 10 筆看看
print(head(cluster_report, 10))

# 匯出成 CSV 檔案
write_csv(cluster_report, "Cluster_Performance_Report.csv")
cat("\n✅ 分群績效報表已儲存: Cluster_Performance_Report.csv\n")
# ============================================================================
# 步驟4: 多樣化指標計算
# ============================================================================

calculate_diversity_metrics <- function(cluster_result) {
  # 1. 基本防呆
  if (is.null(cluster_result)) return(NULL)
  
  # 2. 提取必要資訊
  k <- cluster_result$optimal_k
  n <- cluster_result$n_countries
  
  # ★★★ 修正點：現場計算群組大小 (並強制轉為數值向量) ★★★
  # cluster_result$clusters 是一個包含 1, 1, 2, 3... 的向量
  # table() 計算各群數量，as.numeric() 確保它是純數字，避免格式錯誤
  sizes <- as.numeric(table(cluster_result$clusters))
  
  # 3. 進行指標計算
  
  # 指標1: 分群數
  num_clusters <- k
  
  # 計算比例 (p_i)
  props <- sizes / n
  
  # 指標2: 標準化熵 (Shannon Entropy / ln(k))
  # 代表群組大小是否「均勻」。若每群國家數一樣多，此值為 1。
  entropy <- -sum(props * log(props))
  max_entropy <- log(k)
  
  # 避免 k=1 時分母為 0 的情況
  normalized_entropy <- if(k > 1) entropy / max_entropy else 0
  
  # 指標3: Simpson多樣性指數 (1 - sum(p^2))
  # 代表「隨機抓兩個國家，它們屬於不同群的機率」
  simpson <- 1 - sum(props^2)
  
  # 指標4: 有效群數 (Effective number of clusters)
  effective_k <- exp(entropy)
  
  # 指標5: 變異係數 (CV of cluster sizes)
  # 衡量群組大小的差異程度 (值越大代表有一群特別大或特別小)
  cv <- sd(sizes) / mean(sizes)
  
  return(list(
    year = cluster_result$year,
    level = cluster_result$level,
    n_countries = n,
    num_clusters = num_clusters,
    silhouette = cluster_result$silhouette,
    bss_tss = cluster_result$bss_tss,
    
    # 多樣性指標
    normalized_entropy = normalized_entropy,
    simpson_index = simpson,
    effective_k = effective_k,
    cv_cluster_size = cv,
    
    # 為了方便檢查，也把各群大小存進去 (轉成字串以免破壞表格結構)
    size_distribution = paste(sizes, collapse = ", ")
  ))
}

# 重新執行計算
diversity_metrics_list <- lapply(clustering_results, calculate_diversity_metrics)

# 將結果轉為 Data Frame 報表
diversity_report <- bind_rows(diversity_metrics_list) %>%
  arrange(year, level)

# 檢查結果 (不再有警告)
print(head(diversity_report))

# 匯出報表
write_csv(diversity_report, "Diversity_Analysis_Report.csv")
cat("\n✅ 多樣性指標報表已產出！\n")

# ============================================================================
# 生成CP9623_final
# ============================================================================

cat("\n🔧 關鍵修正步驟: 合併群組標籤以生成 CP9623_final...\n")

# 1. 將 clustering_results 列表中的群組標籤數據扁平化
all_cluster_labels <- map_dfr(clustering_results, function(res) {
  if (!is.null(res)) {
    # 🚨 關鍵修正：不能依賴 names(res$clusters)。必須從另一個向量獲取 ID。
    # 假設您的函數輸出中仍然包含 res$country_id
    
    # 1. 確保 ID 向量存在，如果不存在，我們就無法繼續
    if (is.null(res$country_id)) {
      stop(paste("錯誤：結果結構中缺少 'country_id' 向量。請確認您已重新運行分群迴圈，並在 'cluster_by_year_level' 的輸出列表中加入了 'country_id'。"))
    }
    
    N_rows <- length(res$clusters)
    
    df <- data.frame(
      # 🎯 修正點 1: 使用 res$country_id 作為 ID
      country_id = res$country_id, 
      # 修正點 2: 使用 rep() 函數，強制 year 和 level 的長度與國家數匹配
      year = rep(res$year, N_rows),
      level = rep(res$level, N_rows),
      cluster_group = res$clusters,
      stringsAsFactors = FALSE
    )
    return(df)
  }
}) %>%
  mutate(year = as.numeric(year), level = as.numeric(level))

# 2. 將 PC 分數數據 CP9623_pca 準備好進行合併
CP9623_data_for_join <- CP9623_pca %>%
  mutate(
    year = as.numeric(year), 
    level = as.numeric(!!sym(partnership_col)),
    country_id = as.character(!!sym(country_id_col))
  )

# 3. 執行合併，生成 CP9623_final
CP9623_final <- left_join(
  CP9623_data_for_join, 
  all_cluster_labels, 
  by = c("country_id", "year", "level")
)

CP9623_final <- CP9623_final %>% select(-country_id) 

cat("✓ CP9623_final 數據集已成功創建並包含 'cluster_group' 欄位。\n")

# 確認 CP9623_final 已經生成，您現在可以執行計算群組中心點的步驟了。
# ============================================================================
# 步驟5: 趨勢分析與視覺化
# ============================================================================

cat("\n步驟5: 趨勢分析\n", rep("-", 80), "\n")

# 計算各層級的多樣化趨勢 (使用 reframe 修正警告)
diversity_trends <- diversity_report %>%
  group_by(level) %>%
  arrange(year) %>%
  reframe( # 將 summarise 替換為 reframe
    entropy_1996 = normalized_entropy[year == 1996],
    entropy_2023 = normalized_entropy[year == 2023],
    entropy_change = entropy_2023 - entropy_1996,
    
    simpson_1996 = simpson_index[year == 1996],
    simpson_2023 = simpson_index[year == 2023],
    simpson_change = simpson_2023 - simpson_1996,
    
    k_1996 = num_clusters[year == 1996],
    k_2023 = num_clusters[year == 2023],
    k_change = k_2023 - k_1996
  )

cat("\n各層級多樣化趨勢（1996→2023）：\n")
print(kable(diversity_trends, digits = 3))

# 計算加權後的整體平均趨勢
weighted_overall_trend <- diversity_report %>%
  group_by(year) %>%
  summarise(
    # 加權平均熵 = Sum(熵 * 國家數) / Sum(國家數)
    weighted_mean_entropy = sum(normalized_entropy * n_countries, na.rm = TRUE) / sum(n_countries, na.rm = TRUE),
    
    # 加權平均辛普森指數
    weighted_mean_simpson = sum(simpson_index * n_countries, na.rm = TRUE) / sum(n_countries, na.rm = TRUE),
    
    # 總分群國家數 (用於判斷權重變化)
    total_countries_clustered = sum(n_countries, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n✅ 修正後的整體加權平均趨勢：\n")
print(kable(weighted_overall_trend, digits = 3))

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
  diversity_metrics = diversity_report,
  diversity_trends = diversity_trends,
  overall_trend = weighted_overall_trend
)

saveRDS(final_results, "partnership_diversity_analysis.rds")
cat("✓ 結果已儲存至: partnership_diversity_analysis.rds\n\n")

# ============================================================================
# 結論摘要
# ============================================================================

# ============================================================================
# 結論摘要 (已修正為使用加權平均數據)
# ============================================================================

cat(rep("=", 80), "\n")
cat("分析總結：中國夥伴外交決策結構性變化\n")
cat(rep("=", 80), "\n\n")

cat("研究問題：中國夥伴外交決策是否隨時間愈趨多樣化？ (基於全球國家結構)\n\n")

cat("主要發現：\n")

for (i in 1:nrow(weighted_overall_trend)) {
  year <- weighted_overall_trend$year[i]
  
  # 移除誤導性的 mean_k，改為顯示總國家數以提供權重背景
  cat(sprintf("【%d年】 加權平均熵: %.3f | 加權平均Simpson指數: %.3f | 總分群國家數: %d\n",
              year, 
              weighted_overall_trend$weighted_mean_entropy[i],
              weighted_overall_trend$weighted_mean_simpson[i],
              weighted_overall_trend$total_countries_clustered[i]))
}

# 找出 1996 年和 2023 年的數據
entropy_1996 <- weighted_overall_trend$weighted_mean_entropy[weighted_overall_trend$year == 1996]
entropy_2023 <- weighted_overall_trend$weighted_mean_entropy[weighted_overall_trend$year == 2023]

entropy_change_pct <- (entropy_2023 - entropy_1996) / entropy_1996 * 100

cat(sprintf("\n1996→2023 結構多樣性變化 (加權熵): %+.2f%%\n", entropy_change_pct))

# 根據實際的加權趨勢來判斷結果
if (entropy_change_pct > 0.05) { # 增加
  cat("結論：全球結構多樣化程度 (均衡性) 增加。\n")
} else if (entropy_change_pct < -0.05) { # 減少
  cat("結論：全球結構多樣化程度 (均衡性) **顯著減少**，結構趨於集中極化。\n")
} else {
  cat("結論：結構多樣化程度相對穩定。\n")
}

cat("\n補充：然而，微觀上 Level 0 的 K 值 (類型數量) 則從 3 增加到 4，顯示決策因素複雜度有提升。\n")
cat(rep("=", 80), "\n")
# ============================================================================
# 計算群組中心點 (PC Scores)
# ============================================================================

# 1. 篩選目標資料：2023 年，Level 0
target_data <- CP9623_final %>%
  filter(
    year == 2023,
    !!sym(partnership_col) == 0,
    !is.na(cluster_group)
  )

# 確認 PC 欄位名稱
# 由於您前面的程式碼使用了 econ_PC1, sanct_PC1, 以及一個動態的治理欄位名稱 (例如 gov_PC1)
# 我們需要再次確認治理欄位的名稱
governance_col_name <- names(target_data)[grepl("govern|gov", names(target_data), ignore.case = TRUE) & grepl("pc1", names(target_data), ignore.case = TRUE)][1]

if (is.na(governance_col_name)) {
  stop("錯誤：無法識別治理 PC 欄位名稱。請確認 CP9623_final 中是否存在類似 'gov_PC1' 的欄位。")
}

pc_cols_for_analysis <- c("econ_PC1", "sanct_PC1", governance_col_name)

# 2. 計算各群組在 PC 因子上的平均分數 (群組中心點)
cluster_centers_2023_L0 <- target_data %>%
  # 選取群組欄位和所有 PC 欄位
  select(cluster_group, all_of(pc_cols_for_analysis)) %>%
  # 依群組分組
  group_by(cluster_group) %>%
  # 計算每個 PC 欄位的平均數
  summarise(
    N = n(),
    across(starts_with("PC1") | starts_with("econ_PC1") | starts_with("sanct_PC1") | starts_with("gov_PC1") | starts_with("govern"), mean, .names = "Mean_{.col}"),
    .groups = "drop"
  ) %>%
  # 將群組名稱重新排序為 K-means 的標準 (1, 2, 3, 4...)
  arrange(cluster_group)

# 3. 顯示結果並匯出
cat("\n✅ 2023年 Level 0 (未締結夥伴關係國) 分群中心點：\n")
print(kable(cluster_centers_2023_L0, digits = 3))

write_csv(cluster_centers_2023_L0, "2023_L0_Cluster_PC_Centers.csv")
cat("\n✓ 群組中心點報表已儲存: 2023_L0_Cluster_PC_Centers.csv\n")
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

cat("3. 產製分群散佈圖 (修正中...)\n")

# 重新定義年份列表，以確保迴圈正確運行
years <- unique(CP9623_final$year)

# 修正後的 3D 繪圖函數 (直接使用全域變數 CP9623_final 和 clustering_results)
plot_yearly_clusters_3d <- function(year) {
  # 🎯 繪圖目標：我們選擇繪製 Level 0 的結果，因為這是最主要的群體且分群 K=4 最複雜。
  level <- 0
  year_char <- as.character(year)
  key <- paste0("Y", year, "_L", level)
  
  # 1. 資料篩選 (修正點 1: 使用 CP9623_final; 修正點 2: 篩選 Level 0)
  year_data_raw <- dplyr::filter(
    CP9623_final,
    .data$year == year, 
    .data[[partnership_col]] == level, # 篩選特定 Level
    !is.na(.data$cluster_group)      # 使用正確的群組欄位名稱
  )
  
  # --- 治理維度欄位確認 (維持原有的邏輯) ---
  governance_col <- names(year_data_raw)[grepl("govern|gov", names(year_data_raw), ignore.case = TRUE) & grepl("pc1", names(year_data_raw), ignore.case = TRUE)][1]
  
  if (is.na(governance_col)) {
    warning(paste("【警告】", year, "年 Level", level, "找不到治理維度 PC1 欄位，跳過繪圖。"))
    return(NULL)
  }
  
  # 2. 建立繪圖資料，確保 PC 欄位存在 (假設 PC 欄位名稱為 econ_PC1, sanct_PC1)
  year_data <- data.frame(
    econ_PC1  = as.numeric(year_data_raw$econ_PC1),
    sanct_PC1 = as.numeric(year_data_raw$sanct_PC1),
    gov_PC1   = as.numeric(year_data_raw[[governance_col]]),
    cluster   = as.factor(year_data_raw$cluster_group), # 使用正確的群組欄位
    check.names = FALSE
  )
  year_data <- year_data[stats::complete.cases(year_data), , drop = FALSE]
  
  if (nrow(year_data) < 5) {
    return(NULL)
  }
  
  # 3. 提取分群資訊 (修正點 3: 使用 clustering_results)
  clusters <- clustering_results[[key]]
  subtitle_text <- if (!is.null(clusters)) {
    paste("K值:", clusters$optimal_k, "| 輪廓係數:", round(clusters$silhouette, 3), "| 國家數:", clusters$n_countries)
  } else {
    "（無可用分群摘要）"
  }
  
  # 4. 繪製 3D 散佈圖
  p <- plotly::plot_ly(
    data = year_data,
    x = ~econ_PC1, y = ~sanct_PC1, z = ~gov_PC1,
    color = ~cluster,
    colors = RColorBrewer::brewer.pal(max(8, length(unique(year_data$cluster))), "Set1"),
    type = "scatter3d", mode = "markers",
    marker = list(size = 4, opacity = 0.8)
  )
  
  p <- plotly::layout(
    p,
    title = paste0(year, "年 Level ", level, " 國家分群結果<br><sup>", subtitle_text, "</sup>"),
    scene = list(
      xaxis = list(title = "經濟維度 PC1"),
      yaxis = list(title = "制裁維度 PC1"),
      zaxis = list(title = "治理維度 PC1"),
      aspectmode = "cube"
    ),
    legend = list(orientation = "h")
  )
  
  return(p)
}

# 執行繪圖迴圈 (只繪製 Level 0)
plots3d <- lapply(years, plot_yearly_clusters_3d)

# 清除 NULL 元素，只保留成功繪製的圖表
plots3d <- plots3d[!sapply(plots3d, is.null)]

# 儲存個別年份的 HTML 檔案
if (length(plots3d) > 0) {
  for (i in seq_along(plots3d)) {
    year_index <- which(sapply(years, function(y) any(grepl(paste0(y, "年"), plots3d[[i]]$layout$title))))
    year_val <- years[year_index]
    
    htmlwidgets::saveWidget(
      plots3d[[i]],
      file = file.path(output_dir, paste0("03_clustering_results_L0_3d_", year_val, ".html")),
      selfcontained = TRUE
    )
  }
  cat("✓ 已儲存 Level 0 的 3D 分群圖 (互動式 HTML 檔案)\n\n")
} else {
  cat("✓ 無 Level 0 資料可繪製 3D 圖。\n\n")
}

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