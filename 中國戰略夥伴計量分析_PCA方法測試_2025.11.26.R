# ============================================================================
# 中國夥伴關係外交分群分析完整流程
# ============================================================================

# ============================================================================
# 1. 套件管理
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
  # 因素分析與分群
  "FactoMineR", "factoextra", "GPArotation", "cluster", "NbClust",
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

# ============================================================================
# 2. 資料讀取與前處理
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("資料讀取與前處理\n")
cat(rep("=", 80), "\n\n")

# 讀入資料檔
China_partnership_1996_2023_breakpoint_2025_03_24_02 <- read_excel(
  "C:/Users/jimyu1743/Desktop/習近平與中國夥伴關係外交/原始與整理資料/China partnership 1996-2023 breakpoint 2025.03.24.02.xlsx"
)
CP9623 <- China_partnership_1996_2023_breakpoint_2025_03_24_02

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

# 加入習近平時期變數
CP9623$xi <- ifelse(CP9623$year < 2013, 0, 1)

# 標準化連續變數
CP9623 <- CP9623 %>%
  mutate(
    across(
      .cols = c("dist", "dip_age", "population_total", "gdp", "gdp_per_capita",
                "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
                "trade", "arms", "military", "financial", "travel","WGI",
                "va", "psv", "ge", "rq", "rl", "cc", "WGI"), 
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
# 3. 缺失值插補
# ============================================================================

cat(rep("=", 80), "\n")
cat("缺失值插補\n")
cat(rep("=", 80), "\n\n")

# 定義三類變數
economic_factors <- c("gdp_std", "china_ex_to_i_std", "china_im_fr_i_std", 
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
# 4. PCA降維分析
# ============================================================================

cat(rep("=", 80), "\n")
cat("PCA降維分析\n")
cat(rep("=", 80), "\n\n")

# PCA函數
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

# 執行PCA
pca_economic <- perform_pca(panel_common, economic_factors, "經濟維度", manual_n = 3)
pca_sanctional <- perform_pca(panel_common, sanctional_factors, "制裁維度")
pca_governance <- perform_pca(panel_common, governance_factors, "治理維度")

# 將主成分加入原始資料
add_pca_scores <- function(data, pca_result, prefix) {
  if (is.null(pca_result)) return(data)
  
  vars <- pca_result$available_vars
  n_pc <- pca_result$n_components
  
  clean_idx <- complete.cases(data[, vars])
  
  for (i in 1:n_pc) {
    col_name <- paste0(prefix, "_PC", i)
    data[[col_name]] <- NA_real_
    if(sum(clean_idx) == nrow(pca_result$pca_obj$ind$coord)){
      data[[col_name]][clean_idx] <- pca_result$pca_obj$ind$coord[, i]
    }
  }
  
  cat("✓", prefix, ": 已加入", n_pc, "個主成分\n")
  return(data)
}

CP9623_pca <- panel_common %>%
  add_pca_scores(pca_economic, "econ") %>%
  add_pca_scores(pca_sanctional, "sanct") %>%
  add_pca_scores(pca_governance, "gov")

pc_cols <- grep("^(econ|sanct|gov)_PC[0-9]", names(CP9623_pca), value = TRUE)
cat("\n主成分總數:", length(pc_cols), "\n")

# 儲存PCA結果
saveRDS(CP9623_pca, file = "CP9623_pca.rds")

# ============================================================================
# 5. K-means與K-medians分群分析
# ============================================================================

cat("分群分析\n")
cat(rep("=", 80), "\n\n")

# ==========================================================
# 【建議的插入位置】: 在篩選年份之前，對整個資料集進行插補
# ==========================================================

# 插補 dip_age_std：將 NA 替換為 0
CP9623_pca <- CP9623_pca %>%
  mutate(
    dip_age_std = tidyr::replace_na(dip_age_std, 0)
  )
# 篩選分析年份
data_filtered <- CP9623_pca %>%
  filter(year %in% c(1996, 2013, 2023))

# 定義分群變數

#============================================
# 定義分群變數A

cluster_vars <- c(
  "partnership",  
  "exportdep_std", "importdep_std", "population_total_std",
  "gdp_per_capita_std", "WGI",
  "econ_PC1", "econ_PC2", "econ_PC3", 
  "sanct_PC1", "sanct_PC2", 
  "gov_PC1", "WGI",
  "FTA", "ORG", "dip_age_std", "dist_std"
)

# ===========================================
# 定義分群變數B

cluster_vars <- c(
  "partnership",  
  "econ_PC1", "econ_PC2", "econ_PC3", 
  "sanct_PC1", "sanct_PC2", 
  "gov_PC1","WGI",
  "FTA", "ORG", "dip_age_std", "dist_std"
)

#============================================
# 定義分群變數C

cluster_vars <- c(
  "partnership",
  "exportdep_std", "importdep_std", "population_total_std",
  "gdp_per_capita_std", "WGI",
  "FTA", "ORG", "dip_age_std", "dist_std"
)

data_complete <- data_filtered %>%
  dplyr::select(year, countrycode, all_of(cluster_vars)) %>%
  stats::na.omit()

cat("分群資料:", nrow(data_complete), "筆觀察值\n")
cat("1996年:", sum(data_complete$year == 1996), "國\n")
cat("2013年:", sum(data_complete$year == 2013), "國\n")
cat("2023年:", sum(data_complete$year == 2023), "國\n\n")

# 提取分群用的數值矩陣
X <- data_complete %>% 
  dplyr::select(all_of(cluster_vars))

# ============================================================================
# 5.1 K-means分群
# ============================================================================

cat("執行K-means分群 (k=2 to 15)...\n")

kmeans_results <- list()
kmeans_stats <- data.frame(k = 2:15, 
                           tot_withinss = NA,
                           calinski_harabasz = NA)

for (k in 2:15) {
  set.seed(123)
  km <- kmeans(X, centers = k, nstart = 25, algorithm = "Lloyd")
  kmeans_results[[paste0("k", k)]] <- km
  
  # 計算評估指標
  kmeans_stats$tot_withinss[k-1] <- km$tot.withinss
  
  # Calinski-Harabasz指數
  BSS <- km$betweenss
  WSS <- km$tot.withinss
  n <- nrow(X)
  CH <- (BSS / (k - 1)) / (WSS / (n - k))
  kmeans_stats$calinski_harabasz[k-1] <- CH
}

cat("\nK-means評估指標:\n")
print(kmeans_stats)

# 視覺化選擇最佳k值
par(mfrow = c(1, 2))

# Elbow plot (保持不變，因為這是標準方法)
plot(kmeans_stats$k, kmeans_stats$tot_withinss, type = "b",
     xlab = "Number of clusters (k)", ylab = "Total within-cluster SS",
     main = "Elbow Method", pch = 19, col = "steelblue", lwd = 2)

# CH index plot (保持不變，用於確認 K=4 是峰值)
plot(kmeans_stats$k, kmeans_stats$calinski_harabasz, type = "b",
     xlab = "Number of clusters (k)", ylab = "Calinski-Harabasz Index",
     main = "CH Index", pch = 19, col = "darkred", lwd = 2)

par(mfrow = c(1, 1))


# 提取最適分群結果
km_final <- kmeans_results$k3

# ==========================================================
# 將分群結果標籤添加回資料
# ==========================================================
# 添加新的分群標籤變數
data_complete$cluster_k3 <- km_final$cluster

# ==========================================================
# 輸出最適分群結果
# ==========================================================
cat("\n=== K-means (k=3) 分群結果 ===\n")
cat("1996年:\n")
# 輸出 cluster_k4 在 1996 年的分佈
print(table(data_complete$cluster_k3[data_complete$year == 1996])) 
cat("\n2013年:\n")
# 輸出 cluster_k4 在 2013 年的分佈
print(table(data_complete$cluster_k3[data_complete$year == 2013]))
cat("\n2023年:\n")
# 輸出 cluster_k4 在 2023 年的分佈
print(table(data_complete$cluster_k3[data_complete$year == 2023]))
# 1. 提取群體標籤
km_final <- kmeans_results$k3
data_complete$cluster_k3 <- km_final$cluster

# 2. 計算每個群體在所有分群變數上的平均值 (使用 dplyr)
cluster_profiles <- data_complete %>%
  # 確保 cluster_k8 是因子 (Factor) 類型以便分組
  mutate(cluster_k3 = factor(cluster_k3)) %>%
  
  # 按群體標籤分組
  group_by(cluster_k3) %>%
  
  # 計算 cluster_vars 中所有變數的平均值
  summarise(
    N = n(), # 統計每個群體包含的國家/年份數量
    across(all_of(cluster_vars), mean, .names = "mean_{.col}")
  ) %>%
  ungroup()

cat("\n=== K=3 群體特徵分析 (變數平均值) ===\n")
print(cluster_profiles)
# 重新印出完整的表格，設置 options 讓 R 不會省略欄位
options(max.print = 9999) # 確保輸出的長度足夠
print(cluster_profiles, width = Inf) # 使用 width = Inf 確保所有欄位都顯示
# --- 輸出群體特徵分析結果 (K=4) ---
# row.names = FALSE 是為了避免將 R 的行號也寫入 CSV 中
write.csv(
  cluster_profiles,
  file = "Cluster_Profiles_A.csv",
  row.names = FALSE
)
cat("已將 K=3 群體特徵分析結果輸出至 Cluster_Profiles_K3.csv\n")

# --- 輸出 K 值評估指標結果 (K-means) ---
# 檔名：Kmeans_Evaluation_Stats.csv
write.csv(
  kmeans_stats,
  file = "Kmeans_Evaluation_Stats.csv",
  row.names = FALSE
)
cat("已將 K-means 評估指標輸出至 Kmeans_Evaluation_Stats.csv\n")

# ============================================================================
# 5.2 K-medians分群 (PAM)
# ============================================================================

cat("\n\n執行K-medians分群 (PAM, k=2 to 15)...\n")

kmedian_results <- list()
kmedian_stats <- data.frame(k = 2:15, avg_silhouette = NA)

for (k in 2:15) {
  set.seed(123)
  pam_fit <- pam(X, k = k, metric = "euclidean")
  kmedian_results[[paste0("k", k)]] <- pam_fit
  kmedian_stats$avg_silhouette[k-1] <- pam_fit$silinfo$avg.width
}

cat("\nK-medians (PAM) 評估指標:\n")
print(kmedian_stats)

# ------------------------------------------------------------------
# 提取 K=2 的分群結果
# ------------------------------------------------------------------
# 提取k=2的分群結果 (從 kmedian_results$k3)
pam2 <- kmedian_results$k2

# ------------------------------------------------------------------
# 將分群結果標籤添加回資料
# ------------------------------------------------------------------
# 添加新的分群標籤變數 (建議命名為 cluster_pam3)
data_complete$cluster_pam2 <- pam2$clustering

# ------------------------------------------------------------------
# 輸出 K=3 的分群結果
# ------------------------------------------------------------------
cat("\n=== K-medians (k=2) 分群結果 ===\n")
cat("1996年:\n")
# 輸出 cluster_pam3 在 1996 年的分佈
print(table(data_complete$cluster_pam2[data_complete$year == 1996]))
cat("\n2013年:\n")
# 輸出 cluster_pam3 在 2013 年的分佈
print(table(data_complete$cluster_pam2[data_complete$year == 2013]))
cat("\n2023年:\n")
# 輸出 cluster_pam3 在 2023 年的分佈
print(table(data_complete$cluster_pam2[data_complete$year == 2023]))

# ============================================================================
# 6. 進階視覺化
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("進階視覺化\n")
cat(rep("=", 80), "\n\n")

# 使用NbClust自動選擇最佳k值
cat("使用NbClust進行最佳k值評估...\n")
nb <- NbClust(X, distance = "euclidean", min.nc = 2, max.nc = 15,
              method = "kmeans", index = "all")

# 視覺化k=3的分群結果（使用PCA降維）
fviz_cluster(km2, data = X, 
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             main = "K-means Clustering (k=2)",
             ggtheme = theme_minimal())

# Silhouette plot
fviz_silhouette(silhouette(km6$cluster, dist(X)))

# 按年份分別視覺化
pca_viz <- prcomp(X, scale. = TRUE)
data_pca <- data.frame(pca_viz$x[, 1:2], 
                       cluster = as.factor(km6$cluster),
                       year = data_complete$year)

plots <- lapply(c(1996, 2013, 2023), function(yr) {
  ggplot(data_pca %>% filter(year == yr), 
         aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste("Year:", yr)) +
    theme_minimal() +
    theme(legend.position = "right")
})

grid.arrange(grobs = plots, ncol = 3)

# 儲存最終結果
saveRDS(data_complete, file = "clustering_results.rds")
cat("\n✓ 分群分析完成！\n")
cat("結果已儲存至: clustering_results.rds\n")
cat(rep("=", 80), "\n")