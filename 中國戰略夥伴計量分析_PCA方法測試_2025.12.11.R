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
    valid_rows <- rownames(clean_data)
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
write_xlsx(CP9623_pca, path = "CP9623_pca.xlsx")
# ============================================================
# 1. 資料準備
# ============================================================

# 目標國家清單
target_list <- c(
  "AGO", "ARE", "ARG", "AUS", "AUT", "BEL", "BEN", "BGD", "BGR", "BLR", 
  "BOL", "BRN", "CAN", "CHE", "CHL", "COD", "COG", "COL", "CRI", "CYP", 
  "CZE", "DEU", "DJI", "DNK", "DZA", "ECU", "EGY", "ESP", "ETH", "FIN", 
  "FJI", "FRA", "GAB", "GBR", "GEO", "GNQ", "GRC", "HRV", "HUN", "IDN", 
  "IND", "IRL", "IRN", "IRQ", "ISR", "ITA", "JAM", "JOR", "KAZ", "KEN", 
  "KGZ", "KHM", "KOR", "KWT", "LAO", "LKA", "MAR", "MDV", "MEX", "MMR", 
  "MNG", "MOZ", "MYS", "NAM", "NGA", "NLD", "NPL", "NZL", "OMN", "PAK", 
  "PER", "PHL", "PNG", "POL", "PRT", "QAT", "ROM", "SAU", "SDN", "SEN", 
  "SGP", "SLE", "SUR", "SWE", "THA", "TJK", "TKM", "TTO", "TUR", "TZA", 
  "UKR", "URY", "UZB", "VNM", "WSM", "ZAF", "ZMB", "ZWE"
)

# 定義分群變數
cluster_vars <- c(
  "exportdep_std", "importdep_std", "population_total_std",
  "gdp_per_capita_std", "WGI", "trade_std", "arms_std", 
  "military_std", "financial_std", "travel_std", 
  "FTA", "ORG", "dip_age_std", "dist_std"
)

# 篩選資料
df_base <- CP9623_pca %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(countrycode %in% target_list)

# 步驟 A: 找出每個國家「建立夥伴關係」的年份 (Start Year)
# 邏輯：找出該國 partnership > 0 的最小年份
start_years <- df_base %>%
  filter(partnership > 0) %>%
  group_by(countrycode) %>%
  summarise(start_year = min(year), .groups = "drop")

# 步驟 B: 計算每個國家的「整體特徵平均值」 (不分時期)
country_features <- df_base %>%
  group_by(countrycode) %>%
  summarise(across(all_of(cluster_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(complete.cases(across(all_of(cluster_vars)))) # 移除缺失值

# 合併特徵與開始年份 (確保只有資料完整的國家進入分析)
final_data <- country_features %>%
  inner_join(start_years, by = "countrycode")

cat("最終分析樣本數:", nrow(final_data), "\n")

library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

# ============================================================
# 2. 自動決定最佳 K 值 (輪廓係數法) & 執行 K-means
# ============================================================

# 標準化 (Scale)
df_scaled <- final_data
df_scaled[, cluster_vars] <- scale(final_data[, cluster_vars])
rownames(df_scaled) <- final_data$countrycode

# 自動偵測最佳 K 值
cat("正在計算最佳分群數 (Silhouette Method)...\n")

# 繪製輪廓係數圖 
sil_plot <- fviz_nbclust(df_scaled[, cluster_vars], kmeans, method = "silhouette", k.max = 10) +
  labs(title = "最佳 K 值檢測 (輪廓係數法)", subtitle = "越高分代表分群越好")
print(sil_plot)

# ★ 自動抓取圖中最高的那個點作為 best_k
sil_data <- sil_plot$data
best_k <- as.numeric(as.character(sil_data$clusters[which.max(sil_data$y)]))

cat(paste0("\n✅ 系統偵測到的最佳分群數 (Best K) 為: ", best_k, "\n"))
cat("正在使用最佳 K 值執行分析...\n")

# --- 使用最佳 K 值執行 K-means ---
set.seed(123)
km_res <- kmeans(df_scaled[, cluster_vars], centers = best_k, nstart = 50)

# 將分群結果併回原始資料
final_data$cluster <- as.factor(km_res$cluster)

# ============================================================
# 3. 分析結果視覺化
# ============================================================

# --- 圖表 1: 各群組的特徵熱圖 ---
cluster_centers <- final_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(cluster_vars), mean), .groups = "drop") %>%
  pivot_longer(
    cols = -cluster, 
    names_to = "variable", 
    values_to = "value"
  )

p1 <- ggplot(cluster_centers, aes(x = variable, y = cluster, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste0("各群組特徵熱圖 (K=", best_k, ")"), 
       subtitle = "紅色 = 高於平均 (顯著特徵), 藍色 = 低於平均", 
       x = "", y = "Cluster")
print(p1)

# --- 圖表 2: [核心] 溯源分析 - 時間分佈 ---
p2 <- ggplot(final_data, aes(x = start_year, fill = cluster)) +
  geom_histogram(binwidth = 1, position = "stack", alpha = 0.8, color = "white") +
  scale_x_continuous(breaks = seq(1996, 2023, 2)) + 
  facet_wrap(~cluster, ncol = 1) + 
  theme_minimal() +
  labs(title = "時間溯源：各群組建立夥伴關係的年份分佈",
       subtitle = paste0("基於自動偵測 K=", best_k, " 的分群結果"),
       x = "建立關係年份 (Start Year)",
       y = "國家數量")

print(p2)

# --- 文字報告: 輸出詳細清單 ---
cat("\n=== 分析報告 ===\n")
cat(paste("採用分群數:", best_k, "\n"))

# 計算各群組統計
summary_stats <- final_data %>%
  group_by(cluster) %>%
  summarise(
    n_countries = n(),
    avg_start_year = round(mean(start_year), 1),
    median_start_year = median(start_year),
    top_features = "請參照熱圖"
  )
print(summary_stats)

# 列出各群組的國家與年份
for(i in 1:best_k) {
  cat(paste0("\n[Cluster ", i, "] 國家清單 (建立年份):\n"))
  cluster_data <- final_data %>% 
    filter(cluster == i) %>% 
    arrange(start_year)
  
  # 格式化輸出: 國家(年份)
  output_str <- paste(cluster_data$countrycode, "(", cluster_data$start_year, ")", sep = "")
  cat(paste(output_str, collapse = ", "), "\n")
}

# 匯出 CSV
filename <- paste0("Cluster_Timeline_Analysis_K", best_k, ".csv")
write.csv(final_data, filename, row.names = FALSE)
cat(paste0("\n檔案已匯出至: ", filename, "\n"))
