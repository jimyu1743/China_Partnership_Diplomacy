# ============================================================
# 0. 環境設定與套件載入
# ============================================================

# 設定套件路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# 定義套件清單 (已加入 flexclust)
packages <- c(
  # --- 核心套件 ---
  "tidyverse", "magrittr",
  
  # --- 資料處理 ---
  "plm", "haven", "readr", "readxl", "openxlsx", "writexl",
  
  # --- 統計分析 ---
  "ExtremeBounds", "car", "ordinal", "MASS", "mvProbit", "psych", 
  "pglm", "pastecs",
  
  # --- 因素分析與分群 (重點區塊) ---
  "FactoMineR", "factoextra", "GPArotation", "cluster", "NbClust", 
  "flexclust",  # <--- 新增這裡：這是執行 kcca/kmedians 必須的套件
  
  # --- 工具 ---
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
  # 使用 tryCatch 避免單一套件載入失敗導致程式中斷
  tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }, error = function(e) {
    cat("載入失敗:", pkg, "\n")
  })
}))

cat("完成！已載入", length(packages), "個套件\n\n")

# ============================================================================
# 1. 資料讀取與前處理
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("資料讀取與前處理\n")
cat(rep("=", 80), "\n\n")

# 讀入資料檔
China_partnership_1996_2023_breakpoint_2025_03_24_02 <- read_excel(
  "C:/Users/jimyu1743/Desktop/習近平與中國夥伴關係外交/原始與整理資料/China partnership 1996-2023 breakpoint 2025.03.24.02.xlsx"
)
CP9623 <- China_partnership_1996_2023_breakpoint_2025_12_13

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

# 標準化指定變數
CP9623 <- CP9623 %>%
  mutate(
    across(
      .cols = c("dist", "dip_age", "population_total", "gdp", "gdp_per_capita",
                "china_ex_to_i", "china_im_fr_i", "exportdep", "importdep", 
                "economy","arms_and_military",
                "trade", "arms", "military", "financial", "travel","WGI",
                "va", "psv", "ge", "rq", "rl", "cc", "WGI","WGI_diff_CHN","gdp_per_capita_diff_CHN","FTA","ORG"), 
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

#=============================================================
#分群分析
#=============================================================

# ============================================================
# 1. 資料準備與清洗
# ============================================================

target_list <- c(
  "AGO", "ARE", "ARG", "AUS", "AUT", "BEL", "BEN", "BGD", "BGR", "BLR", 
  "BOL", "BRN", "CAN", "CHE", "CHL", "COD", "COG", "COL", "CRI", "CYP", 
  "CZE", "DEU", "DJI", "DNK", "DZA", "ECU", "EGY", "ESP", "ETH", "FIN", 
  "FJI", "FRA", "GAB", "GBR", "GEO", "GNQ", "GRC", "HRV", "HUN", "IDN", 
  "IND", "IRL", "IRN", "IRQ", "ISR", "ITA", "JAM", "JOR", "KAZ", "KEN", 
  "KGZ", "KHM", "KOR", "KWT", "LAO", "LKA", "MAR", "MDV", "MEX", "MMR", 
  "MNG", "MOZ", "MYS", "NAM", "NGA", "NLD", "NZL", "OMN", "PAK", 
  "PER", "PHL", "PNG", "POL", "PRT", "QAT", "ROM", "SAU", "SDN", "SEN", 
  "SGP", "SLE", "SUR", "SWE", "THA", "TJK", "TKM", "TTO", "TUR", "TZA", 
  "UKR", "URY", "UZB", "VNM", "WSM", "ZAF", "ZMB", "ZWE"
)

cluster_vars <- c(
  "exportdep_std", "importdep_std", "population_total_std",
  "gdp_per_capita_diff_CHN_std", "WGI_diff_CHN_std",
  "economy_std", "arms_and_military_std",
  "FTA_std", "ORG_std", "dip_age_std", "dist_std"
  )

# 篩選資料
df_base <- CP9623_pca %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(countrycode %in% target_list)

# 變數防呆檢查
valid_vars <- intersect(cluster_vars, names(df_base))

# 彙整資料：Start Year 與 Median Features
start_years <- df_base %>%
  filter(partnership > 0) %>%
  group_by(countrycode) %>%
  summarise(start_year = min(year), .groups = "drop")

# 使用 mean 或 median 皆可，這裡為配合 K-medians 精神，若數據有極端值建議用 mean/median 混合
# 但為了保持與先前變數一致性，這裡先計算平均特徵
country_features <- df_base %>%
  group_by(countrycode) %>%
  summarise(across(all_of(valid_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(complete.cases(across(all_of(valid_vars))))

final_data <- country_features %>%
  inner_join(start_years, by = "countrycode")

# 標準化
df_scaled <- final_data
df_scaled[, valid_vars] <- scale(final_data[, valid_vars])

cat("資料準備完成，樣本數:", nrow(final_data), "\n")

# ============================================================
# 2. 自動決定最佳 K 值 (輪廓係數法)
# ============================================================

cat("正在計算最佳分群數 (Silhouette Method)...\n")

# 使用 factoextra 的自動檢測功能
# 注意：為了符合 K-medians 的邏輯，我們這裡用 PAM (Partitioning Around Medoids) 算法來估算最佳 K
# 因為 PAM 也是基於中位數/代表點的 robust 算法，與 K-medians 最接近
sil_plot <- fviz_nbclust(df_scaled[, valid_vars], cluster::pam, method = "silhouette", k.max = 10) +
  labs(title = "最佳 K 值檢測 (輪廓係數法)", subtitle = "數值越高代表分群越好")

print(sil_plot)

# ★ 自動抓取圖中最高的那個點作為 best_k
sil_data <- sil_plot$data
best_k <- as.numeric(as.character(sil_data$clusters[which.max(sil_data$y)]))

cat(paste0("\n✅ 系統偵測到的最佳分群數 (Best K) 為: ", best_k, "\n"))

# ============================================================
# 3. 執行 K-medians 分群 (使用 Best K)
# ============================================================

set.seed(123)
kmedians_res <- kcca(df_scaled[, valid_vars], k = best_k, 
                     family = kccaFamily("kmedians"),
                     control = list(initcent = "kmeanspp"))

# 存回結果
final_data$cluster <- as.factor(predict(kmedians_res))

cat("分群執行完畢！\n")

# ============================================================
# 4. 視覺化結果
# ============================================================

# --- 熱圖 (Heatmap) ---
cluster_centers <- final_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(valid_vars), median), .groups = "drop") %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

p1 <- ggplot(cluster_centers, aes(x = variable, y = cluster, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste0("K-medians (k=", best_k, ") 特徵熱圖"), 
       subtitle = "使用中位數特徵：紅色=高, 藍色=低", x = "", y = "Cluster")

print(p1)

# --- 時間溯源圖 (Timeline) ---
p2 <- ggplot(final_data, aes(x = start_year, fill = cluster)) +
  geom_histogram(binwidth = 1, position = "stack", alpha = 0.9, color = "white") +
  scale_x_continuous(breaks = seq(min(final_data$start_year), 2023, 2)) +
  facet_wrap(~cluster, ncol = 1) + 
  theme_minimal() +
  labs(title = "時間溯源：建立夥伴關係年份分佈",
       subtitle = paste0("基於 K=", best_k, " 的分群結果"),
       x = "建立關係年份 (Start Year)", y = "國家數量")

print(p2)

# ============================================================
# 5. 輸出名單 (含年份)
# ============================================================

cat(paste0("\n=== K-medians (K=", best_k, ") 分群名單 ===\n"))

for(i in 1:best_k) {
  cat(paste0("\n[Cluster ", i, "] 國家清單 (按年份排序):\n"))
  
  ctrys <- final_data %>% 
    filter(cluster == i) %>% 
    arrange(start_year) %>% 
    mutate(info = paste0(countrycode, "(", start_year, ")")) %>% 
    pull(info)
  
  cat(paste(ctrys, collapse = ", "), "\n")
}

# 設定想輸出的資料夾路徑 (請依您的電腦修改)
output_dir <- "D:/R/Research_result"


# 組合路徑並匯出
# file.path 會自動幫您加上正確的斜線
write.csv(final_data, 
          file.path(output_dir, "Auto_Kmedians_Result.csv"), 
          row.names = FALSE)

cat("檔案已儲存至:", file.path(output_dir, "Auto_Kmedians_Result.csv"), "\n")

# ============================================================
# 繪製 K-medians 分群地圖 (修正版)
# ============================================================

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis) # 載入 viridis 套件以獲得更好的多色階配色

# 1. 取得世界地圖基底
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# 2. 整理分群資料 (包含修復 pseries 錯誤的關鍵步驟)
map_input <- final_data %>%
  ungroup() %>%  # 解除群組狀態以防萬一
  select(countrycode, cluster) %>%
  mutate(
    # ★ 關鍵修正：強制將 pseries 轉為純文字 character
    countrycode = as.character(countrycode),
    # 確保 cluster 是因子
    cluster = as.factor(cluster)
  )

# 3. 合併資料 (現在不會報錯了)
merged_map <- world_map %>%
  left_join(map_input, by = c("iso_a3" = "countrycode"))

# 4. 繪圖 (針對 K-medians 8群 優化配色)
ggplot(data = merged_map) +
  # 繪製國家 (邊框設細一點 size=0.1 比較精緻)
  geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
  
  # 設定投影 (Robinson 是標準世界地圖投影)
  coord_sf(crs = "+proj=robin") +
  
  # ★ 配色調整：因為有 8 群，使用 'plasma' 或 'turbo' 色票區別度較高
  # na.value 設定灰色，代表沒有被分群的國家
  scale_fill_viridis_d(option = "turbo", na.value = "#eeeeee") +
  
  # 外觀修飾
  labs(title = paste0("全球分群態勢圖 (K-medians, K=", length(unique(na.omit(map_input$cluster))), ")"),
       subtitle = "灰色區域為未納入分析之國家",
       fill = "群組 (Cluster)",
       caption = "資料來源: CIER Research / Method: K-medians (Manhattan Distance)") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",       # 圖例放下面
    legend.key.width = unit(1.5, "cm"), # 拉寬圖例讓顏色更清楚
    panel.grid = element_blank(),     # 去除網格
    axis.text = element_blank()       # 去除經緯度數字
  )


# ============================================================
# 自動決定最佳 K 值 (輪廓係數法 - K-Means 版本)
# ============================================================

cat("正在計算最佳分群數 (Silhouette Method using K-Means)...\n")

# 使用 factoextra 的自動檢測功能
# 修改點：FUN = kmeans (原本是 cluster::pam)
sil_plot <- fviz_nbclust(df_scaled[, valid_vars], kmeans, method = "silhouette", k.max = 10) +
  labs(title = "最佳 K 值檢測 (輪廓係數法 - K-Means)", subtitle = "數值越高代表分群越好")

print(sil_plot)

# ★ 自動抓取圖中最高的那個點作為 best_k
sil_data <- sil_plot$data
best_k <- as.numeric(as.character(sil_data$clusters[which.max(sil_data$y)]))

cat(paste0("\n✅ 系統偵測到的最佳分群數 (Best K) 為: ", best_k, "\n"))

# ============================================================
# 執行 K-Means 分群 (使用 Best K)
# ============================================================

set.seed(123)
# 修改點：使用標準 kmeans 函式
# nstart = 25 是為了避免陷入局部最佳解，讓它隨機嘗試 25 次起點並選最好的
kmeans_res <- kmeans(df_scaled[, valid_vars], centers = best_k, nstart = 25)

# 存回結果
final_data$cluster <- as.factor(kmeans_res$cluster)

cat("K-Means 分群執行完畢！\n")

# ============================================================
# 視覺化結果
# ============================================================

# --- 熱圖 (Heatmap) ---
# 修改點：因為是 K-Means，群中心應該看 "Mean" (平均值) 而非 Median
cluster_centers <- final_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(valid_vars), mean), .groups = "drop") %>% 
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

p1 <- ggplot(cluster_centers, aes(x = variable, y = cluster, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste0("K-Means (k=", best_k, ") 特徵熱圖"), 
       subtitle = "使用平均值特徵：紅色=高, 藍色=低", x = "", y = "Cluster")

print(p1)

# --- 時間溯源圖 (Timeline) ---
p2 <- ggplot(final_data, aes(x = start_year, fill = cluster)) +
  geom_histogram(binwidth = 1, position = "stack", alpha = 0.9, color = "white") +
  scale_x_continuous(breaks = seq(min(final_data$start_year, na.rm = TRUE), 2023, 2)) +
  facet_wrap(~cluster, ncol = 1) + 
  theme_minimal() +
  labs(title = "時間溯源：建立夥伴關係年份分佈",
       subtitle = paste0("基於 K-Means (K=", best_k, ") 的分群結果"),
       x = "建立關係年份 (Start Year)", y = "國家數量")

print(p2)

# ============================================================
# 輸出名單 (含年份) 與 存檔
# ============================================================

cat(paste0("\n=== K-Means (K=", best_k, ") 分群名單 ===\n"))

for(i in 1:best_k) {
  cat(paste0("\n[Cluster ", i, "] 國家清單 (按年份排序):\n"))
  
  ctrys <- final_data %>% 
    filter(cluster == i) %>% 
    arrange(start_year) %>% 
    mutate(info = paste0(countrycode, "(", start_year, ")")) %>% 
    pull(info)
  
  cat(paste(ctrys, collapse = ", "), "\n")
}

# 設定輸出路徑
output_dir <- "D:/R/Research_result"

# 建立目錄 (若不存在)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 儲存檔案
output_file <- file.path(output_dir, "Auto_Kmeans_Result.csv")

write.csv(final_data, output_file, row.names = FALSE)

cat("檔案已儲存至:", output_file, "\n")


#==============================================================
#繪製分群國家分布圖
#==============================================================

# --- 載入套件 ---
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# --- 準備地圖資料 ---
# 1. 取得世界地圖基底 (使用 medium 解析度)
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# 2. 整理您的分群資料
map_input <- final_data %>%
  # ★ 關鍵修正：先解除 group 狀態 (若有的話)，並強制轉型
  ungroup() %>% 
  select(countrycode, cluster) %>%
  mutate(
    cluster = as.factor(cluster),
    # 把 pseries 轉回一般的 character
    countrycode = as.character(countrycode) 
  )
# 檢查一下：現在應該要是 <chr> 而不是 <pseries>
print(class(map_input$countrycode))
# 3. 合併資料 (利用 ISO 代碼對接)
merged_map <- world_map %>%
  left_join(map_input, by = c("iso_a3" = "countrycode"))
# --- 繪圖 (ggplot2) ---
ggplot(data = merged_map) +
  # 繪製國家多邊形
  geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
  
  # 設定投影方式 (Robinson 投影，適合世界地圖)
  coord_sf(crs = "+proj=robin") +
  
  # 設定配色 (使用 Set1 鮮明配色，若群數超過 9 群建議改用 viridis)
  scale_fill_brewer(palette = "Set1", na.value = "#eeeeee") +
  
  # 外觀設定
  labs(title = paste0("全球分群分布圖 (K=", length(unique(na.omit(map_input$cluster))), ")"),
       subtitle = "灰色區域為未納入分析或資料缺失的國家",
       fill = "所屬群組",
       caption = "資料來源: CIER Research") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
