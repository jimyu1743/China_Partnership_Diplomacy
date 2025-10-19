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