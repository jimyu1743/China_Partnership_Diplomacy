# ============================================================
# 0. 環境設定與套件載入
# ============================================================

# 設定套件路徑
lib_path <- "D:/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))
options(repos = c(CRAN = "https://cran.rstudio.com/"))

packages <- c(
  "tidyverse", "magrittr", "plm", "haven", "readr", "readxl", "openxlsx", "writexl",
  "ExtremeBounds", "car", "ordinal", "MASS", "mvProbit", "psych", 
  "pglm", "pastecs", "FactoMineR", "factoextra", "GPArotation", 
  "knitr", "kableExtra", "stargazer", "broom" # 新增 stargazer/broom 用於美觀輸出回歸結果
)

# 檢查並安裝缺少的套件
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) install.packages(missing, dependencies = TRUE, lib = lib_path)

invisible(lapply(packages, function(pkg) {
  tryCatch({ suppressPackageStartupMessages(library(pkg, character.only = TRUE)) }, 
           error = function(e) { cat("載入失敗:", pkg, "\n") })
}))

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

# ============================================================================
# 4. 回歸分析資料準備
# ============================================================================

# 步驟 4.1: 建立滯後/領先變數 (Lag Structure)
# 您的需求：每年的自變數 (X_t) 要對下一年的應變數 (Y_{t+1})
# 邏輯：我們建立一個 'partnership_next' 代表下一年的狀態
# 同時建立 'partnership_binary_next'：0=無關係, 1=有關係 (用於 Logit/Probit)

reg_data <- CP9623_pca %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(
    # 建立下一年的夥伴關係等級
    partnership_next = dplyr::lead(partnership, 1),
    
    # 建立二元變數：下一年是否有夥伴關係 (0 vs >0)
    is_partner_next = ifelse(partnership_next > 0, 1, 0),
    
    # 標記：當年是否為0 (我們主要關注從無到有的過程)
    is_zero_now = ifelse(partnership == 0, 1, 0)
  ) %>%
  ungroup() %>%
  # 移除因為 lead() 產生的最後一年 NA
  filter(!is.na(partnership_next))

cat("已建立滯後變數，總樣本數:", nrow(reg_data), "\n")

# ============================================================================
# 5. 定義時期與篩選目標國家
# ============================================================================

# 檢查並轉換 reg_data 的關鍵欄位
# 注意：對因子(factor)轉數值時，必須先轉文字(character)再轉數值(numeric)
# 否則 1996 會變成 1, 1997 會變成 2 (因子的編號)

reg_data <- reg_data %>%
  ungroup() %>% # 確保沒有殘留的分組設定
  mutate(
    # 處理年份
    year = as.numeric(as.character(year)),
    
    # 處理夥伴關係等級 (避免它也是因子)
    partnership = as.numeric(as.character(partnership)),
    partnership_next = as.numeric(as.character(partnership_next))
  )

# 檢查轉換是否成功 (應該要看到 numeric 或 dbl，而不是 factor)
str(reg_data$year)
# 定義函數：找出在特定時期內，從 0 變為 >0 的國家
identify_upgraders <- function(data, start_y, end_y) {
  period_data <- data %>% filter(year >= start_y & year <= end_y)
  
  target_countries <- period_data %>%
    group_by(countrycode) %>%
    summarise(
      min_p = min(partnership, na.rm = TRUE),
      max_p = max(partnership, na.rm = TRUE),
      # 檢查是否有發生 "升級事件" (下一年的狀態 > 當年的狀態，且當年是0)
      has_upgrade_event = any(partnership == 0 & partnership_next > 0, na.rm = TRUE)
    ) %>%
    filter(has_upgrade_event == TRUE) %>% # 嚴格篩選：必須在該時期內發生 0->1 事件
    pull(countrycode)
  
  return(target_countries)
}

# --- 時期 1: 1996-2012 ---
countries_p1 <- identify_upgraders(reg_data, 1996, 2012)
cat("\n[1996-2012] 符合從0升級的國家數:", length(countries_p1), "\n")
cat("名單:", paste(countries_p1, collapse=", "), "\n")

# --- 時期 2: 2013-2022 ---
countries_p2 <- identify_upgraders(reg_data, 2013, 2022)
cat("\n[2013-2022] 符合從0升級的國家數:", length(countries_p2), "\n")
cat("名單:", paste(countries_p2, collapse=", "), "\n")

# ============================================================================
# 6. 建立回歸樣本 (Risk Set)
# ============================================================================

# 為了執行 Probit/Logit 分析「升級機率」，我們通常採取 Event History (Onset) 的邏輯：
# 樣本僅包含 partnership == 0 的年份。
# 解釋：我們想知道在「還沒有關係」的時候，什麼因素導致「下一年建立關係」。
# 如果包含 partnership > 0 的年份，模型會變成「預測維持關係」，而非「預測升級」。

create_reg_subset <- function(data, countries, start_y, end_y) {
  subset_df <- data %>%
    filter(
      countrycode %in% countries,      # 1. 鎖定發生過升級的國家
      year >= start_y & year <= end_y, # 2. 鎖定時間區間
      partnership == 0                 # 3. 鎖定當年無關係的樣本 (Risk Set)
    )
  return(subset_df)
}

df_period1 <- create_reg_subset(reg_data, countries_p1, 1996, 2012)
df_period2 <- create_reg_subset(reg_data, countries_p2, 2013, 2022)

cat("\n[回歸樣本檢查]\n")
cat("時期 1 (1996-2012) 樣本數:", nrow(df_period1), "| 事件數(Y=1):", sum(df_period1$is_partner_next), "\n")
cat("時期 2 (2013-2022) 樣本數:", nrow(df_period2), "| 事件數(Y=1):", sum(df_period2$is_partner_next), "\n")

# ============================================================================
# 7. 執行 Probit 與 Logit 回歸
# ============================================================================

# 設定自變數 (X)
# 使用您前面 PCA 產生的主成分，加上可能的控制變數
# 注意：請根據您 PCA 結果的實際欄位名稱調整 (這裡是基於您前段代碼的假設)
iv_formula <- paste(
  c("econ_PC1", "econ_PC2", "econ_PC3", # 經濟維度
    "sanct_PC1",                        # 制裁維度
    "gov_PC1",                          # 治理維度
    "dist_std",                         # 地理距離
    "dip_age_std",                      # 建交年齡
    "FTA",                              # 與中國是否簽署FTA
    "ORG"                               # 是否加入中國主導之國際組織
  ), 
  collapse = " + "
)

full_formula <- as.formula(paste("is_partner_next ~", iv_formula))

cat("\n使用的回歸公式:", paste("is_partner_next ~", iv_formula), "\n\n")

# --- 函數：執行模型並整理結果 ---
run_models <- function(data, period_name) {
  if(nrow(data) < 10) {
    cat("樣本過少，無法執行", period_name, "的回歸分析\n")
    return(NULL)
  }
  
  # 1. Logit Model
  logit_fit <- glm(full_formula, family = binomial(link = "logit"), data = data)
  
  # 2. Probit Model
  probit_fit <- glm(full_formula, family = binomial(link = "probit"), data = data)
  
  cat(paste0("=== ", period_name, " 回歸結果預覽 ===\n"))
  
  # 使用 Stargazer 輸出漂亮的純文字表格
  stargazer(logit_fit, probit_fit, type = "text", 
            title = paste("Results for", period_name),
            column.labels = c("Logit", "Probit"),
            star.cutoffs = c(0.05, 0.01, 0.001))
  
  return(list(logit = logit_fit, probit = probit_fit))
}

# 執行回歸
cat("\n------------------------------------------------------------\n")
cat("執行時期 1 (1996-2012) 回歸\n")
res_p1 <- run_models(df_period1, "Period 1 (1996-2012)")

cat("\n------------------------------------------------------------\n")
cat("執行時期 2 (2013-2022) 回歸\n")
res_p2 <- run_models(df_period2, "Period 2 (2013-2022)")

# ============================================================================
# 8. 儲存結果與係數輸出
# ============================================================================

output_dir <- "D:/R_workspace/Regression_Results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 整理係數表函數
save_coefs <- function(model_list, p_name) {
  if(is.null(model_list)) return()
  
  # 提取 Logit 結果
  tidy_logit <- tidy(model_list$logit) %>% mutate(Model = "Logit", Period = p_name)
  # 提取 Probit 結果
  tidy_probit <- tidy(model_list$probit) %>% mutate(Model = "Probit", Period = p_name)
  
  bind_rows(tidy_logit, tidy_probit)
}

all_results <- bind_rows(
  save_coefs(res_p1, "1996-2012"),
  save_coefs(res_p2, "2013-2022")
)

# 寫出 Excel
write_xlsx(all_results, path = file.path(output_dir, "Regression_Coefficients.xlsx"))

cat("\n✅ 分析完成！\n")
cat("係數結果已儲存至:", file.path(output_dir, "Regression_Coefficients.xlsx"), "\n")

# 若您需要輸出比對表格 (Odd Ratios for Logit)，可執行以下：
if(!is.null(res_p2$logit)) {
  cat("\n[2013-2022 Logit] Odds Ratios (勝算比):\n")
  print(exp(coef(res_p2$logit)))
}

