library(dplyr)

# 先將year轉換為數值
panel_CP9623_pca$year <- as.numeric(as.character(panel_CP9623_pca$year))

# 計算每個國家每年的變化
partnership_changes <- panel_CP9623_pca %>%
  filter(year >= 1997) %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(
    partnership_lag = lag(partnership),
    partnership_change = partnership - lag(partnership),
    is_increase = partnership_change > 0
  ) %>%
  filter(!is.na(partnership_change))

# 篩選出有增加的情況
increases_only <- partnership_changes %>%
  filter(is_increase == TRUE)

# 查看結果
print(increases_only)

# 按年份查看哪些國家有增加
yearly_summary <- increases_only %>%
  group_by(year) %>%
  summarise(
    countries = paste(countrycode, collapse = ", "),
    n_countries = n()
  )

print(yearly_summary)

# 或者用迴圈逐年顯示
for(y in sort(unique(increases_only$year))) {
  cat("\n年份:", y, "\n")
  countries <- increases_only %>%
    filter(year == y) %>%
    pull(countrycode)
  cat("增加的國家:", paste(countries, collapse = ", "), "\n")
}
# 匯出完整的增加資料(包含國家、年份、partnership值、變化量等)
write.csv(increases_only, 
          file = "partnership_increases.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
