{r}
library(ggplot2)
library(dplyr)
library(lubridate)

# 假设 combined_data 已经是你的整理后的数据集

# 确保日期格式是正确的，并且转换为日期类型
combined_data$`Lease Commencement Date` <- dmy(paste0("01-", combined_data$`Lease Commencement Date`))

# 转换日期为季度
combined_data$Quarter <- as.factor(floor_date(combined_data$`Lease Commencement Date`, "quarter"))

# 确保 'No of Bedroom' 是因子类型
combined_data$`No of Bedroom` <- as.factor(combined_data$`No of Bedroom`)

# 滤除无效数据和非1至5卧室的数据
filtered_data <- combined_data %>%
  filter(!is.na(`Monthly Rent ($)`), `No of Bedroom` %in% 1:5)

# 绘制箱型图
ggplot(filtered_data, aes(x = Quarter, y = `Monthly Rent ($)`, fill = `No of Bedroom`)) +
  geom_boxplot(outlier.shape = NA) +  # 不显示离群点
  labs(
    title = "Monthly Rent Distribution by Quarter and Number of Bedrooms",
    x = "Quarter",
    y = "Monthly Rent ($)",
    fill = "Number of Bedrooms"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转X轴标签以便清晰显示





{r}
library(ggplot2)
library(dplyr)
library(lubridate)

# 确保日期格式是正确的
combined_data$`Lease Commencement Date` <- dmy(paste0("01-", combined_data$`Lease Commencement Date`))

# 仅保留1至5间的房间数
filtered_data <- combined_data %>%
  filter(`No of Bedroom` %in% 1:5)

# 确保 'No of Bedroom' 是因子类型
filtered_data$`No of Bedroom` <- as.factor(filtered_data$`No of Bedroom`)

# 按房间数和月份分组，计算每组的最小值和最大值
rent_summary <- filtered_data %>%
  group_by(`Lease Commencement Date`, `No of Bedroom`) %>%
  summarize(
    min_rent = min(`Monthly Rent ($)`),
    max_rent = max(`Monthly Rent ($)`),
    .groups = 'drop'
  )

# 绘制折线图和范围
ggplot(rent_summary, aes(x = `Lease Commencement Date`, ymin = min_rent, ymax = max_rent, fill = `No of Bedroom`)) + 
  geom_ribbon(alpha = 0.4) + 
  geom_line(aes(y = (min_rent + max_rent) / 2, color = `No of Bedroom`)) + 
  scale_fill_brewer(palette = "Set1") + # 用不同颜色表示不同卧室数
  scale_color_brewer(palette = "Set1") + # 同上
  labs(
    title = "Monthly Rent Range Over Time by Number of Bedrooms",
    x = "Lease Commencement Date",
    y = "Monthly Rent ($)",
    fill = "No of Bedrooms",
    color = "No of Bedrooms"
  ) +
  theme_minimal()
