library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)

options(scipen = 9999)

# faster than read_csv
df = fread("data/flights_sample_2m.csv", header=TRUE)

#---------------------------------------
df <- df |>
  mutate(
    FL_DATE = as.Date(FL_DATE),
    YEAR = year(FL_DATE),
    MONTH = month(FL_DATE)
  )

############################################
# 1. PHÂN TÍCH MÔ TẢ (DESCRIPTIVE ANALYSIS)
############################################
#------------------------------------------
# 1.1 Tổng quan
#------------------------------------------
# Số chuyến bay hàng năm

flights_yearly <- df |>
  group_by(YEAR) |>
  summarise(Total_Flights = n(), .groups = "drop")

ggplot(flights_yearly,
       aes(YEAR, Total_Flights)) + 
  geom_col(fill="darkorange2") + 
  geom_text(aes(label = scales::comma(Total_Flights)),
            vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Total flights per year (2019-2023)",
       x = "Year", y = "Number of flights") + 
  theme_minimal()

#------------------------------------------
# Số chuyến bay theo tháng của từng năm
flights_monthly <- df |>
  group_by(YEAR, MONTH) |>
  summarise(Total_Flights = n(), .groups = "drop")

ggplot(flights_monthly, 
       aes(x=MONTH, y=Total_Flights, 
                            color = factor(YEAR))) + 
  geom_line(size=1) +
  geom_point() + 
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Monthly Flights Trend (2019-2023)",
       x = "Month", y = "Number of Flights",
       color = "Year") +
  theme_minimal()

#------------------------------------------
# Hãng hàng không nào có nhiều chuyến bay nhất mỗi năm?

# Tổng số chuyến bay theo năm và hãng
flights_airline_yearly <- df |>
  group_by(YEAR, AIRLINE) |>
  summarise(Total_Flights = n(),
            .groups = "drop")

# tổng toàn ngành mỗi năm
flights_industry_total <- flights_airline_yearly |>
  group_by(YEAR) |>
  summarise(Industry_Total = sum(Total_Flights),
            .group = "drop")

# Tính % thay đổi so với năm trước (tốc độ phục hồi)
flights_airline_vs_industry <- flights_airline_yearly |>
  left_join(flights_industry_total, by = "YEAR") |>
  group_by(AIRLINE) |>
  mutate(Growth_Rate = (Total_Flights / lag(Total_Flights) -1)*100) |>
  ungroup()

flights_industry_total <- flights_industry_total |>
  mutate(Industry_Growth = (Industry_Total/lag(Industry_Total)-1)*100)

#------------------------------------------
# xác định hãng top của 2019
airline_top <- flights_airline_vs_industry |>
  filter(YEAR == "2019") |>
  arrange(desc(Total_Flights)) |>
  slice(1) |>
  pull(AIRLINE)

# So sánh tốc độ phục hồi
compare <- flights_airline_vs_industry |>
  filter(AIRLINE == airline_top) |>
  select(YEAR, Growth_Rate) |>
  left_join(flights_industry_total |> 
              select(YEAR, Industry_Growth), 
            by = "YEAR")

print(paste("Top airline is ", airline_top))
print(compare)

# lineplot
compare |> 
  mutate(YEAR = as.factor(YEAR)) |>
  ggplot(aes(x = YEAR)) + 
  geom_line(aes(y = Growth_Rate, group = 1, 
                color = airline_top), size = 1.2) +
  geom_line(aes(y = Industry_Growth, group=1,
                color = "Industry Average"),
                linetype = "dashed", size=1.2) +
  geom_point(aes(y = Growth_Rate, 
                 color = airline_top),
                  size = 3) +
  geom_point(aes(y = Industry_Growth, 
                 color = "Industry Average"),
                size = 3) +
  labs(
    title = paste("Recovery rate comparision:\n", airline_top, "vs Industry"),
    x = "Year",
    y = "Growth Rate (%)",
    color = "legend"
  ) +
  theme_minimal()

#------------------------------------------
# Sân bay nào là điểm khởi hành (ít) phổ biến nhất?
flights_origin_yearly <- df |>
  group_by(YEAR, ORIGIN, ORIGIN_CITY) |>
  summarise(Total_Flights = n())

# phổ biến nhiều nhất
origin_top <- flights_origin_yearly |>
  group_by(YEAR) |>
  slice_max(Total_Flights, n = 1, 
            with_ties = FALSE) |>
  ungroup()

# phổ biến ít nhất
origin_least <- flights_origin_yearly |>
  group_by(YEAR) |>
  slice_min(Total_Flights, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Sân bay nào là điểm đến (ít) phổ biến nhất?
flights_dest_yearly <- df |>
  group_by(YEAR, DEST, DEST_CITY) |>
  summarise(Total_Flights = n())

# phổ biến nhiều nhất
dest_top <- flights_dest_yearly |>
  group_by(YEAR) |>
  slice_max(Total_Flights, n = 1, 
            with_ties = FALSE) |>
  ungroup()

# phổ biến ít nhất
dest_least <- flights_dest_yearly |>
  group_by(YEAR) |>
  slice_min(Total_Flights, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# 1.2 Delay tổng quát
#------------------------------------------
# Tỉ lệ chuyến bay bị delay (>15 phút) là bao nhiêu?

# tỉ lệ delay theo năm - tháng
delay_rate_yearly <- df |>
  group_by(YEAR, MONTH) |>
  summarise(Delay_Rate = signif(mean(ARR_DELAY > 15, 
                                     na.rm = TRUE) * 100, 3)) |>
  ungroup()
  
delay_rate_yearly1 <- df |>
  mutate(is_delayed = ifelse(!is.na(ARR_DELAY) & ARR_DELAY > 15, 1, 0)) |>
  group_by(YEAR, MONTH) |>
  summarise(
    Delay_Rate = round(mean(is_delayed) * 100, 1)
  ) |>
  ungroup()

# heatmap plot
ggplot(delay_rate_yearly, aes(x = factor(YEAR), y = factor(MONTH), 
                                  fill = Delay_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Delay_Rate, 1)), color = "black", size = 3.5) +
  scale_fill_distiller(palette = "RdPu", name = "Delay Rate (%)") +
  labs(
    title = "Tỷ lệ chuyến bay bị delay >15 phút theo Năm và Tháng",
    x = "Năm", y = "Tháng") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Thời gian delay trung bình là bao nhiêu phút?












