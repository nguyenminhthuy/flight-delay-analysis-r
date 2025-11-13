library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(scales) # scale_y_continuous(labels = comma)

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
  scale_y_continuous(labels = comma) + 
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
  scale_y_continuous(labels = label_comma()) +
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
ggplot(delay_rate_yearly, aes(x = factor(YEAR), 
                              y = factor(MONTH), 
                              fill = Delay_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Delay_Rate, 1)), 
            color = "black", size = 3.5) +
  scale_fill_distiller(palette = "RdPu", 
                       name = "Delay Rate (%)") +
  labs(
    title = "Tỷ lệ chuyến bay bị delay >15 phút theo Năm và Tháng",
    x = "Năm", y = "Tháng") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#------------------------------------------
# Thời gian delay trung bình là bao nhiêu phút?
flights_delayed <- df |>
  filter(DEP_DELAY > 0)

delay_avg_yearly <- flights_delayed |>
  group_by(YEAR) |>
  summarise(
    Avg_Arrival_Delay = round(mean(DEP_DELAY, na.rm=TRUE), 1)
  ) |>
  ungroup()

# Phân phối độ trễ (histogram): delay thường rơi vào khoảng bao nhiêu phút?
ggplot(flights_delayed, aes(x = DEP_DELAY)) +
  geom_histogram(bins = 50, binwidth = 5,
                 color="darkgreen", 
                 fill = "lightgreen",
                 boundary = 0) +
  coord_cartesian(xlim = c(0, 180)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribution of Arrival Delays",
    x = "Departure Delay (minutes)", 
    y = "Number of Flights") +
  theme_minimal(base_size = 13)

#------------------------------------------
# Tỉ lệ chuyến bay bị hủy (CANCELLED = 1)?

flights_cancelled <- df |>
  filter(CANCELLED == 1)

flights_cancel_yearly <- flights_cancelled |>
  group_by(YEAR) |>
  summarise(
    Total_Cancelled_Flights = n()
  ) |>
  ungroup()

# Tỉ lệ chuyến bay bị hủy hàng năm
df_cancel_merge <- flights_cancel_yearly |>
  left_join(flights_yearly, by = "YEAR") |>
  mutate(Cancelled_Rate = round(Total_Cancelled_Flights / Total_Flights * 100, 1)) |>
  ungroup()

#------------------------------------------
# Tỉ lệ chuyến bị diverted (DIVERTED = 1)?
flights_diverted <- df |>
  filter(DIVERTED == 1)

divert_rate_yearly <- flights_diverted |>
  group_by(YEAR) |>
  summarise(Total_Diverted_Flights = n()) |>
  ungroup()

# Tỉ lệ chuyến bay bị hủy hàng năm
divert_airline_vs_total <- divert_rate_yearly |>
  left_join(flights_yearly, by = "YEAR") |>
  mutate(Diverted_Rate = round(Total_Diverted_Flights / Total_Flights * 100, 1)) |>
  ungroup()

#------------------------------------------
# Tỉ lệ chuyến bay bị hủy theo tháng hàng năm
divert_rate_monthly <- flights_diverted |>
  group_by(YEAR, MONTH) |>
  summarise(Total_Diverted_Flights = n(), .groups = "drop")

divert_airline_vs_total <- divert_rate_monthly |>
  inner_join(flights_monthly, by = c("YEAR", "MONTH")) |>
  mutate(
    Diverted_Rate = round((Total_Diverted_Flights / Total_Flights) * 100, 1)
  ) |> 
  ungroup()

#------------------------------------------
# 1.3 Theo thời gian
#------------------------------------------
# Tỉ lệ delay theo tháng (hoặc quý) thay đổi ra sao?
flights_delayed <- flights_delayed |>
  mutate(QUARTER = ((MONTH - 1) %/% 3) + 1)

# tỉ lệ delay theo tháng
delay_rate_monthly <- flights_delayed |>
  group_by(YEAR, MONTH) |>
  summarise(Total_Delayed_Flights = n()) |>
  ungroup()

# Tỉ lệ chuyến bay bị delay theo quý hàng năm
delay_rate_quarterly <- flights_delayed |>
  group_by(YEAR, QUARTER) |>
  summarise(Total_Delayed_Flights = n()) |>
  ungroup()

#------------------------------------------
# Tỉ lệ delay trung bình theo ngày trong tuần (Monday–Sunday).
flights_delayed <- flights_delayed |>
  mutate(
    DAY_OF_WEEK = weekdays(FL_DATE)
  )

# Define the desired order of weekdays
# Convert the 'DAY_OF_WEEK' column to an ordered factor
flights_delayed <- flights_delayed |>
  mutate(
    DAY_OF_WEEK = factor(DAY_OF_WEEK,
                         levels = c("Monday", "Tuesday", "Wednesday", 
                                    "Thursday", "Friday", "Saturday", "Sunday"),
                         ordered = TRUE)
  )

delay_rate_weekday <- flights_delayed |>
  group_by(YEAR, DAY_OF_WEEK) |>
  summarise(Avg_Delayed_Flights = round(mean(DEP_DELAY, na.rm = TRUE), 1)) |>
  ungroup()

#------------------------------------------
# Delay trung bình theo giờ khởi hành (CRS_DEP_TIME) — giờ cao điểm có nhiều delay hơn không?
flights_delayed <- flights_delayed |>
  mutate(DEP_HOUR = CRS_DEP_TIME %/% 100)

delay_avg_hourly <- flights_delayed |>
  group_by(DEP_HOUR) |>
  summarise(Avg_Departure_Delay = round(mean(DEP_DELAY, na.rm = TRUE),1)) |>
  ungroup()

ggplot(delay_avg_hourly, 
       aes(x = DEP_HOUR, y = Avg_Departure_Delay)) + 
  geom_line(linetype = "twodash", color="blue", size=0.7) +
  geom_point(color="red", size=1) + 
  labs(title = "Average Departure Delay by Scheduled Departure Hour",
       x = "Scheduled Departure Hour (0–23)", 
       y = "Average Departure Delay (minutes)") +
  theme_minimal()

#------------------------------------------
# Có xu hướng delay tăng vào mùa đông hoặc mùa mưa không?
flights_delayed <- flights_delayed |>
  mutate(
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      MONTH %in% c(9, 10, 11) ~ "Fall"
    )
  )

delay_avg_seasonal <- flights_delayed |>
  group_by(YEAR, SEASON) |>
  summarise(Avg_Delayed_Flights = round(mean(DEP_DELAY), 1)) |>
  ungroup()

ggplot(delay_avg_seasonal, aes(x = SEASON, 
                              y = factor(YEAR), 
                              fill = Avg_Delayed_Flights)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Avg_Delayed_Flights, 1)), 
            color = "black", size = 3.5) +
  scale_fill_distiller(palette = "YlOrBr", 
                       name = "Average Delay (Minutes)") +
  labs(
    title = "Average Delay (minutes) \nby Season",
    x = "Season", y = "Năm") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#------------------------------------------
# 1.4 Theo hãng và sân bay
#------------------------------------------
# Hãng hàng không nào có delay trung bình cao nhất / thấp nhất?
delay_airline <- flights_delayed |>
  group_by(YEAR, AIRLINE) |>
  summarise(Delay_Rate = round(mean(ARR_DELAY, na.rm = TRUE), 1)) |>
  ungroup()

airline_delay_top <- delay_airline |>
  group_by(YEAR) |>
  slice_max(Delay_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

airline_delay_least <- delay_airline |>
  group_by(YEAR) |>
  slice_min(Delay_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Tỉ lệ chuyến bay bị hủy hàng năm
cancel_rate_yearly <- flights_cancelled |>
  group_by(YEAR) |>
  summarise(Total_Cancelled_Flights = n()) |>
  ungroup()

df_cancel_merge <- cancel_rate_yearly |>
  left_join(flights_yearly, by="YEAR") |>
  mutate(
    Cancelled_Rate = round(Total_Cancelled_Flights / Total_Flights * 100, 1)
  ) |>
  ungroup()

#------------------------------------------
# Hãng nào có tỉ lệ hủy cao nhất?
cancel_rate_airline <- flights_cancelled |>
  group_by(YEAR, AIRLINE) |>
  summarise(Cancelled_Flights = n()) |>
  ungroup()
 
cancel_airline_vs_total <- cancel_rate_airline |>
  inner_join(flights_airline_yearly, by=c("YEAR", "AIRLINE")) |>
  mutate(
    Cancelled_Rate = round(Cancelled_Flights / Total_Flights * 100, 1)
  ) |>
  ungroup()

airline_cancel_top <- cancel_airline_vs_total |>
  group_by(YEAR) |>
  slice_max(Cancelled_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

airline_cancel_least <- cancel_airline_vs_total |>
  group_by(YEAR) |>
  slice_min(Cancelled_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Sân bay nào có delay lớn nhất (tính theo ORIGIN)?
delay_origin <- flights_delayed |>
  group_by(YEAR, ORIGIN, ORIGIN_CITY) |>
  summarise(Delay_Rate = round(mean(ARR_DELAY, na.rm = TRUE), 1)) |>
  ungroup()

origin_delay_top <- delay_origin |>
  group_by(YEAR) |>
  slice_max(Delay_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

origin_delay_least <- delay_origin |>
  group_by(YEAR) |>
  slice_min(Delay_Rate, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Sân bay nào thường xuyên gặp TAXI_OUT dài nhất?
taxiout_origin <- df |>
  group_by(YEAR, ORIGIN, ORIGIN_CITY) |>
  summarise(Avg_Out_Time = round(mean(TAXI_OUT, na.rm = TRUE), 1)) |>
  ungroup()

origin_taxiout_top <- taxiout_origin |>
  group_by(YEAR) |>
  slice_max(Avg_Out_Time, n = 1, 
            with_ties = FALSE) |>
  ungroup()

origin_taxiout_least <- taxiout_origin |>
  group_by(YEAR) |>
  slice_min(Avg_Out_Time, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Sân bay nào thường xuyên gặp TAXI_IN dài nhất?
taxiin_origin <- df |>
  group_by(YEAR, ORIGIN, ORIGIN_CITY) |>
  summarise(Avg_In_Time = round(mean(TAXI_IN, na.rm = TRUE), 1)) |>
  ungroup()

origin_taxiin_top <- taxiin_origin |>
  group_by(YEAR) |>
  slice_max(Avg_In_Time, n = 1, 
            with_ties = FALSE) |>
  ungroup()

origin_taxiin_least <- taxiin_origin |>
  group_by(YEAR) |>
  slice_min(Avg_In_Time, n = 1, 
            with_ties = FALSE) |>
  ungroup()

############################################
# 2. PHÂN TÍCH NGUYÊN NHÂN (DIAGNOSTIC ANALYSIS)
############################################
























