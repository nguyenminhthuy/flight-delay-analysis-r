library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(scales) # scale_y_continuous(labels = comma)
library(hexbin) # Hexbin plot (nhẹ hơn rất nhiề so vs scatter)

# options(scipen = 9999)

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

df_flights <- copy(df)
flights_yearly <- df_flights |>
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
flights_monthly <- df_flights |>
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
flights_airline_yearly <- df_flights |>
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
flights_origin_yearly <- df_flights |>
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
flights_dest_yearly <- df_flights |>
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
df_flights_delay <- copy(df)
# Tỉ lệ chuyến bay bị delay (>15 phút) là bao nhiêu?

# tỉ lệ delay theo năm - tháng
delay_rate_yearly <- df_flights_delay |>
  mutate(is_delayed = ifelse(!is.na(ARR_DELAY) & ARR_DELAY > 15, 1, 0)) |>
  group_by(YEAR, MONTH) |>
  summarise(
    Delay_Rate = round(mean(is_delayed) * 100, 1)
  ) |>
  ungroup()

# heatmap plot
ggplot(delay_rate_yearly, aes(x = factor(YEAR), 
                              y = factor(MONTH, levels = rev(1:12)), 
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
# 15p mới record nguyên nhân delay
flights_delayed <- df_flights_delay |>
  filter(ARR_DELAY > 15)

delay_avg_yearly <- flights_delayed |>
  group_by(YEAR) |>
  summarise(
    Avg_Arrival_Delay = round(mean(ARR_DELAY, na.rm=TRUE), 1)
  ) |>
  ungroup()

# Phân phối độ trễ (histogram): delay thường rơi vào khoảng bao nhiêu phút?
ggplot(flights_delayed, aes(x = ARR_DELAY)) +
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
df_flights_cancel <- copy(df)

flights_cancelled <- df_flights_cancel |>
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
df_flights_divert <- copy(df)

flights_diverted <- df_flights_divert |>
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
taxiout_origin <- df_flights |>
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
taxiin_origin <- df_flights |>
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
#------------------------------------------
# 2.1 Nguyên nhân delay
#------------------------------------------
# Trong các loại delay, loại nào chiếm nhiều thời gian nhất 
# (carrier / weather / NAS / late aircraft / security)?

# Tự động tìm các cột có tên bắt đầu bằng 'DELAY_DUE_'
reason_cols <- grep("^DELAY_DUE_", names(df_flights), value = TRUE)

# Lấy delay và xem khi nào bắt đầu có dữ liệu nguyên nhân
df_flights <- df_flights |>
  mutate(HAS_REASON = if_any(all_of(reason_cols), ~ !is.na(.x)))

# Tìm ngưỡng delay nhỏ nhất mà từ đó trở lên có nguyên nhân
threshold <- df_flights |>
  filter(HAS_REASON) |>
  summarise(min_delay = min(ARR_DELAY, na.rm = TRUE))

print(threshold)

# tính tổng các DELAY_DUE_ qua các năm
delay_reason_total_yearly <- df_flights |>
  group_by(YEAR) |>
  summarise(across(all_of(reason_cols), ~ sum(.x, na.rm = TRUE))) |>
  ungroup()

# stacked bar chart
delay_long <- delay_reason_total_yearly |>
  pivot_longer(cols = all_of(reason_cols), names_to = "Cause", values_to = "Minutes")

ggplot(delay_long, aes(x = factor(YEAR), y = Minutes, fill = Cause)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Delay Minutes by Cause and Year",
    x = "Year", y = "Total Delay (minutes)"
  ) +
  theme_minimal()

# multi-line
ggplot(delay_long, aes(x = YEAR, y = Minutes, color = Cause)) +
  geom_line(size=1) +
  geom_point(size=2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Yearly Delay Trend by Cause",
       x = "Year", y = "Total Delay (minutes)") +
  theme_minimal()

#------------------------------------------
# Delay do “Late Aircraft” có mối liên hệ mạnh với ARR_DELAY tổng không?

# Kiểm tra tương quan
# Python không fill NA, R có fill NA → kết quả lệch.
late_aircraft_df <- flights_delayed |> 
  select(ARR_DELAY, DELAY_DUE_LATE_AIRCRAFT) |> 
  drop_na()

late_aircraft_corr <- cor(late_aircraft_df$ARR_DELAY, 
                      late_aircraft_df$DELAY_DUE_LATE_AIRCRAFT)

corr_review <- paste(
  "Nhận xét:",
  "- Late Aircraft Delay tăng → Arrival Delay cũng tăng",
  "- corr ~ 0.504 -> trung bình - khá",
  "- R-squared ≈ corr^2 = 0.504^2 ≈ 0.254",
  "- => Khoảng 25% biến động Arrival Delay đến từ Late Aircraft Delay.\n",
  sep = "\n"
)
cat(corr_review)

ggplot(late_aircraft_df, aes(DELAY_DUE_LATE_AIRCRAFT, ARR_DELAY)) +
  geom_hex() +
  geom_smooth(method = "lm", color="red") +
  scale_y_continuous(labels = comma) +
  labs(title="Relationship: Late Aircraft Delay vs Arrival Delay",
       x="Late Aircraft Delay", y="Arrival Delay")

#------------------------------------------
# Tỉ lệ delay do thời tiết khác nhau giữa các tháng như thế nào?

# tổng số phút delay do weather theo year + month
weather_delay_monthly <- flights_delayed |>
  group_by(YEAR, MONTH) |>
  summarise(Weather_Delay = sum(DELAY_DUE_WEATHER))

# Tính tổng ARR_DELAY theo YEAR + MONTH
arr_delay_monthly <- flights_delayed |>
  group_by(YEAR, MONTH) |>
  summarise(Arr_Delay = sum(ARR_DELAY))

weather_rate_monthly <- weather_delay_monthly |>
  inner_join(arr_delay_monthly, by = c("YEAR", "MONTH")) |>
  mutate(Weather_Delay_Rate = round(Weather_Delay / Arr_Delay * 100, 1))
  
# plot
ggplot(weather_rate_monthly, aes(x = factor(YEAR), 
                                 y = factor(MONTH, levels = rev(1:12)), 
                                 fill = Weather_Delay_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Weather_Delay_Rate, 1)), 
            color = "black", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Weather_Delay_Rate (minutes)",
    x = "Năm", y = "Tháng") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#------------------------------------------
# Kiểm tra tương quan
# Python không fill NA, R có fill NA → kết quả lệch.
due_weather_df <- flights_delayed |> 
  select(ARR_DELAY, DELAY_DUE_WEATHER) |> 
  drop_na()

due_weather_corr <- cor(due_weather_df)

corr_review1 <- paste(
  "Nhận xét:",
  "- DELAY_DUE_WEATHER tăng → Arrival Delay cũng tăng",
  "- corr ~ 0.2872 -> thấp",
  "- R-squared ≈ corr^2 ≈ 0.08248384 * 100 = 8.2%",
  "- => Khoảng 8.2% biến động Arrival Delay đến từ Late Aircraft Delay.\n",
  sep = "\n"
)
cat(corr_review1)

ggplot(due_weather_df, aes(DELAY_DUE_WEATHER, ARR_DELAY)) +
  geom_hex() +
  geom_smooth(method = "lm", color="red") +
  scale_y_continuous(labels = comma) +
  labs(title="Relationship: Weather Delay vs Arrival Delay",
       x="Weather Delay", y="Arrival Delay")

#------------------------------------------
# Hãng nào thường bị ảnh hưởng nhiều bởi thời tiết nhất?
weather_delay_flights <- flights_delayed |>
  group_by(YEAR, AIRLINE) |>
  summarise(Flights_Delayed_Weather = sum(DELAY_DUE_WEATHER > 0, na.rm = TRUE))

weather_delay_flights_top <- weather_delay_flights |>
  group_by(YEAR) |>
  slice_max(Flights_Delayed_Weather, n = 1, 
            with_ties = FALSE) |>
  ungroup()

weather_delay_flights_least <- weather_delay_flights |>
  group_by(YEAR) |>
  slice_min(Flights_Delayed_Weather, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Có mối tương quan giữa DISTANCE và AIR_TIME không?
distance_air_df <- df_flights |>
  select("DISTANCE", "AIR_TIME") |>
  drop_na()

distance_air_corr <- cor(distance_air_df)

corr_review2 <- paste(
  "Nhận xét:",
  "- DISTANCE tăng → AIR_TIME cũng tăng",
  "- corr ~ 0.983 -> cao -> khoảng cách càng xa, thời gian bay càng nhiều",
  "- R-squared ≈ correlation^2 = 0.966298 * 100 = 96%",
  "- => Khoảng 96% biến động AIR_TIME đến từ DISTANCE",
  sep = "\n"
)
cat(corr_review2)

ggplot(distance_air_df, aes(DISTANCE, AIR_TIME)) +
  geom_hex() +
  geom_smooth(method = "lm", color="red") +
  scale_y_continuous(labels = comma) +
  labs(title="Relationship: DISTANCE vs AIR_TIME",
       x="DISTANCE (miles)", y="AIR_TIME (minutes)")

#------------------------------------------
# Delay do “Carrier” có xu hướng xảy ra ở sân bay nào nhiều nhất?
carrier_delay_flights <- flights_delayed |>
  group_by(YEAR, ORIGIN, ORIGIN_CITY) |>
  summarise(Flights_Delayed_Carrier = sum(DELAY_DUE_CARRIER > 0, na.rm = TRUE))

carrier_delay_flights_top <- carrier_delay_flights |>
  group_by(YEAR) |>
  slice_max(Flights_Delayed_Carrier, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# TAXI_OUT dài có liên quan đến ARR_DELAY cao hơn không?
taxiout_arrdelay_df <- df_flights |>
  select("TAXI_OUT", "ARR_DELAY") |>
  drop_na()

taxiout_arrdelay_corr <- cor(taxiout_arrdelay_df)

corr_review3 <- paste(
  "Nhận xét:",
  "- TAXI_OUT tăng → ARR_DELAY cũng tăng",
  "- corr ~ 0.195 -> thấp ",
  "- R-squared ≈ correlation^2 = 0.38025 * 100 = 3.8%",
  "- => Khoảng 3.8% biến động ARR_DELAY đến từ TAXI_OUT",
  sep = "\n"
)
cat(corr_review3)

#------------------------------------------
# Thời gian bay (AIR_TIME) dài hơn có làm tăng khả năng delay (ARR_DELAY) không?
airtime_arrdelay_df <- df_flights |>
  select("AIR_TIME", "ARR_DELAY") |>
  drop_na()

airtime_arrdelay_corr <- cor(airtime_arrdelay_df)

corr_review4 <- paste(
  "Nhận xét:",
  "- AIR_TIME tăng → ARR_DELAY cũng tăng",
  "- corr ~ 0.1723 -> thấp ",
  "- R-squared ≈ correlation^2 = 0.002967 * 100 = 0.297%",
  "- => Khoảng 0.297% biến động ARR_DELAY đến từ AIR_TIME",
  sep = "\n"
)
cat(corr_review4)

#------------------------------------------
# Các chuyến đêm muộn hoặc sáng sớm có ít delay hơn không?
flights_delayed <- flights_delayed |>
  mutate(
    HOUR = floor(CRS_DEP_TIME / 100),
    TIME_BIN = case_when(
      HOUR >= 22 | HOUR < 5 ~ "Late Night",
      HOUR >= 5  & HOUR < 8 ~ "Early Morning",
      HOUR >= 8  & HOUR < 17 ~ "Daytime",
      TRUE ~ "Evening"
    )
  )

timebin_delay_flights <- flights_delayed |>
  group_by(YEAR, TIME_BIN) |>
  summarise(Delay_Rate = round(mean(ARR_DELAY), 1))

# plot
ggplot(timebin_delay_flights, aes(x = factor(YEAR), 
                                 y = TIME_BIN, 
                                 fill = Delay_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Delay_Rate, 1)), 
            color = "black", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Delay Rate by Time of Day",
    x = "Năm", y = "TIME_BIN") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#------------------------------------------
# 2.2 Hủy chuyến và chuyển hướng
#------------------------------------------
# Nguyên nhân hủy chuyến (A–D) nào phổ biến nhất?
cancel_code <- flights_cancelled |>
  group_by(CANCELLATION_CODE) |>
  summarise(Total_Flights = n())

cancell_code_yearly <- flights_cancelled |>
  group_by(YEAR, CANCELLATION_CODE) |>
  summarise(Total_Flights = n())

#------------------------------------------
# Tháng nào có nhiều chuyến bị hủy nhất?
flights_cancel_monthly <- flights_cancelled |>
  group_by(MONTH) |>
  summarise(Total_Cancelled_Flights = n())

#------------------------------------------
# Hãng nào có tỉ lệ diverted cao nhất?
divert_rate_airline_yearly <- flights_diverted |>
  group_by(YEAR, AIRLINE) |>
  summarise(Total_Diverted_Flights = n())

divert_rate_airline_top <- divert_rate_airline_yearly |>
  group_by(YEAR) |>
  slice_max(Total_Diverted_Flights, n = 1, 
            with_ties = FALSE) |>
  ungroup()

#------------------------------------------
# Có mối quan hệ giữa weather delay và cancellation code = B không?
# Chỉ filter ra B → không kiểm tra được mối quan hệ.
# Để kiểm tra quan hệ, bạn phải giữ cả 2 nhóm: có delay và không, bị hủy B và không.
# Dùng: prop.table() + chisq.test() là rõ nhất.

# Cách 1: Kiểm tra quan hệ: so sánh nhóm B và nhóm không B
# B1
df$CANCELLATION_CODE <- as.character(df$CANCELLATION_CODE)

df$is_cancelled_B <- df$CANCELLATION_CODE == "B"
df$has_weather_delay <- df$DELAY_DUE_WEATHER > 0

# B2 -> bảng tần suất
table(df$has_weather_delay, df$is_cancelled_B)

# B3 -> bảng so sánh tỉ lệ theo nhóm
prop.table(table(df$has_weather_delay, df$is_cancelled_B))

#------------------------------------------
# Cách 2: Chi-square test trong R
tbl <- table(df$has_weather_delay, df$is_cancelled_B)
results <- chisq.test(tbl)
print(results)

#------------------------------------------
# Cách 3: Logistic regression (nếu muốn phân tích sâu hơn)
model <- glm(is_cancelled_B ~ DELAY_DUE_WEATHER, 
             data = df, family = binomial)
summary(model)

# df$is_cancelled_B -> 19022 / 2 triệu
# data chiếm phần rất nhỏ của dữ liệu, ko đáng kể, có thể bỏ qua ko cần phân tích

############################################
# 3. Phân tích dự đoán (Predictive / Machine Learning)
############################################
# 3.1 Phân loại (Classification)







