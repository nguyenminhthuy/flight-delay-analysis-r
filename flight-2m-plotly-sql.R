library(plotly)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)

df = fread("data/flights_sample_2m.csv", header=TRUE)

#---------------------------------------
df <- df |>
  mutate(
    FL_DATE = as.Date(FL_DATE),
    YEAR = year(FL_DATE),
    MONTH = month(FL_DATE)
  )

############################################
# 1. PLOTLY
############################################
# Số chuyến bay hàng năm

df_flights <- copy(df)
flights_yearly <- df_flights |>
  group_by(YEAR) |>
  summarise(Total_Flights = n(), .groups = "drop")


fig_flights_yearly <- plot_ly(data = flights_yearly,
                             x= ~YEAR, y= ~Total_Flights,
                             type='bar',
                             marker = list(color = 'rgb(58,200,225)',
                                           line = list(color = 'rgb(8,48,107)', 
                                                       width = 1.5))) |>
                      layout(title="Total flights per year (2019-2023)",
                             xaxis=list(title="Year"),
                             yaxis=list(title="Number of flights")) |>
                      config(responsive = TRUE)

fig_flights_yearly

#------------------------------------------
# Số chuyến bay theo tháng của từng năm
flights_monthly <- df_flights |>
  group_by(YEAR, MONTH) |>
  summarise(Total_Flights = n(), .groups = "drop")

fig_flights_monthly <- plot_ly(flights_monthly, 
                              x = ~MONTH, y = ~Total_Flights,
                              color = ~factor(YEAR), type = "scatter",
                              mode = "lines+markers") |>
                       layout(title = "Monthly Flights Trend (2019-2023)",
                              xaxis = list(title = "Month", dtick = 1),
                              yaxis = list(title = "Number of Flights", 
                                           separatethousands = TRUE),
                              legend = list(title = list(text="Year"))) |>
                       config(responsive = TRUE)
fig_flights_monthly


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

plot_ly(flights_airline_yearly,
        x = ~YEAR, y = ~Total_Flights, 
        color = ~AIRLINE, 
        type = 'scatter', 
        mode = 'none',
        stackgroup = 'one') %>%
  layout(title="Airline Market Share Over Years",
         yaxis=list(title="Flights"))

############################################
# 2. SQL
############################################
library(DBI)
library(RSQLite)

# Tạo database SQLite mới (file .db)
con <- dbConnect(RSQLite::SQLite(), "flights.db")

# Ghi vào table 'flights'
dbWriteTable(con, "flights", df, overwrite = TRUE)

# Ví dụ: delay trung bình theo tháng
query <- "
SELECT MONTH, ROUND(AVG(ARR_DELAY),1) AS avg_delay
FROM flights
GROUP BY MONTH
ORDER BY MONTH
"
result <- dbGetQuery(con, query)
head(result)

# Top 3 hãng delay nặng nhất mỗi tháng và năm
query1 <- "
SELECT YEAR, MONTH, AIRLINE, delay_rate
FROM (
    SELECT YEAR, MONTH, AIRLINE,
           ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate,
           RANK() OVER (PARTITION BY YEAR, MONTH ORDER BY AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END) DESC) AS rnk
    FROM flights
    GROUP BY YEAR, MONTH, AIRLINE
)
WHERE rnk <= 3
ORDER BY YEAR, MONTH, rnk;
"
result1 <- dbGetQuery(con, query1)
head(result1)

# Trend delay cumulative theo tháng cho từng năm
query2 <- "
SELECT YEAR, MONTH,
       ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate,
       SUM(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)) OVER(PARTITION BY YEAR ORDER BY MONTH) * 100 AS cumulative_delay_rate
FROM flights
GROUP BY YEAR, MONTH
ORDER BY YEAR, MONTH;
"
result2 <- dbGetQuery(con, query2)
head(result2)

# Chặng bay có delay rate tăng mạnh nhất năm 2023 so với 2022
query3 <- "
SELECT t1.ORIGIN, t1.DEST, 
       t1.delay_rate AS delay_2023, t2.delay_rate AS delay_2022,
       t1.delay_rate - t2.delay_rate AS diff
FROM 
    (SELECT ORIGIN, DEST, ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate
     FROM flights
     WHERE YEAR = 2023
     GROUP BY ORIGIN, DEST) t1
JOIN 
    (SELECT ORIGIN, DEST, ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate
     FROM flights
     WHERE YEAR = 2022
     GROUP BY ORIGIN, DEST) t2
ON t1.ORIGIN = t2.ORIGIN AND t1.DEST = t2.DEST
ORDER BY diff DESC
LIMIT 10;
"
result3 <- dbGetQuery(con, query3)
head(result3)

# Top 5 tháng delay cao nhất cho mỗi hãng
query4 <- "
SELECT AIRLINE, YEAR, MONTH, delay_rate
FROM (
    SELECT AIRLINE, YEAR, MONTH,
           ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate,
           RANK() OVER (PARTITION BY AIRLINE ORDER BY AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END) DESC) AS rnk
    FROM flights
    GROUP BY AIRLINE, YEAR, MONTH
)
WHERE rnk <= 5
ORDER BY AIRLINE, rnk;
"
result4 <- dbGetQuery(con, query4)
head(result4)

# Phân loại chuyến bay và tỉ lệ lũy kế theo hãng
query5 <- "
SELECT AIRLINE, delay_category, COUNT(*) AS count_flights,
       ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER(PARTITION BY AIRLINE), 1) AS pct_by_airline
FROM (
    SELECT AIRLINE,
           CASE 
               WHEN ARR_DELAY <= 0 THEN 'On Time'
               WHEN ARR_DELAY <= 15 THEN 'Minor'
               WHEN ARR_DELAY <= 60 THEN 'Moderate'
               ELSE 'Severe'
           END AS delay_category
    FROM flights
) t
GROUP BY AIRLINE, delay_category
ORDER BY AIRLINE, pct_by_airline DESC;
"
result5 <- dbGetQuery(con, query5)
head(result5)

# Trend delay giữa năm, highlight chênh lệch so với năm trước
query6 <- "
SELECT YEAR, MONTH, delay_rate,
       LAG(delay_rate) OVER(PARTITION BY MONTH ORDER BY YEAR) AS prev_year,
       delay_rate - LAG(delay_rate) OVER(PARTITION BY MONTH ORDER BY YEAR) AS diff_vs_prev
FROM (
    SELECT YEAR, MONTH,
           ROUND(AVG(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)*100,1) AS delay_rate
    FROM flights
    GROUP BY YEAR, MONTH
)
ORDER BY MONTH, YEAR;
"
result6 <- dbGetQuery(con, query6)
head(result6)

dbDisconnect(con)

