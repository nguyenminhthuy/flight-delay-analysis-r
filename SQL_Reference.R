library(plotly)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(DBI)
library(RSQLite)

df = fread("data/flights_sample_2m.csv", header=TRUE)

############################################
# PREPARATION
############################################
dow_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                "Friday", "Saturday", "Sunday")

df <- df |>
  mutate(
    FL_DATE = ymd(FL_DATE),
    YEAR = year(FL_DATE),
    MONTH = month(FL_DATE),
    QUARTER = quarter(FL_DATE),
    
    # wday(): Sunday = 1 → Monday = 2 → ... → Saturday = 7
    DAY_NUM = wday(FL_DATE), 
    # Chuyển về Monday = 1, ... Sunday = 7
    DAY_NUM = ifelse(DAY_NUM == 1, 7, DAY_NUM - 1),
    DAY_OF_WEEK = factor(dow_levels[DAY_NUM], 
                         levels = dow_levels, 
                         ordered = TRUE),
    
    DISTANCE_CAT = cut(
      DISTANCE,
      breaks = c(-Inf, 500, 1500, Inf),
      labels = c("Short-haul", "Medium-haul", "Long-haul")
    ),
    
    DEP_HOUR = factor(floor(CRS_DEP_TIME / 100), levels = 0:23),
    
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )

############################################
# SQL
############################################
# Tạo database SQLite mới (file .db)
conn <- dbConnect(RSQLite::SQLite(), "flights.db")

# Ghi vào table 'flights'
dbWriteTable(conn, "flights", df, overwrite = TRUE)

query_1 <- "
SELECT R1.TOTAL_FLIGHTS, R2.N_DELAYED_FLIGHTS, R3.N_CANCELLED_FLIGHTS, 
       R4.N_DIVERTED_FLIGHTS, R5.N_ONTIME_FLIGHTS
FROM (SELECT COUNT(*) AS TOTAL_FLIGHTS FROM flights) R1
JOIN (SELECT COUNT(*) AS N_DELAYED_FLIGHTS 
      FROM flights WHERE ARR_DELAY > 15) R2
JOIN (SELECT COUNT(*) AS N_CANCELLED_FLIGHTS 
      FROM flights WHERE CANCELLED = 1) R3
JOIN (SELECT COUNT(*) AS N_DIVERTED_FLIGHTS 
      FROM flights WHERE DIVERTED = 1) R4
JOIN (SELECT COUNT(*) AS N_ONTIME_FLIGHTS 
      FROM flights WHERE ARR_DELAY <= 15) R5
"
result_1 <- dbGetQuery(conn, query_1)
result_1

#-------------------------------------------
query_2 <- "
SELECT DISTINCT AIRLINE
FROM flights
ORDER BY AIRLINE;
"
result_2 <- dbGetQuery(conn, query_2)
result_2

#-------------------------------------------
query_3 = "
SELECT YEAR, COUNT(*) AS TOTAL_FLIGHTS
FROM flights
GROUP BY YEAR;
"
result_3 = dbGetQuery(conn, query_3)
result_3

#-------------------------------------------
query_4 = "
SELECT YEAR, MONTH, COUNT(*) AS TOTAL_FLIGHTS
FROM flights
GROUP BY YEAR, MONTH;
"
result_4 = dbGetQuery(conn, query_4)
result_4

#-------------------------------------------
query_5 <- "
SELECT
    YEAR,
    COUNT(*) AS Total_Flights,

    -- Delay rate (ARR_DELAY > 15)
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Delay_Rate,

    -- On-time rate (ARR_DELAY <= 15)
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY <= 15 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS OnTime_Rate,

    -- Cancel rate
    ROUND(100.0 * SUM(CASE WHEN CANCELLED = 1 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Cancel_Rate,

    -- Divert rate
    ROUND(100.0 * SUM(CASE WHEN DIVERTED = 1 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Divert_Rate

FROM flights
WHERE AIRLINE = 'Envoy Air'
GROUP BY YEAR
ORDER BY YEAR
"
result_5 <- dbGetQuery(conn, query_5)
result_5

#-------------------------------------------
query_6 <- "
SELECT
    YEAR,
    MONTH,
    COUNT(*) AS Total_Flights,

    -- Delay rate
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Delay_Rate,

    -- On-time rate
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY <= 15 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS OnTime_Rate,

    -- Cancel rate
    ROUND(100.0 * SUM(CASE WHEN CANCELLED = 1 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Cancel_Rate,

    -- Divert rate
    ROUND(100.0 * SUM(CASE WHEN DIVERTED = 1 THEN 1 ELSE 0 END) 
          / COUNT(*), 2) AS Divert_Rate

FROM flights
WHERE AIRLINE = 'Envoy Air'
GROUP BY YEAR, MONTH
ORDER BY YEAR, MONTH
"

result_6 <- dbGetQuery(conn, query_6)
result_6

#-------------------------------------------
query_7 <- "
SELECT
    YEAR,
    QUARTER,
    COUNT(*) AS Total_Flights,

    -- Delay rate
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY > 15 THEN 1 ELSE 0 END)
          / COUNT(*), 2) AS Delay_Rate,

    -- On-time rate
    ROUND(100.0 * SUM(CASE WHEN ARR_DELAY <= 15 THEN 1 ELSE 0 END)
          / COUNT(*), 2) AS OnTime_Rate,

    -- Cancel rate
    ROUND(100.0 * SUM(CASE WHEN CANCELLED = 1 THEN 1 ELSE 0 END)
          / COUNT(*), 2) AS Cancel_Rate,

    -- Divert rate
    ROUND(100.0 * SUM(CASE WHEN DIVERTED = 1 THEN 1 ELSE 0 END)
          / COUNT(*), 2) AS Divert_Rate

FROM flights
WHERE AIRLINE = 'Envoy Air'
GROUP BY YEAR, QUARTER
ORDER BY YEAR, QUARTER
"
result_7 <- dbGetQuery(conn, query_7)
result_7

dbDisconnect(conn)

