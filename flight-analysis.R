if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

df = read.csv("data/flights_sample_2m.csv", header=TRUE)

head(df)