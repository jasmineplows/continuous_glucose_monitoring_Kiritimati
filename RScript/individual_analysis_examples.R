#Packages
library(tidyverse)
library(lme4)
library(ggpubr)
library(ggplot2)
library(pander)
library(lubridate)
library(ggrepel)

## Prevents pander from wrapping the table @ 80 chars.
panderOptions('table.split.table', Inf)

##### Load in data ########
dataSet <- read.csv("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Data/102_post.csv",header=TRUE)

dataSet$time <- as.character(dataSet$time)

dataSet$time[1]

dataSet$time <- as.POSIXct(dataSet$time,format="%m/%d/%Y %H:%M")

theme_set(theme_classic())

y_limits <- c(50,400)

# 102 Post Plot
ggplot(dataSet, aes(x = time, y = glucose_mg_dl)) +
  geom_line(aes(y = glucose_mg_dl)) +
  theme(axis.text = element_text(color = "black")) +
  labs(
    title = "102 Post-Intervention",
    caption = "Grey area represents target blood glucose range",
    y = "Blood Glucose (mg/dL)",
    x = "Date/Time"
  ) +
  ylim(0, 300) +
  scale_x_datetime(expand = c(0,0)) +
  geom_text_repel(
    aes(label = food),
    segment.color = "red",
    direction = "x",
    force = 20,
    vjust = 0,
    nudge_y = 100,
    nudge_x = 50,
    size = 3.5) +
  annotate("rect", ymin = 70, ymax = 150, xmin = min(dataSet$time),xmax = max(dataSet$time), fill = "grey", alpha = 0.5)

##### Load in data ########
dataSet <- read.csv("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Data/108_post.csv",header=TRUE)

dataSet$time <- as.character(dataSet$time)

dataSet$time[1]

dataSet$time <- as.POSIXct(dataSet$time,format="%m/%d/%Y %H:%M")

theme_set(theme_classic())

y_limits <- c(50,400)

# 108 Post Plot
ggplot(dataSet, aes(x = time, y = glucose_mg_dl)) +
  geom_line(aes(y = glucose_mg_dl)) +
  theme(axis.text = element_text(color = "black")) +
  labs(
    title = "108 Post-Intervention",
    caption = "Grey area represents target blood glucose range",
    y = "Blood Glucose (mg/dL)",
    x = "Date/Time"
  ) +
  ylim(0, 300) +
  scale_x_datetime(expand = c(0,0)) +
  geom_text_repel(
    aes(label = food),
    segment.color = "red",
    direction = "x",
    force = 20,
    vjust = 0,
    nudge_y = 100,
    nudge_x = 50,
    size = 3.5) +
  annotate("rect", ymin = 70, ymax = 150, xmin = min(dataSet$time),xmax = max(dataSet$time), fill = "grey", alpha = 0.5)

##### Load in data ########
dataSet <- read.csv("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Data/109_post.csv",header=TRUE)

dataSet$time <- as.character(dataSet$time)

dataSet$time[1]

dataSet$time <- as.POSIXct(dataSet$time,format="%m/%d/%Y %H:%M")

theme_set(theme_classic())

y_limits <- c(50,400)

# 109 Post Plot
ggplot(dataSet, aes(x = time, y = glucose_mg_dl)) +
  geom_line(aes(y = glucose_mg_dl)) +
  theme(axis.text = element_text(color = "black")) +
  labs(
    title = "109 Post-Intervention",
    caption = "Grey area represents target blood glucose range",
    y = "Blood Glucose (mg/dL)",
    x = "Date/Time"
  ) +
  ylim(0, 300) +
  scale_x_datetime(expand = c(0,0)) +
  geom_text_repel(
    aes(label = food),
    segment.color = "red",
    direction = "x",
    force = 20,
    vjust = 0,
    nudge_y = 100,
    nudge_x = 50,
    size = 3.5) +
  annotate("rect", ymin = 70, ymax = 150, xmin = min(dataSet$time),xmax = max(dataSet$time), fill = "grey", alpha = 0.5)

##### Load in data ########
dataSet <- read.csv("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Data/130_post.csv",header=TRUE)

dataSet$time <- as.character(dataSet$time)

dataSet$time[1]

dataSet$time <- as.POSIXct(dataSet$time,format="%m/%d/%Y %H:%M")

theme_set(theme_classic())

y_limits <- c(50,400)

# 130 Post Plot
ggplot(dataSet, aes(x = time, y = glucose_mg_dl)) +
  geom_line(aes(y = glucose_mg_dl)) +
  theme(axis.text = element_text(color = "black")) +
  labs(
    title = "130 Post-Intervention",
    caption = "Grey area represents target blood glucose range",
    y = "Blood Glucose (mg/dL)",
    x = "Date/Time"
  ) +
  ylim(0, 300) +
  scale_x_datetime(expand = c(0,0)) +
  geom_text_repel(
    aes(label = food),
    segment.color = "red",
    direction = "x",
    force = 20,
    vjust = 0,
    nudge_y = 100,
    nudge_x = 50,
    size = 3.5) +
  annotate("rect", ymin = 70, ymax = 150, xmin = min(dataSet$time),xmax = max(dataSet$time), fill = "grey", alpha = 0.5)