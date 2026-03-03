# ============================================================
# BSAN 615 Week 8 — Fleet Data Analysis
# ============================================================

# === Setup ===
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)

# Read data
assets      <- read_csv("assets.csv")
fuel        <- read_csv("fuel.csv")
maintenance <- read_csv("maintenance.csv")

# Parse date columns
assets$IN_SERVICE_DATE     <- as.Date(assets$IN_SERVICE_DATE)
fuel$TRANS_DATE            <- as.Date(fuel$TRANS_DATE)
maintenance$REPAIR_ORDER_DATE <- as.Date(maintenance$REPAIR_ORDER_DATE)

# Create MAKE_MODEL column
assets <- assets %>%
  mutate(MAKE_MODEL = paste(MAKE, MODEL))

# Reference date for age calculations (dataset is from 2018)
ref_date <- as.Date("2018-12-31")

# ============================================================
# === Q1: Top 10 Makes/Models by Vehicle Count ===
# ============================================================

q1 <- assets %>%
  count(MAKE_MODEL, name = "COUNT") %>%
  arrange(desc(COUNT)) %>%
  slice_head(n = 10)

print(q1)

p1 <- ggplot(q1, aes(x = reorder(MAKE_MODEL, COUNT), y = COUNT)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = COUNT), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Top 10 Makes/Models by Vehicle Count",
    x     = "Make / Model",
    y     = "Number of Vehicles"
  ) +
  theme_minimal()

print(p1)

# ============================================================
# === Q2: Assets by Age and Odometer ===
# ============================================================

assets <- assets %>%
  mutate(
    MONTHS_IN_SERVICE = as.numeric(
      interval(IN_SERVICE_DATE, ref_date) / months(1)
    ),
    AGE_BIN = cut(
      MONTHS_IN_SERVICE,
      breaks = seq(0, ceiling(max(MONTHS_IN_SERVICE, na.rm = TRUE) / 12) * 12, by = 12),
      include.lowest = TRUE,
      right = FALSE
    ),
    ODOM_BIN = cut(
      CURRENT_ODOM,
      breaks = seq(0, ceiling(max(CURRENT_ODOM, na.rm = TRUE) / 5000) * 5000, by = 5000),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 2a: Count of assets by months-in-service bins
q2a <- assets %>%
  filter(!is.na(AGE_BIN)) %>%
  count(AGE_BIN, name = "COUNT")

print(q2a)

p2a <- ggplot(q2a, aes(x = AGE_BIN, y = COUNT)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Assets by Months in Service",
    x     = "Months in Service (12-Month Bins)",
    y     = "Number of Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2a)

# 2b: Count of assets by odometer bins (5,000-mile intervals)
q2b <- assets %>%
  filter(!is.na(ODOM_BIN)) %>%
  count(ODOM_BIN, name = "COUNT")

print(q2b)

p2b <- ggplot(q2b, aes(x = ODOM_BIN, y = COUNT)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Assets by Current Odometer Reading",
    x     = "Odometer (5,000-Mile Bins)",
    y     = "Number of Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2b)

# ============================================================
# === Q3: Typical Annual Fuel Spend ===
# ============================================================

q3 <- fuel %>%
  inner_join(assets %>% select(VEHICLE_ID), by = "VEHICLE_ID") %>%
  group_by(VEHICLE_ID) %>%
  summarise(ANNUAL_SPEND = sum(AMOUNT, na.rm = TRUE), .groups = "drop")

cat("\n--- Q3: Annual Fuel Spend Summary ---\n")
print(summary(q3$ANNUAL_SPEND))
cat(sprintf("Mean:   $%.2f\n", mean(q3$ANNUAL_SPEND, na.rm = TRUE)))
cat(sprintf("Median: $%.2f\n", median(q3$ANNUAL_SPEND, na.rm = TRUE)))

p3 <- ggplot(q3, aes(x = ANNUAL_SPEND)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Distribution of Annual Fuel Spend per Vehicle",
    x     = "Annual Fuel Spend ($)",
    y     = "Number of Vehicles"
  ) +
  theme_minimal()

print(p3)

# ============================================================
# === Q4: Top 10 Makes/Models by Highest CPG ===
# ============================================================

# All fuel product names (gasoline blends + diesel), excluding non-fuel line items
all_fuel_products <- c(
  "UNLEADED", "UNL PLUS", "SUPER UN", "UNLALC10", "UNLALC57",
  "UNLALC77", "ETHANL85", "DIESEL", "PREM DSL",
  "UN+ALC10", "UN+ALC57", "UN+ALC77",
  "SUPALC10", "SUPALC57", "SUPALC77"
)

q4 <- fuel %>%
  filter(PRODUCT_NAME %in% all_fuel_products, UNITS > 0) %>%
  mutate(CPG = AMOUNT / UNITS) %>%
  inner_join(assets %>% select(VEHICLE_ID, MAKE_MODEL), by = "VEHICLE_ID") %>%
  group_by(MAKE_MODEL) %>%
  summarise(AVG_CPG = mean(CPG, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(AVG_CPG)) %>%
  slice_head(n = 10)

print(q4)

p4 <- ggplot(q4, aes(x = reorder(MAKE_MODEL, AVG_CPG), y = AVG_CPG)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = dollar(round(AVG_CPG, 2))), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Top 10 Makes/Models by Highest Average Cost Per Gallon (CPG)",
    x     = "Make / Model",
    y     = "Average CPG ($)"
  ) +
  theme_minimal()

print(p4)

# ============================================================
# === Q5: Average CPG by State for Gasoline and Diesel ===
# ============================================================

gasoline_products <- c(
  "UNLEADED", "UNL PLUS", "SUPER UN", "UNLALC10", "UNLALC57", "ETHANL85"
)
diesel_products <- c("DIESEL", "PREM DSL")

q5 <- fuel %>%
  filter(PRODUCT_NAME %in% c(gasoline_products, diesel_products), UNITS > 0) %>%
  mutate(
    CPG          = AMOUNT / UNITS,
    FUEL_CATEGORY = case_when(
      PRODUCT_NAME %in% gasoline_products ~ "Gasoline",
      PRODUCT_NAME %in% diesel_products   ~ "Diesel"
    )
  ) %>%
  group_by(SITE_STATE, FUEL_CATEGORY) %>%
  summarise(AVG_CPG = mean(CPG, na.rm = TRUE), .groups = "drop")

print(q5)

# Gasoline chart
q5_gas <- q5 %>% filter(FUEL_CATEGORY == "Gasoline") %>% arrange(AVG_CPG)

p5a <- ggplot(q5_gas, aes(x = reorder(SITE_STATE, AVG_CPG), y = AVG_CPG)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Gasoline CPG by State",
    x     = "State",
    y     = "Average Cost Per Gallon ($)"
  ) +
  theme_minimal()

print(p5a)

# Diesel chart
q5_dsl <- q5 %>% filter(FUEL_CATEGORY == "Diesel") %>% arrange(AVG_CPG)

p5b <- ggplot(q5_dsl, aes(x = reorder(SITE_STATE, AVG_CPG), y = AVG_CPG)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Average Diesel CPG by State",
    x     = "State",
    y     = "Average Cost Per Gallon ($)"
  ) +
  theme_minimal()

print(p5b)

# ============================================================
# === Q6: Top 5 Makes/Models by Highest Cost Per Month ===
# ============================================================

q6 <- maintenance %>%
  filter(!is.na(COST)) %>%
  inner_join(assets %>% select(VEHICLE_ID, MAKE_MODEL, IN_SERVICE_DATE), by = "VEHICLE_ID") %>%
  mutate(
    MONTHS_IN_SERVICE = as.numeric(interval(IN_SERVICE_DATE, ref_date) / months(1))
  ) %>%
  filter(MONTHS_IN_SERVICE > 0) %>%
  group_by(VEHICLE_ID, MAKE_MODEL, MONTHS_IN_SERVICE) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop") %>%
  mutate(COST_PER_MONTH = TOTAL_COST / MONTHS_IN_SERVICE) %>%
  group_by(MAKE_MODEL) %>%
  summarise(AVG_COST_PER_MONTH = mean(COST_PER_MONTH, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(AVG_COST_PER_MONTH)) %>%
  slice_head(n = 5)

print(q6)

p6 <- ggplot(q6, aes(x = reorder(MAKE_MODEL, AVG_COST_PER_MONTH), y = AVG_COST_PER_MONTH)) +
  geom_col(fill = "mediumpurple") +
  geom_text(aes(label = dollar(round(AVG_COST_PER_MONTH, 2))), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Top 5 Makes/Models by Average Maintenance Cost per Month",
    x     = "Make / Model",
    y     = "Average Cost per Month ($)"
  ) +
  theme_minimal()

print(p6)

# ============================================================
# === Q7: Repair Orders, Items, and Cost by 12-Month Bins ===
# ============================================================

q7_data <- maintenance %>%
  filter(!is.na(COST)) %>%
  inner_join(assets %>% select(VEHICLE_ID, IN_SERVICE_DATE), by = "VEHICLE_ID") %>%
  mutate(
    MONTHS_IN_SERVICE = as.numeric(interval(IN_SERVICE_DATE, ref_date) / months(1)),
    AGE_BIN = cut(
      MONTHS_IN_SERVICE,
      breaks = seq(0, ceiling(max(MONTHS_IN_SERVICE, na.rm = TRUE) / 12) * 12, by = 12),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  filter(!is.na(AGE_BIN))

q7 <- q7_data %>%
  group_by(AGE_BIN) %>%
  summarise(
    REPAIR_ORDERS   = n_distinct(REPAIR_ORDER_NO),
    LINE_ITEMS      = n(),
    TOTAL_COST      = sum(COST, na.rm = TRUE),
    AVG_COST_PER_RO = TOTAL_COST / REPAIR_ORDERS,
    .groups = "drop"
  )

cat("\n--- Q7: Repair Summary by Age Bin ---\n")
print(q7)

p7a <- ggplot(q7, aes(x = AGE_BIN, y = REPAIR_ORDERS)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Unique Repair Orders by Months-in-Service Bin",
    x     = "Months in Service (12-Month Bins)",
    y     = "Number of Repair Orders"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7a)

p7b <- ggplot(q7, aes(x = AGE_BIN, y = LINE_ITEMS)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Repair Line Items by Months-in-Service Bin",
    x     = "Months in Service (12-Month Bins)",
    y     = "Number of Line Items"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7b)

p7c <- ggplot(q7, aes(x = AGE_BIN, y = TOTAL_COST)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Total Maintenance Cost by Months-in-Service Bin",
    x     = "Months in Service (12-Month Bins)",
    y     = "Total Cost ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7c)

p7d <- ggplot(q7, aes(x = AGE_BIN, y = AVG_COST_PER_RO)) +
  geom_col(fill = "mediumpurple") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Average Cost per Repair Order by Months-in-Service Bin",
    x     = "Months in Service (12-Month Bins)",
    y     = "Avg Cost per Repair Order ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7d)

# ============================================================
# === Q8: At What Months and Miles Do Expenses Increase? ===
# ============================================================

# Identify top 5 makes/models by total maintenance cost
top5_makes <- maintenance %>%
  filter(!is.na(COST)) %>%
  inner_join(assets %>% select(VEHICLE_ID, MAKE_MODEL), by = "VEHICLE_ID") %>%
  group_by(MAKE_MODEL) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(TOTAL_COST)) %>%
  slice_head(n = 5) %>%
  pull(MAKE_MODEL)

cat("\n--- Q8: Top 5 Makes/Models by Total Maintenance Cost ---\n")
print(top5_makes)

q8_data <- maintenance %>%
  filter(!is.na(COST)) %>%
  inner_join(
    assets %>% select(VEHICLE_ID, MAKE_MODEL, IN_SERVICE_DATE),
    by = "VEHICLE_ID"
  ) %>%
  filter(MAKE_MODEL %in% top5_makes) %>%
  mutate(
    MONTHS_IN_SERVICE = as.numeric(interval(IN_SERVICE_DATE, ref_date) / months(1)),
    AGE_BIN = cut(
      MONTHS_IN_SERVICE,
      breaks = seq(0, ceiling(max(MONTHS_IN_SERVICE, na.rm = TRUE) / 12) * 12, by = 12),
      include.lowest = TRUE,
      right = FALSE
    ),
    ODOM_BIN_10K = cut(
      ODOMETER,
      breaks = seq(0, ceiling(max(ODOMETER, na.rm = TRUE) / 10000) * 10000, by = 10000),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  filter(!is.na(AGE_BIN), !is.na(ODOM_BIN_10K))

# 8a: Total cost by months-in-service bins, faceted by MAKE_MODEL
q8a <- q8_data %>%
  group_by(MAKE_MODEL, AGE_BIN) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop")

p8a <- ggplot(q8a, aes(x = AGE_BIN, y = TOTAL_COST)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = dollar_format()) +
  facet_wrap(~MAKE_MODEL, scales = "free_y") +
  labs(
    title = "Maintenance Cost by Months in Service — Top 5 Makes/Models",
    x     = "Months in Service (12-Month Bins)",
    y     = "Total Maintenance Cost ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p8a)

# 8b: Total cost by odometer bins (10,000-mile intervals), faceted by MAKE_MODEL
q8b <- q8_data %>%
  group_by(MAKE_MODEL, ODOM_BIN_10K) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop")

p8b <- ggplot(q8b, aes(x = ODOM_BIN_10K, y = TOTAL_COST)) +
  geom_col(fill = "darkorange") +
  scale_y_continuous(labels = dollar_format()) +
  facet_wrap(~MAKE_MODEL, scales = "free_y") +
  labs(
    title = "Maintenance Cost by Odometer — Top 5 Makes/Models",
    x     = "Odometer (10,000-Mile Bins)",
    y     = "Total Maintenance Cost ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p8b)

cat("\n=== Analysis Complete ===\n")
