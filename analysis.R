# ============================================================
# BSAN 615 – Week 8 Analysis
# Fleet Asset, Fuel & Maintenance Analysis
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)

# ============================================================
# LOAD DATA
# ============================================================
assets      <- read_csv("assets.csv",      show_col_types = FALSE)
fuel        <- read_csv("fuel.csv",        show_col_types = FALSE)
maintenance <- read_csv("maintenance.csv", show_col_types = FALSE)

# Standardize dates and create derived columns
assets <- assets %>%
  mutate(
    IN_SERVICE_DATE   = ymd(IN_SERVICE_DATE),
    MAKE_MODEL        = paste(MAKE, MODEL),
    # Q2 / Q7 / Q8: age to TODAY (instructor-specified formula)
    MONTHS_IN_SERVICE = interval(IN_SERVICE_DATE, ymd(Sys.Date())) %/% months(1)
  )

fuel <- fuel %>%
  mutate(TRANS_DATE = ymd(TRANS_DATE))

maintenance <- maintenance %>%
  mutate(REPAIR_ORDER_DATE = ymd(REPAIR_ORDER_DATE))

# ============================================================
# Q1: Top 10 Makes/Models by Asset Count
# ============================================================
q1 <- assets %>%
  count(MAKE_MODEL, name = "ASSET_COUNT") %>%
  arrange(desc(ASSET_COUNT)) %>%
  slice_head(n = 10)

print(q1)

p1 <- ggplot(q1, aes(x = reorder(MAKE_MODEL, ASSET_COUNT), y = ASSET_COUNT)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = ASSET_COUNT), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Q1: Top 10 Makes/Models by Asset Count",
       x = "Make / Model", y = "Number of Assets") +
  theme_minimal()
print(p1)

# ============================================================
# Q2: Assets by Age (months-in-service) and Odometer
# ============================================================

# --- 2a: Age bins (12-month intervals) ---
max_months <- max(assets$MONTHS_IN_SERVICE, na.rm = TRUE)
age_breaks <- seq(0, ceiling(max_months / 12) * 12, by = 12)

q2_age <- assets %>%
  filter(!is.na(MONTHS_IN_SERVICE)) %>%
  mutate(AGE_BIN = cut(MONTHS_IN_SERVICE,
                       breaks = age_breaks,
                       include.lowest = TRUE,
                       right = FALSE)) %>%
  count(AGE_BIN)

p2a <- ggplot(q2_age, aes(x = AGE_BIN, y = n)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  labs(title = "Q2a: Assets by Months-in-Service (12-month bins)",
       x = "Months in Service", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2a)

# --- 2b: Odometer bins (5,000-mile intervals) ---
max_odom <- max(assets$CURRENT_ODOM, na.rm = TRUE)
odom_breaks <- seq(0, ceiling(max_odom / 5000) * 5000, by = 5000)

q2_odom <- assets %>%
  filter(!is.na(CURRENT_ODOM)) %>%
  mutate(ODOM_BIN = cut(CURRENT_ODOM,
                        breaks = odom_breaks,
                        include.lowest = TRUE,
                        right = FALSE)) %>%
  count(ODOM_BIN)

p2b <- ggplot(q2_odom, aes(x = ODOM_BIN, y = n)) +
  geom_col(fill = "seagreen") +
  geom_text(aes(label = n), vjust = -0.4, size = 2.5) +
  labs(title = "Q2b: Assets by Current Odometer (5,000-mile bins)",
       x = "Odometer Reading (miles)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2b)

# ============================================================
# Q3: Typical Annual Fuel Spend
# Data limitation: fuel.csv covers ~July–Oct 2018 (~4 months).
# We annualize: total_spend / months_covered * 12
# ============================================================
cat("\n--- Q3 Data Limitation Note ---\n")
cat("The fuel dataset covers only a partial year (approx. mid-2018).\n")
cat("Annual spend is estimated by annualizing the observed spend.\n\n")

# Determine months covered in the data
fuel_date_range <- range(fuel$TRANS_DATE, na.rm = TRUE)
months_covered  <- as.numeric(interval(fuel_date_range[1], fuel_date_range[2]) / months(1))
months_covered  <- max(months_covered, 1)  # avoid division by zero
cat(sprintf("Fuel data date range: %s to %s (%.1f months)\n",
            fuel_date_range[1], fuel_date_range[2], months_covered))

q3 <- fuel %>%
  filter(!is.na(AMOUNT), AMOUNT > 0) %>%
  group_by(VEHICLE_ID) %>%
  summarise(TOTAL_SPEND = sum(AMOUNT, na.rm = TRUE), .groups = "drop") %>%
  mutate(ANNUAL_SPEND_EST = TOTAL_SPEND / months_covered * 12)

cat("\nSummary of Estimated Annual Fuel Spend per Vehicle:\n")
print(summary(q3$ANNUAL_SPEND_EST))

p3 <- ggplot(q3, aes(x = ANNUAL_SPEND_EST)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = median(ANNUAL_SPEND_EST, na.rm = TRUE)),
             color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = dollar_format()) +
  labs(title = "Q3: Estimated Annual Fuel Spend per Vehicle",
       subtitle = sprintf("Based on %.1f months of data (annualized) — red line = median", months_covered),
       x = "Estimated Annual Spend ($)", y = "Number of Vehicles") +
  theme_minimal()
print(p3)

# ============================================================
# Q4: Top 10 Makes/Models by Highest Average CPG
# ============================================================
fuel_types_valid <- c("UNLEADED", "UNL PLUS", "SUPER UN", "UNLALC10",
                      "UNLALC57", "ETHANL85", "DIESEL", "DIESEL PREMIUM")

q4 <- fuel %>%
  filter(PRODUCT_NAME %in% fuel_types_valid,
         !is.na(UNITS), UNITS > 0,
         !is.na(AMOUNT), AMOUNT > 0) %>%
  mutate(CPG = AMOUNT / UNITS) %>%
  filter(is.finite(CPG), CPG > 0, CPG < 20) %>%   # remove outliers / errors
  left_join(assets %>% select(VEHICLE_ID, MAKE_MODEL), by = "VEHICLE_ID") %>%
  filter(!is.na(MAKE_MODEL)) %>%
  group_by(MAKE_MODEL) %>%
  summarise(AVG_CPG = mean(CPG, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(AVG_CPG)) %>%
  slice_head(n = 10)

print(q4)

p4 <- ggplot(q4, aes(x = reorder(MAKE_MODEL, AVG_CPG), y = AVG_CPG)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = round(AVG_CPG, 2)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = dollar_format(prefix = "$")) +
  labs(title = "Q4: Top 10 Makes/Models by Average Cost Per Gallon (CPG)",
       x = "Make / Model", y = "Avg CPG ($)") +
  theme_minimal()
print(p4)

# ============================================================
# Q5: Average CPG by State — Gasoline and Diesel
# ============================================================
gasoline_types <- c("UNLEADED", "UNL PLUS", "SUPER UN", "UNLALC10", "UNLALC57", "ETHANL85")
diesel_types   <- c("DIESEL", "DIESEL PREMIUM")

q5 <- fuel %>%
  filter(PRODUCT_NAME %in% c(gasoline_types, diesel_types),
         !is.na(UNITS), UNITS > 0,
         !is.na(AMOUNT), AMOUNT > 0) %>%
  mutate(
    CPG       = AMOUNT / UNITS,
    FUEL_TYPE = if_else(PRODUCT_NAME %in% gasoline_types, "Gasoline", "Diesel")
  ) %>%
  filter(is.finite(CPG), CPG > 0, CPG < 20) %>%
  group_by(SITE_STATE, FUEL_TYPE) %>%
  summarise(AVG_CPG = mean(CPG, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(q5, aes(x = reorder(SITE_STATE, AVG_CPG), y = AVG_CPG, fill = FUEL_TYPE)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~FUEL_TYPE, scales = "free_x") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_fill_manual(values = c("Gasoline" = "steelblue", "Diesel" = "firebrick")) +
  labs(title = "Q5: Average Cost Per Gallon by State",
       x = "State", y = "Avg CPG ($)") +
  theme_minimal(base_size = 9)
print(p5)

# ============================================================
# Q6: Top 5 Makes/Models by Highest Maintenance Cost Per Month
# ============================================================
q6 <- maintenance %>%
  filter(!is.na(COST)) %>%
  left_join(assets %>% select(VEHICLE_ID, MAKE_MODEL, MONTHS_IN_SERVICE),
            by = "VEHICLE_ID") %>%
  filter(!is.na(MAKE_MODEL), !is.na(MONTHS_IN_SERVICE), MONTHS_IN_SERVICE > 0) %>%
  group_by(VEHICLE_ID, MAKE_MODEL, MONTHS_IN_SERVICE) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop") %>%
  mutate(COST_PER_MONTH = TOTAL_COST / MONTHS_IN_SERVICE) %>%
  group_by(MAKE_MODEL) %>%
  summarise(AVG_CPM = mean(COST_PER_MONTH, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(AVG_CPM)) %>%
  slice_head(n = 5)

print(q6)

p6 <- ggplot(q6, aes(x = reorder(MAKE_MODEL, AVG_CPM), y = AVG_CPM)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = dollar(round(AVG_CPM, 2))), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  labs(title = "Q6: Top 5 Makes/Models by Average Maintenance Cost Per Month",
       x = "Make / Model", y = "Avg Cost Per Month ($)") +
  theme_minimal()
print(p6)

# ============================================================
# Q7: Repair Orders, Items, and Cost by 12-Month Age Bins
# Join maintenance with assets to get vehicle age
# ============================================================
maint_with_age <- maintenance %>%
  filter(!is.na(COST)) %>%
  left_join(assets %>% select(VEHICLE_ID, MAKE_MODEL, MONTHS_IN_SERVICE),
            by = "VEHICLE_ID") %>%
  filter(!is.na(MONTHS_IN_SERVICE))

age_breaks_q7 <- seq(0, ceiling(max(maint_with_age$MONTHS_IN_SERVICE, na.rm=TRUE) / 12) * 12, by = 12)

maint_with_age <- maint_with_age %>%
  mutate(AGE_BIN = cut(MONTHS_IN_SERVICE,
                       breaks = age_breaks_q7,
                       include.lowest = TRUE,
                       right = FALSE))

q7 <- maint_with_age %>%
  group_by(AGE_BIN) %>%
  summarise(
    N_REPAIR_ORDERS = n_distinct(REPAIR_ORDER_NO),
    N_LINE_ITEMS    = n(),
    TOTAL_COST      = sum(COST, na.rm = TRUE),
    AVG_COST_PER_RO = TOTAL_COST / N_REPAIR_ORDERS,
    .groups = "drop"
  )

cat("\n--- Q7: Repair Summary by Vehicle Age Bin ---\n")
print(q7)

p7a <- ggplot(q7, aes(x = AGE_BIN, y = N_REPAIR_ORDERS)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = N_REPAIR_ORDERS), vjust = -0.4, size = 3) +
  labs(title = "Q7a: Number of Repair Orders by Vehicle Age (12-Month Bins)",
       x = "Vehicle Age (Months in Service)", y = "# Repair Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7a)

p7b <- ggplot(q7, aes(x = AGE_BIN, y = N_LINE_ITEMS)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = N_LINE_ITEMS), vjust = -0.4, size = 3) +
  labs(title = "Q7b: Number of Repair Line Items by Vehicle Age",
       x = "Vehicle Age (Months in Service)", y = "# Line Items") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7b)

p7c <- ggplot(q7, aes(x = AGE_BIN, y = TOTAL_COST)) +
  geom_col(fill = "seagreen") +
  geom_text(aes(label = dollar(round(TOTAL_COST))), vjust = -0.4, size = 2.8) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Q7c: Total Maintenance Cost by Vehicle Age",
       x = "Vehicle Age (Months in Service)", y = "Total Cost ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7c)

p7d <- ggplot(q7, aes(x = AGE_BIN, y = AVG_COST_PER_RO)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = dollar(round(AVG_COST_PER_RO))), vjust = -0.4, size = 2.8) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Q7d: Avg Cost Per Repair Order by Vehicle Age",
       x = "Vehicle Age (Months in Service)", y = "Avg Cost Per RO ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7d)

# ============================================================
# Q8: At What Months and Miles Do Expenses Increase?
# Use vehicle age (MONTHS_IN_SERVICE from assets) and
# odometer at time of repair (from maintenance.csv ODOMETER)
# ============================================================

# Top 5 makes/models by total maintenance cost
top5_makes <- maintenance %>%
  left_join(assets %>% select(VEHICLE_ID, MAKE_MODEL), by = "VEHICLE_ID") %>%
  filter(!is.na(MAKE_MODEL), !is.na(COST)) %>%
  group_by(MAKE_MODEL) %>%
  summarise(TOTAL = sum(COST, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(TOTAL)) %>%
  slice_head(n = 5) %>%
  pull(MAKE_MODEL)

maint_top5 <- maintenance %>%
  left_join(assets %>% select(VEHICLE_ID, MAKE_MODEL, MONTHS_IN_SERVICE),
            by = "VEHICLE_ID") %>%
  filter(MAKE_MODEL %in% top5_makes,
         !is.na(COST), !is.na(MONTHS_IN_SERVICE))

age_breaks_q8  <- seq(0, ceiling(max(maint_top5$MONTHS_IN_SERVICE, na.rm=TRUE)/12)*12, by=12)
odom_breaks_q8 <- seq(0, ceiling(max(maint_top5$ODOMETER, na.rm=TRUE)/10000)*10000, by=10000)

maint_top5 <- maint_top5 %>%
  mutate(
    AGE_BIN  = cut(MONTHS_IN_SERVICE, breaks = age_breaks_q8,  include.lowest=TRUE, right=FALSE),
    ODOM_BIN = cut(ODOMETER,          breaks = odom_breaks_q8, include.lowest=TRUE, right=FALSE)
  )

# Q8 Plot 1: Cost by vehicle age (months) — faceted by make/model
q8_age <- maint_top5 %>%
  group_by(MAKE_MODEL, AGE_BIN) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop")

p8a <- ggplot(q8_age, aes(x = AGE_BIN, y = TOTAL_COST, group = MAKE_MODEL)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~MAKE_MODEL, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Q8a: Maintenance Cost by Vehicle Age (Top 5 Makes/Models)",
       subtitle = "Each panel = one make/model; x = months in service since delivery",
       x = "Vehicle Age (Months in Service)", y = "Total Maintenance Cost ($)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p8a)

# Q8 Plot 2: Cost by odometer at time of repair — faceted by make/model
q8_odom <- maint_top5 %>%
  filter(!is.na(ODOM_BIN)) %>%
  group_by(MAKE_MODEL, ODOM_BIN) %>%
  summarise(TOTAL_COST = sum(COST, na.rm = TRUE), .groups = "drop")

p8b <- ggplot(q8_odom, aes(x = ODOM_BIN, y = TOTAL_COST, group = MAKE_MODEL)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "firebrick", size = 2) +
  facet_wrap(~MAKE_MODEL, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Q8b: Maintenance Cost by Odometer at Time of Repair (Top 5 Makes/Models)",
       subtitle = "Each panel = one make/model; x = odometer reading at repair (10k-mile bins)",
       x = "Odometer at Repair (miles)", y = "Total Maintenance Cost ($)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p8b)

cat("\n=== Analysis Complete ===\n")
