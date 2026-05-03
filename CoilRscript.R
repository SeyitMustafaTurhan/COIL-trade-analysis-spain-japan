# ======================================================
# COIL Project: Spain ??? Japan Trade Analysis (2013???2024)
# Economics Team (Ozyegin University)
# ======================================================


#required libraries
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("knitr")
install.packages("plotly")
install.packages("corrplot")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)
library(tidyverse)
library(corrplot)

#setting our directory
setwd("C:/Users/tseyi/Desktop")
#importing data
coil_dataset <- read_xlsx("coilLASTdataset.xlsx")



#looking for data structure

dim(coil_dataset)
glimpse(coil_dataset)


#choosing columns that we need
coil_clean <- coil_dataset %>%
  filter(flowDesc %in% c("Import", "Export"), 
         !is.na(primaryValue)) %>%
  select(refYear, reporterDesc, partnerDesc, flowDesc, cmdDesc, primaryValue) %>%
  mutate(primaryValue = as.numeric(primaryValue))

head(coil_clean)


#total exports & imports
total_trade <- coil_clean %>%
  group_by(refYear, flowDesc) %>%
  summarise(total_value = sum(primaryValue, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  pivot_wider(names_from = flowDesc, values_from = total_value) %>%
  mutate(Trade_Balance = Export - Import,
         Total_Volume = Export + Import)
print(total_trade)

kable(total_trade, digits = 2)


#time series graph
ggplot(total_trade, aes(x = refYear)) +
  geom_line(aes(y = Export, color = "Export"), size = 1.2) +
  geom_line(aes(y = Import, color = "Import"), size = 1.2) +
  labs(title = "Spain - Japan Trade (2013-2024)",
       y = "Million USD", x = "Year", color = "Flow") +
  theme_minimal()


#Top exports & imports sectors
top_export <- coil_clean %>%
  filter(refYear >= 2021, flowDesc == "Export") %>%
  group_by(cmdDesc) %>%
  summarise(avg_export = mean(primaryValue, na.rm = TRUE) / 1e6) %>%
  arrange(desc(avg_export)) %>%
  head(5)

top_import <- coil_clean %>%
  filter(refYear >= 2021, flowDesc == "Import") %>%
  group_by(cmdDesc) %>%
  summarise(avg_import = mean(primaryValue, na.rm = TRUE) / 1e6) %>%
  arrange(desc(avg_import)) %>%
  head(5)

cat("\nTop 5 Export Sectors (2021-2024 average, million USD):\n")
kable(top_export, digits = 2)

cat("\nTop 5 Import Sectors (2021-2024 average, million USD):\n")
kable(top_import, digits = 2)


# sector share within Spain's exports to Japan
rca_df <- coil_clean %>%
  filter(flowDesc == "Export") %>%
  group_by(refYear, cmdDesc) %>%
  summarise(sector_export = sum(primaryValue, na.rm = TRUE), .groups = "drop") %>%
  group_by(refYear) %>%
  mutate(total_export = sum(sector_export),
         share = sector_export / total_export) %>%
  ungroup()
#Showing sectors with consistently high share (above 10%) as potential comparative advantage
high_share <- rca_df %>%
  filter(share > 0.10) %>%
  distinct(cmdDesc)

cat("\nSectors with export share >10% in at least one year (potential comparative advantage):\n")
print(high_share)

# Stacked area chart: sectoral composition of exports over time
df_stack <- coil_clean %>%
  filter(flowDesc == "Export") %>%
  group_by(refYear, cmdDesc) %>%
  summarise(value = sum(primaryValue, na.rm = TRUE) / 1e6, .groups = "drop")

ggplot(df_stack, aes(x = refYear, y = value, fill = cmdDesc)) +
  geom_area(alpha = 0.7) +
  labs(title = "Sectoral Composition of Spain's Exports to Japan (million USD)",
       y = "Value", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("sectoral_composition.png", width = 10, height = 6)



# Identify structural breaks (2019 EPA, COVID-19)

# Calculate year-on-year growth rates
summary(total_trade)

trade_growth <- total_trade %>%
  arrange(refYear) %>%
  mutate(Export_growth = (Export - lag(Export)) / lag(Export) * 100,
         Import_growth = (Import - lag(Import)) / lag(Import) * 100)

print(trade_growth[, c("refYear", "Export_growth", "Import_growth")], digits = 2)

kable(trade_growth[, c("refYear", "Export_growth", "Import_growth")], digits = 2)


cat("\nYear-on-year growth rates (%):\n")
kable(trade_growth[, c("refYear", "Export_growth", "Import_growth")], digits = 2)

# Summary statistics for report
summary_stats <- coil_clean %>%
  group_by(flowDesc) %>%
  summarise(median_value = median(primaryValue, na.rm = TRUE) / 1e6,
            max_value = max(primaryValue, na.rm = TRUE) / 1e6,
            total_period = sum(primaryValue, na.rm = TRUE) / 1e6)


cat("\nSummary Statistics (million USD, 2013-2024):\n")
kable(summary_stats, digits = 2)


# Index Calculation Grubel-Llyold
gl_index <- coil_clean %>%
  group_by(refYear, cmdDesc) %>%
  summarise(export = sum(primaryValue[flowDesc == "Export"], na.rm = TRUE),
            import = sum(primaryValue[flowDesc == "Import"], na.rm = TRUE)) %>%
  mutate(gl = 1 - (abs(export - import) / (export + import))) %>%
  filter(export + import > 0, is.finite(gl))


p <- ggplot(gl_index, aes(x = refYear, y = gl, color = cmdDesc)) +
  geom_line() +
  labs(title = "Grubel-Lloyd Intra-Industry Trade Index",
       y = "GL Index (0=between industries, 1=domestic trade)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave("GL_index.png", p, width = 10, height = 6)



# Basic linear trend (for export)
trend_model <- lm(Export ~ refYear, data = total_trade)
summary(trend_model)

#YearsXsectors

top_sectors <- coil_clean %>%
  filter(flowDesc == "Export") %>%
  group_by(cmdDesc) %>%
  summarise(total_export = sum(primaryValue, na.rm = TRUE)) %>%
  arrange(desc(total_export)) %>%
  head(6) %>%
  pull(cmdDesc)

wide_top <- coil_clean %>%
  filter(flowDesc == "Export", cmdDesc %in% top_sectors) %>%
  group_by(refYear, cmdDesc) %>%
  summarise(value = sum(primaryValue, na.rm = TRUE)) %>%
  pivot_wider(names_from = cmdDesc, values_from = value, values_fill = 0)


cor_matrix_top <- cor(wide_top[, -1], use = "pairwise.complete.obs")

corrplot(cor_matrix_top, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.6, tl.cex = 0.8)

# Export before 2019 and after 2019
before <- total_trade$Export[total_trade$refYear < 2019]
after <- total_trade$Export[total_trade$refYear >= 2019]
t.test(before, after)  



ggplot(total_trade, aes(x = refYear, y = Export)) +
  geom_line() +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
  annotate("text", x = 2019, y = max(total_trade$Export, na.rm = TRUE), 
           label = "EPA Takes Effect", angle = 90, vjust = -0.5)










#Seyit Mustafa Turhan
#Ella Widau
#Allison Miller 









































































