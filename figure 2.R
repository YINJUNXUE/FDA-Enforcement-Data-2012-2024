## =========================================================
## Figure 3 | Recall classification (Class I/II/III) by year (2012–2024)
## Input : enforcement_2012_2024_categorized_v3.csv
## Output:
##  - Figure3_class_by_year_percent.png / .pdf   (100% stacked)
##  - Table_S3b_year_by_class_counts.csv
##  - Table_S3c_year_by_class_percent.csv
## Workdir: C:/Users/73596/Desktop/食品大数据 (1)/转换/分析3
## =========================================================

library(data.table)
library(stringr)
library(ggplot2)
library(scales)

setwd("C:/Users/73596/Desktop/食品大数据 (1)/转换/分析4")

infile <- "enforcement_2012_2024_categorized_v3.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")

## ---- clean ----
dt[, year := as.integer(year)]
dt <- dt[!is.na(year) & year >= 2012 & year <= 2024]

dt[, classification := str_squish(as.character(classification))]
dt[is.na(classification) | classification=="", classification := "Unknown"]

## keep only I/II/III else Unknown
dt[, class3 := fifelse(classification %in% c("Class I","Class II","Class III"),
                       classification, "Unknown")]

## ---- build Year x Class table ----
tab <- dt[, .N, by = .(year, class3)]
setnames(tab, "N", "Count")

## complete missing combinations
all_year  <- sort(unique(dt$year))
all_class <- c("Class I","Class II","Class III","Unknown")
grid <- CJ(year = all_year, class3 = all_class)
tab <- merge(grid, tab, by = c("year","class3"), all.x = TRUE)
tab[is.na(Count), Count := 0L]

## yearly totals + percent
tab[, Year_total := sum(Count), by = year]
tab[, Percent := ifelse(Year_total == 0, 0, 100 * Count / Year_total)]

## ---- export supplementary tables ----
tab_counts <- copy(tab)[order(year, match(class3, all_class))]
fwrite(tab_counts, "Table_S3b_year_by_class_counts.csv", bom = TRUE)

tab_pct <- tab_counts[, .(year, class3, Percent)]
fwrite(tab_pct, "Table_S3c_year_by_class_percent.csv", bom = TRUE)

## ---- Figure 3 (100% stacked): plot only Class I/II/III ----
plot_dt <- tab[class3 %in% c("Class I","Class II","Class III")]
plot_dt[, class3 := factor(class3, levels = c("Class I","Class II","Class III"))]

## label settings: show percent text only if >= 3% (avoid clutter)
label_threshold <- 3

plot_dt[, label := ifelse(Percent >= label_threshold, paste0(round(Percent, 1), "%"), "")]

## SCI-friendly palette (safe for print)
pal_class <- c(
  "Class I"  = "#B22222",  # muted red
  "Class II" = "#3C5488",  # muted blue
  "Class III"= "#4D4D4D"   # dark gray
)

p3 <- ggplot(plot_dt, aes(x = factor(year), y = Percent, fill = class3)) +
  geom_col(width = 0.78, color = "black", linewidth = 0.35) +
  
  ## percent labels centered in each stack
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3.6,
    color = "white",
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = pal_class) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Year",
    y = "Proportion of recall events",
    fill = "Classification"
  ) +
  
  ## SCI theme: border, no title
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("Figure3_class_by_year_percent.png", p3, width = 11, height = 6.5, dpi = 300)
ggsave("Figure3_class_by_year_percent.pdf", p3, width = 11, height = 6.5)

cat("\nSaved outputs:\n")
cat("- Figure3_class_by_year_percent.png\n")
cat("- Figure3_class_by_year_percent.pdf\n")
cat("- Table_S3b_year_by_class_counts.csv\n")
cat("- Table_S3c_year_by_class_percent.csv\n")
cat("DONE.\n")
