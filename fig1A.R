## =========================================================
## Figure 2 | Annual number of FDA food recall events (2012–2024)
## Input : enforcement_2012_2024_categorized_v3.csv
## Output:
##  - Figure2_yearly_counts.png / .pdf
##  - Table_S3a_year_counts.csv   (recommended supplementary table)
## Workdir: C:/Users/73596/Desktop/食品大数据 (1)/转换/分析3
## =========================================================

library(data.table)
library(ggplot2)
library(scales)

## ---- workdir ----
setwd("C:/Users/73596/Desktop/食品大数据 (1)/转换/分析4")

## ---- input ----
infile <- "enforcement_2012_2024_categorized_v3.csv"
if (!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")

## ---- clean year ----
dt[, year := as.integer(year)]
dt <- dt[!is.na(year) & year >= 2012 & year <= 2024]

## ---- annual counts ----
fig2_dt <- dt[, .N, by = year][order(year)]
setnames(fig2_dt, "N", "Recall_events")

## ---- supplementary table S3a ----
N_total <- nrow(dt)
fig2_dt[, Percent := round(100 * Recall_events / N_total, 2)]
fwrite(fig2_dt, "Table_S3a_year_counts.csv", bom = TRUE)

## ---- plot (SCI style: new color, percent labels, border, no title) ----
p2 <- ggplot(fig2_dt, aes(x = year, y = Recall_events)) +
  
  ## bars with border
  geom_col(
    width = 0.7,
    fill = "#3C5488",      # SCI-friendly muted blue
    color = "black",
    linewidth = 0.4
  ) +
  
  ## line + points
  geom_line(
    aes(group = 1),
    color = "#2F2F2F",
    linewidth = 0.9
  ) +
  geom_point(
    color = "#2F2F2F",
    size = 2.4
  ) +
  
  ## percent labels above bars
  geom_text(
    aes(label = paste0(Percent, "%")),
    vjust = -0.5,
    size = 4,
    color = "#2F2F2F"
  ) +
  
  ## axis scales
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))  # leave headroom for percent labels
  ) +
  
  ## no title (caption goes in manuscript)
  labs(
    x = "Year",
    y = "Number of recall events"
  ) +
  
  ## classic theme + full border
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black")
  )

## ---- save ----
ggsave("Figure2_yearly_counts.png", p2, width = 10, height = 6, dpi = 300)
ggsave("Figure2_yearly_counts.pdf", p2, width = 10, height = 6)

cat("\nSaved outputs:\n")
cat("- Figure2_yearly_counts.png\n")
cat("- Figure2_yearly_counts.pdf\n")
cat("- Table_S3a_year_counts.csv\n")
cat("DONE.\n")
