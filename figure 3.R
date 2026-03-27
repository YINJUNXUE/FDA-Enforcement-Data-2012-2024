## =========================================================
## Figure 4 | Overall distribution of recall reasons (grouped) (2012–2024)
## Input : enforcement_2012_2024_categorized_v3.csv
## Output:
##  - figure4_reason_distribution.png / .pdf
##  - Table_S3d_reason_distribution.csv
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

dt[, reason_cat := str_squish(as.character(reason_cat))]
dt[is.na(reason_cat) | reason_cat=="", reason_cat := "Other / Unspecified"]

N_total <- nrow(dt)

## ---- reason distribution ----
tab_reason <- dt[, .N, by = reason_cat][order(-N)]
setnames(tab_reason, "N", "Count")
tab_reason[, Percent := round(100 * Count / N_total, 2)]
tab_reason[, `n (%)` := sprintf("%s (%.1f%%)", comma(Count), Percent)]

## export supplementary table (S3d)
fwrite(tab_reason, "Table_S3d_reason_distribution.csv", bom = TRUE)

## ---- factor order (desc, top at top after flip) ----
tab_reason[, reason_cat := factor(reason_cat, levels = rev(tab_reason$reason_cat))]

## ---- label position + extend border (x-axis after flip) ----
xmax <- max(tab_reason$Count)
# label放在柱子右侧 2% 的位置
tab_reason[, label_x := Count + 0.02 * xmax]
# 右侧多留 15% 空间，让panel边框“更长”
xlim_max <- xmax * 1.25

## ---- plot (SCI style, border extended) ----
p4 <- ggplot(tab_reason, aes(x = reason_cat, y = Count)) +
  geom_col(
    width = 0.75,
    fill = "#3C5488",          # muted blue
    color = "black",
    linewidth = 0.4
  ) +
  geom_text(
    aes(y = label_x, label = `n (%)`),
    hjust = 0,
    size = 3.8,
    color = "#2F2F2F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = comma,
    limits = c(0, xlim_max),   # 关键：拉长边框
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Number of recall events"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    plot.margin = margin(10, 90, 10, 10)  # 右侧加宽避免label被裁切
  )

ggsave("figure4_reason_distribution.png", p4, width = 11, height = 6.8, dpi = 300)
ggsave("figure4_reason_distribution.pdf", p4, width = 11, height = 6.8)

cat("\nSaved outputs:\n")
cat("- figure4_reason_distribution.png\n")
cat("- figure4_reason_distribution.pdf\n")
cat("- Table_S3d_reason_distribution.csv\n")
cat("DONE.\n")
