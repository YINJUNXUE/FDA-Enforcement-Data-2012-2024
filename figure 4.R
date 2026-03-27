## =========================================================
## Figure 5 | Distribution of food recalls by product category
## Input : enforcement_2012_2024_categorized_v3.csv
## Output:
##  - figure5_product_category_distribution.png / .pdf
##  - Table_S4_product_category_distribution.csv
## Workdir: C:/Users/73596/Desktop/食品大数据 (1)/转换/分析3
## =========================================================

library(data.table)
library(ggplot2)
library(stringr)
library(scales)

## -------- 1) set working directory --------
setwd("C:/Users/73596/Desktop/食品大数据 (1)/转换/分析4")

infile <- "enforcement_2012_2024_categorized_v3.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")

## -------- 2) clean --------
dt[, product_cat := str_squish(as.character(product_cat))]
dt[is.na(product_cat) | product_cat == "", product_cat := "Other food"]

N_total <- nrow(dt)

## -------- 3) product category distribution --------
tab_prod <- dt[, .N, by = product_cat][order(-N)]
setnames(tab_prod, "N", "Count")

tab_prod[, Percent := round(100 * Count / N_total, 2)]
tab_prod[, `n (%)` := sprintf("%s (%.1f%%)", comma(Count), Percent)]

## export supplementary table (S4)
fwrite(tab_prod, "Table_S4_product_category_distribution.csv", bom = TRUE)

## -------- 4) factor order (desc, top at top after flip) --------
tab_prod[, product_cat := factor(product_cat, levels = rev(product_cat))]

## -------- 5) extend border & label position --------
xmax <- max(tab_prod$Count)
tab_prod[, label_x := Count + 0.02 * xmax]   # label slightly to the right
xlim_max <- xmax * 1.25                      # extend panel border

## -------- 6) plot (SCI style) --------
p_prod <- ggplot(tab_prod, aes(x = product_cat, y = Count)) +
  geom_col(
    width = 0.75,
    fill = "#3C5488",        # same muted blue as Figure 4
    color = "black",
    linewidth = 0.4
  ) +
  geom_text(
    aes(y = label_x, label = `n (%)`),
    hjust = 0,
    size = 3.6,
    color = "#2F2F2F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = comma,
    limits = c(0, xlim_max),
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
    plot.margin = margin(10, 90, 10, 10)
  )

## -------- 7) save --------
ggsave("figure5_product_category_distribution.png",
       p_prod, width = 11, height = 7.2, dpi = 300)

ggsave("figure5_product_category_distribution.pdf",
       p_prod, width = 11, height = 7.2)

cat("\nSaved outputs:\n")
cat("- figure5_product_category_distribution.png\n")
cat("- figure5_product_category_distribution.pdf\n")
cat("- Table_S4_product_category_distribution.csv\n")
cat("DONE.\n")
