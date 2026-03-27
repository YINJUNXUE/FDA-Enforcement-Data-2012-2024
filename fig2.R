## =========================================================
## Figure 6 | Heatmap: reason_cat × product_cat (Top products)
## v4 revision version
##
## Input : enforcement_2012_2024_categorized_v4.csv
## Output:
##   - figure6_reason_by_product_heatmap_v4.png
##   - figure6_reason_by_product_heatmap_v4.pdf
##   - Table_S3e_reason_by_product_counts_topN_v4.csv
##   - Table_S3e_reason_by_product_counts_full_v4.csv
## =========================================================

library(data.table)
library(ggplot2)
library(stringr)
library(viridis)
library(grid)

## -------- 0) set working dir --------
setwd("D:/桌面临时文件/已经发表论文/慧玲论文/食品大数据 FDA/转换/回修跑代码")

## -------- 1) read data --------
infile <- "enforcement_2012_2024_categorized_v4.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")
cat("Rows:", nrow(dt), "\n")

## -------- 2) basic cleaning / required columns --------
need_cols <- c("reason_cat", "product_cat")
miss_cols <- setdiff(need_cols, names(dt))
if(length(miss_cols) > 0) stop(paste("Missing columns:", paste(miss_cols, collapse = ", ")))

dt[, reason_cat  := str_squish(as.character(reason_cat))]
dt[, product_cat := str_squish(as.character(product_cat))]

dt[is.na(reason_cat)  | reason_cat  == "", reason_cat  := "Other / Unspecified"]
dt[is.na(product_cat) | product_cat == "", product_cat := "Other food"]

## 如果你不想把兜底类放入热图，可打开这一行：
## dt <- dt[reason_cat != "Other / Unspecified" & product_cat != "Other food"]

## -------- 3) full cross-tab (all products) for supplementary table --------
tab_full <- dt[, .N, by = .(reason_cat, product_cat)]
setnames(tab_full, "N", "Count")
setorder(tab_full, -Count)

fwrite(tab_full, "Table_S3e_reason_by_product_counts_full_v4.csv", bom = TRUE)
cat("Saved: Table_S3e_reason_by_product_counts_full_v4.csv\n")

## -------- 4) choose top N products for plotting --------
topN <- 15
top_products <- dt[, .N, by = product_cat][order(-N)][1:min(topN, .N), product_cat]

## -------- 5) counts for top products + COMPLETE GRID --------
tab_top <- tab_full[product_cat %in% top_products]

all_reason <- unique(dt$reason_cat)
grid_full  <- CJ(reason_cat = all_reason, product_cat = top_products, unique = TRUE)

tab_plot <- merge(grid_full, tab_top, by = c("reason_cat", "product_cat"), all.x = TRUE)
tab_plot[is.na(Count), Count := 0L]

## -------- 6) ordering --------
reason_order <- c(
  "Undeclared allergen",
  "Bacterial pathogen contamination",
  "Viral contamination",
  "Microbial toxin / spore-forming contamination",
  "Foreign material",
  "Chemical contamination / Toxins / Adulteration",
  "Labeling / Misbranding (non-allergen)",
  "Temperature control / Storage deviation",
  "Regulatory / GMP / Process deviation",
  "Packaging / Container defect",
  "Spoilage / Quality defect",
  "Other / Unspecified"
)
reason_order <- reason_order[reason_order %in% unique(dt$reason_cat)]

product_order <- dt[product_cat %in% top_products, .N, by = product_cat][order(-N), product_cat]

tab_plot[, reason_cat  := factor(reason_cat,  levels = rev(reason_order))]
tab_plot[, product_cat := factor(product_cat, levels = product_order)]

## -------- 7) transform for fill --------
tab_plot[, fill_log := log10(Count + 1)]

## -------- 8) export topN support table --------
tab_out <- copy(tab_plot)
tab_out[, reason_cat := as.character(reason_cat)]
tab_out[, product_cat := as.character(product_cat)]

fwrite(
  tab_out[, .(reason_cat, product_cat, Count)],
  "Table_S3e_reason_by_product_counts_topN_v4.csv",
  bom = TRUE
)
cat("Saved: Table_S3e_reason_by_product_counts_topN_v4.csv\n")

## -------- 9) plot --------
p <- ggplot(tab_plot, aes(x = product_cat, y = reason_cat, fill = fill_log)) +
  geom_tile(color = "grey90", linewidth = 0.35) +
  scale_fill_viridis_c(
    option = "C",
    name = "log10(Count + 1)"
  ) +
  labs(
    x = "Product category",
    y = "Recall reason category"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm")
  )

## -------- 10) save --------
ggsave("figure6_reason_by_product_heatmap_v4.png", p, width = 12.5, height = 7.4, dpi = 600)
ggsave("figure6_reason_by_product_heatmap_v4.pdf", p, width = 12.5, height = 7.4)

cat("\nDone.\nSaved outputs:\n")
cat("- figure6_reason_by_product_heatmap_v4.png\n")
cat("- figure6_reason_by_product_heatmap_v4.pdf\n")
cat("- Table_S3e_reason_by_product_counts_full_v4.csv\n")
cat("- Table_S3e_reason_by_product_counts_topN_v4.csv\n")
