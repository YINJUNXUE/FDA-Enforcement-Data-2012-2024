## =========================================================
## Advanced analyses (revised, reviewer-aligned)
##
## Based on:
##   enforcement_2012_2024_categorized_v4.csv
##
## Workdir:
##   D:/桌面临时文件/已经发表论文/慧玲论文/食品大数据 FDA/转换/回修跑代码
##
## Outputs:
##  A) figure7_adjTrend_ClassI_probability_v4.png/.pdf
##     + Table_A1_adjTrend_year_v4.csv
##
##  B) Table_B1_multinomial_RRR_v4.csv   [optional supplementary]
##
##  D) Table_D1_sensitivity_compare_v4.csv
## =========================================================

library(data.table)
library(stringr)
library(ggplot2)
library(scales)
library(dplyr)
library(sandwich)
library(lmtest)

setwd("D:/桌面临时文件/已经发表论文/慧玲论文/食品大数据 FDA/转换/回修跑代码")

infile <- "enforcement_2012_2024_categorized_v4.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))
dt <- fread(infile, encoding = "UTF-8")

## -------------------------
## 0) helper
## -------------------------
fix_unknown <- function(x, unknown_label = "Unknown"){
  s <- str_squish(as.character(x))
  s[is.na(s) | s == "" | str_detect(s, "^\\s*$")] <- unknown_label
  s
}

theme_sci <- function(base_size = 14){
  theme_classic(base_size = base_size) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    )
}

## -------------------------
## 1) basic cleaning
## -------------------------
need_cols <- c(
  "class_binary", "classification_std", "year",
  "product_cat", "reason_cat", "dist_cat", "region_final",
  "recalling_firm"
)
miss_cols <- setdiff(need_cols, names(dt))
if(length(miss_cols) > 0){
  stop(paste("Missing columns:", paste(miss_cols, collapse = ", ")))
}

dt[, year := as.integer(year)]
dt <- dt[!is.na(year) & year >= 2012 & year <= 2024]

dt[, class_binary := as.integer(class_binary)]
dt <- dt[!is.na(class_binary)]

dt[, classification_std := str_squish(as.character(classification_std))]
dt[, product_cat := fix_unknown(product_cat, "Other food")]
dt[, reason_cat := fix_unknown(reason_cat, "Other / Unspecified")]
dt[, dist_cat := fix_unknown(dist_cat, "Unknown/Other")]
dt[, region_final := fix_unknown(region_final, "Unknown")]
dt[, recalling_firm := str_to_lower(fix_unknown(recalling_firm, "unknown firm"))]
dt[, recalling_firm := str_squish(recalling_firm)]

## 统一分类标签
dt[classification_std %chin% c("Class Ii"), classification_std := "Class II"]
dt[classification_std %chin% c("Class Iii"), classification_std := "Class III"]

## 因子化
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

dt2 <- copy(dt)
dt2[, reason_cat := factor(reason_cat, levels = reason_order)]
dt2[, reason_cat := relevel(reason_cat, ref = "Undeclared allergen")]

# 参考水平
prod_ref <- if("Ready-to-eat / Prepared foods" %in% unique(dt2$product_cat)) {
  "Ready-to-eat / Prepared foods"
} else sort(unique(dt2$product_cat))[1]

dist_ref <- if("Single state" %in% unique(dt2$dist_cat)) {
  "Single state"
} else sort(unique(dt2$dist_cat))[1]

region_ref <- if("Midwest" %in% unique(dt2$region_final)) {
  "Midwest"
} else sort(unique(dt2$region_final))[1]

dt2[, product_cat := factor(product_cat)]
dt2[, dist_cat := factor(dist_cat)]
dt2[, region_final := factor(region_final)]
dt2[, year := factor(year)]

dt2[, product_cat := relevel(product_cat, ref = prod_ref)]
dt2[, dist_cat := relevel(dist_cat, ref = dist_ref)]
dt2[, region_final := relevel(region_final, ref = region_ref)]
dt2[, year := relevel(year, ref = "2012")]

## =========================================================
## A) Adjusted time trend using categorical year
## Main model aligned with revised manuscript
## =========================================================
fitA <- glm(
  class_binary ~ reason_cat + product_cat + dist_cat + region_final + year,
  data = dt2,
  family = binomial()
)

## 预测每年的 adjusted probability
years_all <- sort(unique(dt2$year))
pred_list <- list()

for(y in years_all){
  newdat <- copy(dt2)
  newdat[, year := factor(y, levels = levels(dt2$year))]
  pred <- predict(fitA, newdata = newdat, type = "response")
  pred_list[[as.character(y)]] <- data.table(
    year = as.integer(as.character(y)),
    AdjProb = mean(pred, na.rm = TRUE)
  )
}

trendA <- rbindlist(pred_list)[order(year)]
fwrite(trendA, "Table_A1_adjTrend_year_v4.csv", bom = TRUE)

pA <- ggplot(trendA, aes(x = year, y = AdjProb)) +
  geom_line(color = "#2F2F2F", linewidth = 1.0) +
  geom_point(color = "#2F2F2F", size = 2.4) +
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(x = "Year", y = "Adjusted probability of Class I") +
  theme_sci(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figure7_adjTrend_ClassI_probability_v4.png", pA, width = 10, height = 6, dpi = 300)
ggsave("figure7_adjTrend_ClassI_probability_v4.pdf", pA, width = 10, height = 6)

cat("Saved: Table_A1_adjTrend_year_v4.csv\n")
cat("Saved: figure7_adjTrend_ClassI_probability_v4.png/.pdf\n")

## =========================================================
## B) Multinomial logistic regression (optional supplementary)
## Class III as reference
## Note: year modeled categorically
## =========================================================
if(any(!is.na(dt2$classification_std))){
  suppressPackageStartupMessages(library(nnet))
  
  dtB <- dt2[!is.na(classification_std)]
  dtB[, classification_std := factor(classification_std,
                                     levels = c("Class III", "Class II", "Class I"))]
  
  mB <- nnet::multinom(
    classification_std ~ reason_cat + product_cat + dist_cat + region_final + year,
    data = dtB,
    trace = FALSE
  )
  
  coefs <- summary(mB)$coefficients
  ses   <- summary(mB)$standard.errors
  
  tidy_multinom <- function(coefs, ses){
    out <- list()
    for(k in rownames(coefs)){
      b  <- coefs[k, ]
      se <- ses[k, ]
      tmp <- data.table(
        Comparison = paste0(k, " vs Class III"),
        Term = names(b),
        RRR = exp(as.numeric(b)),
        LCL = exp(as.numeric(b) - 1.96 * as.numeric(se)),
        UCL = exp(as.numeric(b) + 1.96 * as.numeric(se))
      )
      out[[k]] <- tmp
    }
    rbindlist(out, use.names = TRUE, fill = TRUE)
  }
  
  resB <- tidy_multinom(coefs, ses)
  resB <- resB[Term != "(Intercept)"]
  resB[, `RRR (95% CI)` := sprintf("%.2f (%.2f–%.2f)", RRR, LCL, UCL)]
  fwrite(resB, "Table_B1_multinomial_RRR_v4.csv", bom = TRUE)
  
  cat("Saved: Table_B1_multinomial_RRR_v4.csv\n")
} else {
  cat("Skip B) multinomial: classification_std not available.\n")
}

## =========================================================
## D) Sensitivity analyses
## D1: Class I vs Class II
## D2: Class I vs Class III
## D3: one recall per firm per year
## =========================================================

extract_clustered_glm <- function(model, cluster_var, scenario_label){
  vc <- vcovCL(model, cluster = cluster_var)
  cf <- coef(model)
  se <- sqrt(diag(vc))
  
  out <- data.table(
    Scenario = scenario_label,
    Term = names(cf),
    beta = as.numeric(cf),
    se = as.numeric(se)
  )
  out <- out[Term != "(Intercept)"]
  out[, P_value := 2 * pnorm(abs(beta / se), lower.tail = FALSE)]
  out[, OR := exp(beta)]
  out[, LCL := exp(beta - 1.96 * se)]
  out[, UCL := exp(beta + 1.96 * se)]
  out[, `aOR (95% CI)` := sprintf("%.2f (%.2f–%.2f)", OR, LCL, UCL)]
  out[, `P value` := fifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))]
  out[]
}

## D1: Class I vs Class II
dt_d1 <- dt2[classification_std %chin% c("Class I", "Class II")]
dt_d1[, y := fifelse(classification_std == "Class I", 1L, 0L)]

m_d1 <- glm(
  y ~ reason_cat + product_cat + dist_cat + region_final + year,
  data = dt_d1,
  family = binomial()
)
res_d1 <- extract_clustered_glm(m_d1, ~ recalling_firm, "Sensitivity 1: Class I vs Class II")

## D2: Class I vs Class III
dt_d2 <- dt2[classification_std %chin% c("Class I", "Class III")]
dt_d2[, y := fifelse(classification_std == "Class I", 1L, 0L)]

m_d2 <- glm(
  y ~ reason_cat + product_cat + dist_cat + region_final + year,
  data = dt_d2,
  family = binomial()
)
res_d2 <- extract_clustered_glm(m_d2, ~ recalling_firm, "Sensitivity 2: Class I vs Class III")

## D3: one recall per firm per year
dt_d3 <- dt2[order(recalling_firm, year)]
dt_d3 <- dt_d3[, .SD[1], by = .(recalling_firm, year)]

m_d3 <- glm(
  class_binary ~ reason_cat + product_cat + dist_cat + region_final + year,
  data = dt_d3,
  family = binomial()
)
res_d3 <- extract_clustered_glm(m_d3, ~ recalling_firm, "Sensitivity 3: One recall per firm per year")

## Main model for comparison
res_main <- extract_clustered_glm(fitA, ~ recalling_firm, "Main model")

resD <- rbindlist(list(res_main, res_d1, res_d2, res_d3), fill = TRUE)
fwrite(resD, "Table_D1_sensitivity_compare_v4.csv", bom = TRUE)

cat("Saved: Table_D1_sensitivity_compare_v4.csv\n")
cat("\nDONE.\n")