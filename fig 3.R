## =========================================================
## Sensitivity analyses + compact manuscript table
## Start from:
##   enforcement_2012_2024_categorized_v4.csv
##
## Outputs:
##   1) Table_S4_LogReg_MainModel_ClassI_v4.csv
##   2) model_sensitivity_I_vs_II_clustered_OR.csv
##   3) model_sensitivity_I_vs_III_clustered_OR.csv
##   4) model_one_recall_per_firm_per_year_clustered_OR.csv
##   5) Table_Sensitivity_compact_for_manuscript.csv
##
## Workdir:
##   D:/桌面临时文件/已经发表论文/慧玲论文/食品大数据 FDA/转换/回修跑代码
## =========================================================

library(data.table)
library(dplyr)
library(stringr)
library(sandwich)
library(lmtest)

setwd("D:/桌面临时文件/已经发表论文/慧玲论文/食品大数据 FDA/转换/回修跑代码")

## =========================================================
## 1) Read data
## =========================================================
infile <- "enforcement_2012_2024_categorized_v4.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")

## =========================================================
## 2) Basic cleaning
## =========================================================
need_cols <- c(
  "year", "recall_number", "recalling_firm",
  "reason_cat", "product_cat", "dist_cat", "region_final",
  "class_binary", "class_I_vs_II", "class_I_vs_III"
)
miss_cols <- setdiff(need_cols, names(dt))
if(length(miss_cols) > 0){
  stop(paste("Missing columns:", paste(miss_cols, collapse = ", ")))
}

dt[, year := as.integer(year)]
dt <- dt[!is.na(year) & year >= 2012 & year <= 2024]

# 文本标准化
for(v in c("reason_cat", "product_cat", "dist_cat", "region_final", "recalling_firm", "recall_number")){
  dt[, (v) := str_squish(as.character(get(v)))]
}

# 缺失兜底
dt[is.na(reason_cat) | reason_cat == "", reason_cat := "Other / Unspecified"]
dt[is.na(product_cat) | product_cat == "", product_cat := "Other food"]
dt[is.na(dist_cat) | dist_cat == "", dist_cat := "Unknown/Other"]
dt[is.na(region_final) | region_final == "", region_final := "Unknown"]
dt[is.na(recalling_firm) | recalling_firm == "", recalling_firm := "Unknown firm"]

# firm 标准化（聚类更稳）
dt[, recalling_firm := str_to_lower(recalling_firm)]
dt[, recalling_firm := str_squish(recalling_firm)]

# outcome 转数值
dt[, class_binary := as.integer(class_binary)]
dt[, class_I_vs_II := as.integer(class_I_vs_II)]
dt[, class_I_vs_III := as.integer(class_I_vs_III)]

## =========================================================
## 3) Helper: prepare factors
## =========================================================
prepare_model_data <- function(dat, outcome_var){
  dat <- copy(dat)
  dat <- dat[!is.na(get(outcome_var))]
  
  dat[, year := factor(year)]
  dat[, reason_cat := factor(reason_cat)]
  dat[, product_cat := factor(product_cat)]
  dat[, dist_cat := factor(dist_cat)]
  dat[, region_final := factor(region_final)]
  
  # 安全参考组
  reason_ref <- if("Undeclared allergen" %in% levels(dat$reason_cat)) "Undeclared allergen" else levels(dat$reason_cat)[1]
  product_ref <- if("Ready-to-eat / Prepared foods" %in% levels(dat$product_cat)) "Ready-to-eat / Prepared foods" else levels(dat$product_cat)[1]
  dist_ref <- if("Single state" %in% levels(dat$dist_cat)) "Single state" else levels(dat$dist_cat)[1]
  region_ref <- if("Midwest" %in% levels(dat$region_final)) "Midwest" else levels(dat$region_final)[1]
  year_ref <- if("2012" %in% levels(dat$year)) "2012" else levels(dat$year)[1]
  
  dat[, reason_cat := relevel(reason_cat, ref = reason_ref)]
  dat[, product_cat := relevel(product_cat, ref = product_ref)]
  dat[, dist_cat := relevel(dist_cat, ref = dist_ref)]
  dat[, region_final := relevel(region_final, ref = region_ref)]
  dat[, year := relevel(year, ref = year_ref)]
  
  return(dat)
}

## =========================================================
## 4) Helper: clustered logistic regression
## =========================================================
run_clustered_model <- function(dat, outcome_var, scenario_name){
  dat2 <- prepare_model_data(dat, outcome_var)
  
  form <- as.formula(
    paste0(outcome_var, " ~ year + reason_cat + product_cat + dist_cat + region_final")
  )
  
  fit <- glm(form, data = dat2, family = binomial())
  vc <- vcovCL(fit, cluster = ~ recalling_firm)
  
  coef_est <- coef(fit)
  se_robust <- sqrt(diag(vc))
  
  out <- data.table(
    Scenario = scenario_name,
    term = names(coef_est),
    estimate = as.numeric(coef_est),
    se_robust = as.numeric(se_robust)
  )
  
  out <- out[term != "(Intercept)"]
  
  out[, OR := exp(estimate)]
  out[, CI_low := exp(estimate - 1.96 * se_robust)]
  out[, CI_high := exp(estimate + 1.96 * se_robust)]
  out[, z := estimate / se_robust]
  out[, P_value := 2 * pnorm(abs(z), lower.tail = FALSE)]
  
  return(out)
}

## =========================================================
## 5) Main model
## =========================================================
res_main <- run_clustered_model(
  dat = dt,
  outcome_var = "class_binary",
  scenario_name = "Main model"
)

write.csv(res_main, "Table_S4_LogReg_MainModel_ClassI_v4_raw.csv", row.names = FALSE)

## 为 Figure 6 再生成一个更整洁版本
parse_term <- function(x){
  if(str_detect(x, "^dist_cat")){
    c("Distribution scope", str_remove(x, "^dist_cat"))
  } else if(str_detect(x, "^product_cat")){
    c("Product category", str_remove(x, "^product_cat"))
  } else if(str_detect(x, "^region_final")){
    c("Region", str_remove(x, "^region_final"))
  } else if(str_detect(x, "^year")){
    c("Year", str_remove(x, "^year"))
  } else if(str_detect(x, "^reason_cat")){
    c("Recall reason", str_remove(x, "^reason_cat"))
  } else {
    c("Other", x)
  }
}

parsed_main <- t(sapply(res_main$term, parse_term))
main_fig <- copy(res_main)
main_fig[, `Variable group` := parsed_main[,1]]
main_fig[, Level := parsed_main[,2]]
main_fig[, aOR := OR]
main_fig[, CI_low := CI_low]
main_fig[, CI_high := CI_high]
main_fig <- main_fig[, .(`Variable group`, Level, aOR, CI_low, CI_high, P_value)]

fwrite(main_fig, "Table_S4_LogReg_MainModel_ClassI_v4.csv", bom = TRUE)

## =========================================================
## 6) Sensitivity 1: Class I vs Class II
## =========================================================
res_I_II <- run_clustered_model(
  dat = dt,
  outcome_var = "class_I_vs_II",
  scenario_name = "Sensitivity 1: Class I vs II"
)
write.csv(res_I_II, "model_sensitivity_I_vs_II_clustered_OR.csv", row.names = FALSE)

## =========================================================
## 7) Sensitivity 2: Class I vs Class III
## =========================================================
res_I_III <- run_clustered_model(
  dat = dt,
  outcome_var = "class_I_vs_III",
  scenario_name = "Sensitivity 2: Class I vs III"
)
write.csv(res_I_III, "model_sensitivity_I_vs_III_clustered_OR.csv", row.names = FALSE)

## =========================================================
## 8) Sensitivity 3: one recall per firm per year
## =========================================================
dt_onefirm_year <- dt %>%
  as.data.frame() %>%
  arrange(recalling_firm, year, recall_number) %>%
  group_by(recalling_firm, year) %>%
  slice(1) %>%
  ungroup() %>%
  as.data.table()

res_onefirm <- run_clustered_model(
  dat = dt_onefirm_year,
  outcome_var = "class_binary",
  scenario_name = "Sensitivity 3: One recall per firm per year"
)
write.csv(res_onefirm, "model_one_recall_per_firm_per_year_clustered_OR.csv", row.names = FALSE)

## =========================================================
## 9) Build compact manuscript table
## =========================================================
all_res <- rbindlist(list(res_main, res_I_II, res_I_III, res_onefirm), fill = TRUE)

# 只保留关键变量
key_terms <- c(
  "reason_catBacterial pathogen contamination",
  "reason_catChemical contamination / Toxins / Adulteration",
  "reason_catForeign material",
  "product_catBeverages",
  "product_catGrains / Rice / Pasta / Noodles",
  "product_catNuts / Seeds",
  "dist_catUnknown/Other",
  "region_finalWest"
)

tab_keep <- all_res[term %in% key_terms]

label_map <- c(
  "reason_catBacterial pathogen contamination" = "Bacterial contamination",
  "reason_catChemical contamination / Toxins / Adulteration" = "Chemical contamination",
  "reason_catForeign material" = "Foreign material",
  "product_catBeverages" = "Beverages",
  "product_catGrains / Rice / Pasta / Noodles" = "Grains / Rice",
  "product_catNuts / Seeds" = "Nuts / Seeds",
  "dist_catUnknown/Other" = "Unknown distribution",
  "region_finalWest" = "West region"
)

tab_keep[, Variable := label_map[term]]

tab_keep[, Model := Scenario]

tab_keep[, `aOR (95% CI), P` := sprintf(
  "%.2f (%.2f–%.2f), p=%s",
  OR, CI_low, CI_high,
  fifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))
)]

tab_wide <- dcast(
  tab_keep[, .(Variable, Model, `aOR (95% CI), P`)],
  Variable ~ Model,
  value.var = "aOR (95% CI), P"
)

var_order <- c(
  "Bacterial contamination",
  "Beverages",
  "Chemical contamination",
  "Foreign material",
  "Grains / Rice",
  "Nuts / Seeds",
  "Unknown distribution",
  "West region"
)

tab_wide[, ord := match(Variable, var_order)]
setorder(tab_wide, ord)
tab_wide[, ord := NULL]

fwrite(tab_wide, "Table_Sensitivity_compact_for_manuscript.csv", bom = TRUE)

## =========================================================
## 10) Done
## =========================================================
cat("Saved files:\n")
cat("- Table_S4_LogReg_MainModel_ClassI_v4_raw.csv\n")
cat("- Table_S4_LogReg_MainModel_ClassI_v4.csv\n")
cat("- model_sensitivity_I_vs_II_clustered_OR.csv\n")
cat("- model_sensitivity_I_vs_III_clustered_OR.csv\n")
cat("- model_one_recall_per_firm_per_year_clustered_OR.csv\n")
cat("- Table_Sensitivity_compact_for_manuscript.csv\n")
cat("DONE.\n")
