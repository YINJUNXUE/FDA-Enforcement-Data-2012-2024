## =========================================================
## Table S4 | Main multivariable logistic regression model
## Outcome: Class I vs non-Class I
## Revised version for reviewer comments
##
## Input : enforcement_2012_2024_categorized_v4.csv
## Output: Table_S4_LogReg_MainModel_ClassI_v4.csv
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

infile <- "enforcement_2012_2024_categorized_v4.csv"
if(!file.exists(infile)) stop(paste0("File not found: ", infile))

dt <- fread(infile, encoding = "UTF-8")

## -------------------------
## 1) Required columns
## -------------------------
need_cols <- c(
  "class_binary",
  "year",
  "reason_cat",
  "product_cat",
  "dist_cat",
  "region_final",
  "recalling_firm"
)
miss_cols <- setdiff(need_cols, names(dt))
if(length(miss_cols) > 0){
  stop(paste("Missing columns:", paste(miss_cols, collapse = ", ")))
}

## -------------------------
## 2) Clean
## -------------------------
dt[, year := as.integer(year)]
dt <- dt[!is.na(year) & year >= 2012 & year <= 2024]

dt[, reason_cat := str_squish(as.character(reason_cat))]
dt[, product_cat := str_squish(as.character(product_cat))]
dt[, dist_cat := str_squish(as.character(dist_cat))]
dt[, region_final := str_squish(as.character(region_final))]
dt[, recalling_firm := str_squish(as.character(recalling_firm))]

dt[is.na(reason_cat) | reason_cat == "", reason_cat := "Other / Unspecified"]
dt[is.na(product_cat) | product_cat == "", product_cat := "Other food"]
dt[is.na(dist_cat) | dist_cat == "", dist_cat := "Unknown/Other"]
dt[is.na(region_final) | region_final == "", region_final := "Unknown"]
dt[is.na(recalling_firm) | recalling_firm == "", recalling_firm := "Unknown firm"]

## outcome
dt <- dt[!is.na(class_binary)]
dt[, class_binary := as.integer(class_binary)]

## -------------------------
## 3) Factor levels
## -------------------------
# reason: 以 Undeclared allergen 为参考（便于解释）
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

# product：建议以 Ready-to-eat / Prepared foods 为参考；若不存在则自动选第一个
prod_ref <- "Ready-to-eat / Prepared foods"
if(!(prod_ref %in% unique(dt$product_cat))){
  prod_ref <- sort(unique(dt$product_cat))[1]
}

# dist：以 Single state 为参考
dist_ref <- "Single state"
if(!(dist_ref %in% unique(dt$dist_cat))){
  dist_ref <- sort(unique(dt$dist_cat))[1]
}

# region：以 Midwest 为参考
region_ref <- "Midwest"
if(!(region_ref %in% unique(dt$region_final))){
  region_ref <- sort(unique(dt$region_final))[1]
}

dt2 <- dt %>%
  as.data.frame() %>%
  mutate(
    reason_cat   = factor(reason_cat, levels = reason_order),
    product_cat  = factor(product_cat),
    dist_cat     = factor(dist_cat),
    region_final = factor(region_final),
    year         = factor(year)
  )

dt2$reason_cat   <- relevel(dt2$reason_cat, ref = "Undeclared allergen")
dt2$product_cat  <- relevel(dt2$product_cat, ref = prod_ref)
dt2$dist_cat     <- relevel(dt2$dist_cat, ref = dist_ref)
dt2$region_final <- relevel(dt2$region_final, ref = region_ref)
dt2$year         <- relevel(dt2$year, ref = "2012")

## -------------------------
## 4) Main model
## -------------------------
fit_main <- glm(
  class_binary ~ reason_cat + product_cat + dist_cat + region_final + year,
  data = dt2,
  family = binomial()
)

## firm-level clustered robust SE
vcov_firm <- vcovCL(fit_main, cluster = ~ recalling_firm)
coefs <- coef(fit_main)
ses <- sqrt(diag(vcov_firm))

res <- data.frame(
  term = names(coefs),
  beta = unname(coefs),
  se = unname(ses),
  stringsAsFactors = FALSE
)

res <- res %>%
  mutate(
    z = beta / se,
    P_value = 2 * pnorm(abs(z), lower.tail = FALSE),
    aOR = exp(beta),
    CI_low = exp(beta - 1.96 * se),
    CI_high = exp(beta + 1.96 * se)
  )

## 去掉截距
res <- res %>% filter(term != "(Intercept)")

## -------------------------
## 5) Parse terms into Variable group / Level
## -------------------------
parse_term <- function(x){
  if(str_detect(x, "^dist_cat")){
    return(c("Distribution scope", str_remove(x, "^dist_cat")))
  } else if(str_detect(x, "^product_cat")){
    return(c("Product category", str_remove(x, "^product_cat")))
  } else if(str_detect(x, "^region_final")){
    return(c("Region", str_remove(x, "^region_final")))
  } else if(str_detect(x, "^year")){
    return(c("Year", str_remove(x, "^year")))
  } else if(str_detect(x, "^reason_cat")){
    return(c("Recall reason", str_remove(x, "^reason_cat")))
  } else {
    return(c("Other", x))
  }
}

parsed <- t(sapply(res$term, parse_term))
res$`Variable group` <- parsed[,1]
res$Level <- parsed[,2]

## -------------------------
## 6) Clean level labels
## -------------------------
res$Level <- str_replace_all(res$Level, "_", " ")
res$Level <- str_squish(res$Level)

## 让 year 显示成纯年份
res$Level[res$`Variable group` == "Year"] <- str_extract(res$Level[res$`Variable group` == "Year"], "\\d{4}")

## -------------------------
## 7) Ordering
## -------------------------
# Distribution scope
dist_order <- c("International", "Multi-state", "Nationwide", "Unknown/Other")
dist_order <- dist_order[dist_order %in% res$Level[res$`Variable group` == "Distribution scope"]]

# Product category：按OR从大到小
prod_levels <- res %>%
  filter(`Variable group` == "Product category") %>%
  arrange(desc(aOR)) %>%
  pull(Level)

# Region：固定顺序
region_order <- c("Northeast", "South", "West", "Unknown")
region_order <- region_order[region_order %in% res$Level[res$`Variable group` == "Region"]]

# Year：自然顺序
year_order <- res %>%
  filter(`Variable group` == "Year") %>%
  mutate(year_num = as.integer(Level)) %>%
  arrange(year_num) %>%
  pull(Level)

# Recall reason：按预定义顺序（去掉参考组）
reason_plot_order <- setdiff(reason_order, "Undeclared allergen")
reason_plot_order <- reason_plot_order[reason_plot_order %in% res$Level[res$`Variable group` == "Recall reason"]]

## 为每组生成 order
res$order_id <- NA_real_

res$order_id[res$`Variable group` == "Distribution scope"] <-
  match(res$Level[res$`Variable group` == "Distribution scope"], dist_order)

res$order_id[res$`Variable group` == "Product category"] <-
  match(res$Level[res$`Variable group` == "Product category"], prod_levels)

res$order_id[res$`Variable group` == "Region"] <-
  match(res$Level[res$`Variable group` == "Region"], region_order)

res$order_id[res$`Variable group` == "Year"] <-
  match(res$Level[res$`Variable group` == "Year"], year_order)

res$order_id[res$`Variable group` == "Recall reason"] <-
  match(res$Level[res$`Variable group` == "Recall reason"], reason_plot_order)

## 组内排序
group_order <- c("Distribution scope", "Product category", "Region", "Year", "Recall reason")
res$group_id <- match(res$`Variable group`, group_order)

res <- res %>%
  arrange(group_id, order_id)

## -------------------------
## 8) Final export
## -------------------------
out <- res %>%
  select(`Variable group`, Level, aOR, CI_low, CI_high, P_value)

fwrite(out, "Table_S4_LogReg_MainModel_ClassI_v4.csv", bom = TRUE)

cat("Saved: Table_S4_LogReg_MainModel_ClassI_v4.csv\n")
cat("Reference levels used:\n")
cat("- Recall reason:", "Undeclared allergen", "\n")
cat("- Product category:", prod_ref, "\n")
cat("- Distribution scope:", dist_ref, "\n")
cat("- Region:", region_ref, "\n")
cat("- Year:", "2012", "\n")
cat("DONE.\n")
