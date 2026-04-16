# ============================================================
# POLAND YOUTH CLIMATE ATTITUDES — CLEAN REPLICATION + ROBUSTNESS
# Rebuild from scratch for thesis analysis and appendix checks
# ============================================================

# -----------------------------
# 0) Packages
# -----------------------------
required_pkgs <- c(
  "readr", "dplyr", "stringr", "forcats", "purrr", "tidyr",
  "broom", "broom.helpers", "ggplot2", "ggeffects",
  "lmtest", "sandwich", "car", "performance",
  "MASS", "nnet", "modelsummary"
)

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Install missing packages first: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(forcats)
  library(purrr)
  library(tidyr)
  library(broom)
  library(ggplot2)
  library(ggeffects)
  library(lmtest)
  library(sandwich)
  library(car)
  library(performance)
  library(MASS)
  library(nnet)
  library(modelsummary)
})

options(contrasts = c("contr.treatment", "contr.poly"))

# -----------------------------
# 1) Read and clean base dataset
# -----------------------------
# Change path if needed
pl_path <- "pl.csv"

PL_raw <- readr::read_csv(pl_path, show_col_types = FALSE)

PL <- PL_raw %>%
  filter(!str_detect(as.character(Q2.2), "ImportId")) %>%
  filter(as.character(Q2.2) != "How old are you?") %>%
  mutate(
    # treatments
    treatment_climate = suppressWarnings(as.integer(as.character(treatment_climate))),
    treatment_policy  = suppressWarnings(as.integer(as.character(treatment_policy))),
    treatment_group = case_when(
      treatment_climate == 0 & treatment_policy == 0 ~ "Control",
      treatment_climate == 1 & treatment_policy == 0 ~ "Climate",
      treatment_climate == 0 & treatment_policy == 1 ~ "Policy",
      treatment_climate == 1 & treatment_policy == 1 ~ "Both",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Control", "Climate", "Policy", "Both")),

    # demographics
    gender = case_when(
      Q2.1 %in% c("Female", "Male") ~ Q2.1,
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Female", "Male")),

    age_cat = case_when(
      Q2.2 %in% c("18 to 24", "25 to 34", "35 to 54", "55+") ~ as.character(Q2.2),
      TRUE ~ NA_character_
    ) %>% factor(levels = c("18 to 24", "25 to 34", "35 to 54", "55+")),

    YOUTH = if_else(age_cat %in% c("18 to 24", "25 to 34"), 1L, 0L),

    urban = case_when(
      Q2.5 == "A rural area" ~ "Rural",
      Q2.5 %in% c(
        "A small town (5,000 – 20,000 inhabitants)",
        "A large town (20,000 – 50,000 inhabitants)",
        "A small city (50,000 – 250,000 inhabitants)",
        "A large city (250,000 – 3,000,000 inhabitants)",
        "A very large city (more than 3 million inhabitants)"
      ) ~ "Urban",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Rural", "Urban")),

    education = case_when(
      Q2.8 %in% c("No schooling completed", "Primary school", "Lower secondary school") ~ "Low",
      Q2.8 %in% c("High school", "Vocational degree") ~ "Medium",
      Q2.8 %in% c("College degree", "Master's degree or above") ~ "High",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Low", "Medium", "High")),

    voted_cat = case_when(
      Q23.4 == "Yes" ~ "Voted",
      Q23.4 == "No" ~ "Did not vote",
      Q23.4 %in% c("Prefer not to say", "I don't know") ~ "Missing/Refused",
      Q23.4 == "I don't have the right to vote in [Country]" ~ NA_character_,
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Did not vote", "Voted", "Missing/Refused")),

    ideology_lr = suppressWarnings(as.numeric(as.character(Q24.5_7))),
    ideology_c = ideology_lr - mean(ideology_lr, na.rm = TRUE),

    trust_gov = case_when(
      Q22.2 == "Strongly disagree" ~ 1,
      Q22.2 == "Somewhat disagree" ~ 2,
      Q22.2 == "Neither agree nor disagree" ~ 3,
      Q22.2 == "Somewhat agree" ~ 4,
      Q22.2 == "Strongly agree" ~ 5,
      TRUE ~ NA_real_
    ),

    # dependent variables used in the paper
    climate_importance = case_when(
      Q13.4 == "Strongly disagree" ~ 1,
      Q13.4 == "Somewhat disagree" ~ 2,
      Q13.4 == "Neither agree nor disagree" ~ 3,
      Q13.4 == "Somewhat agree" ~ 4,
      Q13.4 == "Strongly agree" ~ 5,
      TRUE ~ NA_real_
    ),

    policy_general = case_when(
      Q20.3 == "Strongly disagree" ~ 1,
      Q20.3 == "Somewhat disagree" ~ 2,
      Q20.3 == "Neither agree nor disagree" ~ 3,
      Q20.3 == "Somewhat agree" ~ 4,
      Q20.3 == "Strongly agree" ~ 5,
      TRUE ~ NA_real_
    ),

    policy_carbon_tax = case_when(
      Q17.6 == "Strongly oppose" ~ 1,
      Q17.6 == "Somewhat oppose" ~ 2,
      Q17.6 == "Neither support nor oppose" ~ 3,
      Q17.6 == "Somewhat support" ~ 4,
      Q17.6 == "Strongly support" ~ 5,
      TRUE ~ NA_real_
    ),

    policy_engine = case_when(
      Q15.5 == "Strongly oppose" ~ 1,
      Q15.5 == "Somewhat oppose" ~ 2,
      Q15.5 == "Neither support nor oppose" ~ 3,
      Q15.5 == "Somewhat support" ~ 4,
      Q15.5 == "Strongly support" ~ 5,
      TRUE ~ NA_real_
    ),

    policy_green_infra = case_when(
      Q16.5 == "Strongly support" ~ 5,
      Q16.5 == "Somewhat support" ~ 4,
      Q16.5 == "Neither support nor oppose" ~ 3,
      Q16.5 == "Somewhat oppose" ~ 2,
      Q16.5 == "Strongly oppose" ~ 1,
      TRUE ~ NA_real_
    )
  )

# -----------------------------
# 2) Youth sample used in the paper
# -----------------------------
PL_YOUTH <- PL %>%
  filter(YOUTH == 1) %>%
  mutate(
    gender = relevel(gender, ref = "Female"),
    treatment_group = relevel(treatment_group, ref = "Control")
  )

cat("Youth sample size:", nrow(PL_YOUTH), "\n")
cat("Treatment distribution:\n")
print(table(PL_YOUTH$treatment_group, useNA = "ifany"))

# -----------------------------
# 3) Outcomes and common model formula
# -----------------------------
outcomes <- c(
  "climate_importance",
  "policy_general",
  "policy_carbon_tax",
  "policy_engine",
  "policy_green_infra"
)

outcome_labels <- c(
  climate_importance = "Climate importance",
  policy_general = "General policy support",
  policy_carbon_tax = "Carbon tax support",
  policy_engine = "Combustion-engine ban support",
  policy_green_infra = "Green infrastructure support"
)

rhs_main <- "treatment_group + gender * ideology_c + urban + education + voted_cat + trust_gov"

# complete-case per outcome
make_analysis_df <- function(data, outcome) {
  needed <- c(outcome, "treatment_group", "gender", "ideology_c", "urban", "education", "voted_cat", "trust_gov")
  
  data %>%
    filter(
      if_all(all_of(needed), ~ !is.na(.)),
      voted_cat != "Missing/Refused"   # ← ADD THIS LINE
    )
}

# -----------------------------
# 4) OLS main models
# -----------------------------
ols_models <- setNames(vector("list", length(outcomes)), outcomes)
ols_data   <- setNames(vector("list", length(outcomes)), outcomes)

for (y in outcomes) {
  dat_y <- make_analysis_df(PL_YOUTH, y)
  form_y <- as.formula(paste(y, "~", rhs_main))
  ols_data[[y]] <- dat_y
  ols_models[[y]] <- lm(form_y, data = dat_y)
}

cat("\nOLS model Ns:\n")
print(sapply(ols_models, nobs))

# Robust HC3 SEs
ols_vcov_hc3 <- lapply(ols_models, sandwich::vcovHC, type = "HC3")
ols_se_hc3   <- lapply(ols_vcov_hc3, function(v) sqrt(diag(v)))

# export main OLS table - classical OLS SEs
modelsummary(
  ols_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  output = "ols_main_models_classical.html",
  gof_omit = "AIC|BIC|Log.Lik|RMSE|F",
  title = "Multivariate Analysis of Climate Attitudes (Polish Youth 18–34)"
)

# -----------------------------
# -----------------------------
# 5) OLS diagnostics
# -----------------------------
extract_max_vif <- function(m) {
  vif_raw <- tryCatch(car::vif(m, type = "predictor"), error = function(e) NULL)
  
  if (is.null(vif_raw)) return(NA_real_)
  
  # case 1: matrix output with comparable GVIF column
  if (is.matrix(vif_raw)) {
    if ("GVIF^(1/(2*Df))" %in% colnames(vif_raw)) {
      return(max(vif_raw[, "GVIF^(1/(2*Df))"], na.rm = TRUE))
    } else {
      return(max(as.numeric(vif_raw), na.rm = TRUE))
    }
  }
  
  # case 2: data.frame output
  if (is.data.frame(vif_raw)) {
    if ("GVIF^(1/(2*Df))" %in% names(vif_raw)) {
      return(max(vif_raw[["GVIF^(1/(2*Df))"]], na.rm = TRUE))
    } else {
      num_cols <- vif_raw[, vapply(vif_raw, is.numeric, logical(1)), drop = FALSE]
      if (ncol(num_cols) == 0) return(NA_real_)
      return(max(unlist(num_cols, use.names = FALSE), na.rm = TRUE))
    }
  }
  
  # case 3: named numeric vector
  if (is.numeric(vif_raw)) {
    return(max(vif_raw, na.rm = TRUE))
  }
  
  NA_real_
}

ols_diagnostics <- purrr::map_df(names(ols_models), function(y) {
  m <- ols_models[[y]]
  
  bp_p <- tryCatch(lmtest::bptest(m)$p.value, error = function(e) NA_real_)
  reset_p <- tryCatch(lmtest::resettest(m, power = 2:3, type = "fitted")$p.value, error = function(e) NA_real_)
  
  cd <- cooks.distance(m)
  lev <- hatvalues(m)
  rs <- rstudent(m)
  n <- nobs(m)
  p <- length(coef(m))
  
  tibble::tibble(
    outcome = y,
    n = n,
    max_vif = extract_max_vif(m),
    bp_p = bp_p,
    reset_p = reset_p,
    max_cooks_d = max(cd, na.rm = TRUE),
    cooks_d_over_4n = sum(cd > 4 / n, na.rm = TRUE),
    max_leverage = max(lev, na.rm = TRUE),
    leverage_over_2p_n = sum(lev > 2 * p / n, na.rm = TRUE),
    abs_rstudent_over_3 = sum(abs(rs) > 3, na.rm = TRUE)
  )
})

modelsummary::datasummary_df(
  ols_diagnostics,
  output = "ols_diagnostics_summary.html",
  title = "OLS diagnostics summary"
)
# -----------------------------
# 6) Ordered logit robustness
# -----------------------------
ordered_models <- setNames(vector("list", length(outcomes)), outcomes)
ordered_tidy   <- setNames(vector("list", length(outcomes)), outcomes)

for (y in outcomes) {
  dat_y <- ols_data[[y]] %>%
    mutate(y_ord = ordered(.data[[y]], levels = 1:5))

  form_y <- as.formula(paste("y_ord ~", rhs_main))
  fit <- MASS::polr(form_y, data = dat_y, Hess = TRUE, method = "logistic")
  ordered_models[[y]] <- fit

  coef_tab <- coef(summary(fit))
  pvals <- 2 * (1 - pnorm(abs(coef_tab[, "t value"])))
  ordered_tidy[[y]] <- tibble(
    outcome = y,
    term = rownames(coef_tab),
    estimate = coef_tab[, "Value"],
    std.error = coef_tab[, "Std. Error"],
    statistic = coef_tab[, "t value"],
    p.value = pvals
  )
}

ordered_results <- bind_rows(ordered_tidy)

modelsummary::datasummary_df(
  ordered_results,
  output = "ordered_logit_results.html",
  title = "Ordered logit robustness results"
)

# -----------------------------
# 7) Binary logistic robustness
#    Main binary recode: support (4-5) vs other (1-3)
# -----------------------------
make_binary_support <- function(x) {
  case_when(
    x %in% c(4, 5) ~ 1,
    x %in% c(1, 2, 3) ~ 0,
    TRUE ~ NA_real_
  )
}

logit_models <- setNames(vector("list", length(outcomes)), outcomes)
logit_tidy   <- setNames(vector("list", length(outcomes)), outcomes)

for (y in outcomes) {
  dat_y <- ols_data[[y]] %>%
    mutate(y_bin = make_binary_support(.data[[y]]))

  fit <- glm(
    as.formula(paste("y_bin ~", rhs_main)),
    data = dat_y,
    family = binomial(link = "logit")
  )

  logit_models[[y]] <- fit

  logit_tidy[[y]] <- broom::tidy(
    fit,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    mutate(outcome = y)
}

logit_results_or <- bind_rows(logit_tidy)

modelsummary::datasummary_df(
  logit_results_or,
  output = "binary_logit_odds_ratios.html",
  title = "Binary logistic robustness results (odds ratios)"
)

# optional stricter binary recode for appendix: oppose (1-2) vs rest (3-5)
make_binary_oppose <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,
    x %in% c(3, 4, 5) ~ 0,
    TRUE ~ NA_real_
  )
}

# -----------------------------
# 8) Multinomial robustness
#    Recode to 3-category nominal variable:
#    Oppose (1-2), Neutral (3), Support (4-5)
#    NOTE: this ignores ordinality; keep as appendix robustness only.
# -----------------------------
make_three_cat <- function(x) {
  case_when(
    x %in% c(1, 2) ~ "Oppose",
    x == 3 ~ "Neutral",
    x %in% c(4, 5) ~ "Support",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Neutral", "Oppose", "Support"))
}

multinom_models <- setNames(vector("list", length(outcomes)), outcomes)
multinom_tidy   <- setNames(vector("list", length(outcomes)), outcomes)

for (y in outcomes) {
  dat_y <- ols_data[[y]] %>%
    mutate(y3 = make_three_cat(.data[[y]])) %>%
    filter(!is.na(y3))

  fit <- nnet::multinom(
    as.formula(paste("y3 ~", rhs_main)),
    data = dat_y,
    trace = FALSE
  )

  multinom_models[[y]] <- fit

  s <- summary(fit)
  zvals <- s$coefficients / s$standard.errors
  pvals <- 2 * (1 - pnorm(abs(zvals)))

  multinom_tidy[[y]] <- as_tibble(as.data.frame(as.table(s$coefficients))) %>%
    rename(response = Var1, term = Var2, estimate = Freq) %>%
    mutate(
      std.error = as.vector(s$standard.errors),
      statistic = as.vector(zvals),
      p.value = as.vector(pvals),
      outcome = y,
      odds_ratio = exp(estimate)
    )
}

multinom_results <- bind_rows(multinom_tidy)

modelsummary::datasummary_df(
  multinom_results,
  output = "multinomial_results.html",
  title = "Multinomial robustness results"
)

# -----------------------------
# 9) Political-specification robustness
#    ideology only vs vote only vs both
# -----------------------------
political_specs <- list(
  ideology_only = "treatment_group + gender * ideology_c + urban + education + trust_gov",
  vote_only     = "treatment_group + gender + urban + education + voted_cat + trust_gov",
  both          = "treatment_group + gender * ideology_c + urban + education + voted_cat + trust_gov"
)

political_models <- list()
political_rows <- list()

for (y in outcomes) {
  for (spec_name in names(political_specs)) {
    rhs <- political_specs[[spec_name]]
    vars_needed <- all.vars(as.formula(paste(y, "~", rhs)))
    dat_y <- PL_YOUTH %>% filter(if_all(all_of(vars_needed), ~ !is.na(.)))
    fit <- lm(as.formula(paste(y, "~", rhs)), data = dat_y)
    political_models[[paste(y, spec_name, sep = "__")]] <- fit

    political_rows[[paste(y, spec_name, sep = "__")]] <- broom::tidy(
      lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = "HC3"))
    ) %>%
      mutate(
        outcome = y,
        specification = spec_name,
        n = nobs(fit)
      )
  }
}

political_results <- bind_rows(political_rows)

modelsummary::datasummary_df(
  political_results,
  output = "political_specification_robustness.html",
  title = "Political-specification robustness results"
)

political_results <- bind_rows(political_rows)

# -----------------------------
# 10) Marginal effects / predicted values for paper figures
# -----------------------------
# Example: policy_engine
if ("policy_engine" %in% names(ols_models)) {
  pred_engine <- ggeffects::ggpredict(
    ols_models[["policy_engine"]],
    terms = c("ideology_c [quart]", "gender")
  )
  pred_engine_df <- as.data.frame(pred_engine)
  
  modelsummary::datasummary_df(
    pred_engine_df,
    output = "predicted_policy_engine_by_gender_ideology.html",
    title = "Predicted support for combustion-engine ban by ideology and gender"
  )

  p_engine <- ggplot(as.data.frame(pred_engine), aes(x = x, y = predicted, color = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
    labs(
      x = "Ideology (centered)",
      y = "Predicted support",
      color = "Gender",
      fill = "Gender",
      title = "Predicted support for combustion-engine ban by ideology and gender"
    ) +
    theme_minimal()

  ggsave("predicted_policy_engine_by_gender_ideology.png", p_engine, width = 8, height = 5, dpi = 300)
}

# -----------------------------
# 11) Compact comparison object for appendix write-up
# -----------------------------
appendix_summary <- tibble(
  outcome = outcomes,
  ols_n = map_int(ols_models, nobs),
  ordered_n = map_int(ordered_models, nobs),
  logit_n = map_int(logit_models, nobs),
  multinom_n = map_int(multinom_models, nobs)
)

modelsummary::datasummary_df(
  appendix_summary,
  output = "appendix_model_sample_sizes.html",
  title = "Sample sizes across model specifications"
)

cat("\nDone. Files written to working directory:\n")
cat("- ols_main_models_classical.html\n")
cat("- ols_main_models_hc3.html\n")
cat("- ols_diagnostics_summary.html\n")
cat("- ordered_logit_results.html\n")
cat("- binary_logit_odds_ratios.html\n")
cat("- multinomial_results.html\n")
cat("- political_specification_robustness.html\n")
cat("- appendix_model_sample_sizes.html\n")
cat("- predicted_policy_engine_by_gender_ideology.html\n")
cat("- predicted_policy_engine_by_gender_ideology.png\n")

# -----------------------------
# 10) Marginal effects / predicted values for paper figures
# -----------------------------

# Consistent colors across all plots
female_col <- "#6A3D9A"   # purple
male_col   <- "#E69F00"   # gold

# Common plotting function
make_pred_plot <- function(model, title_text, file_stub) {
  pred_df <- ggeffects::ggpredict(
    model,
    terms = c("ideology_c [-2,-1,0,1,2]", "gender")
  ) %>%
    as.data.frame() %>%
    mutate(
      group = factor(group, levels = c("Female", "Male"))
    )
  
  p <- ggplot(pred_df, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high, fill = group),
      alpha = 0.15,
      color = NA
    ) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.4) +
    scale_color_manual(values = c("Female" = female_col, "Male" = male_col)) +
    scale_fill_manual(values = c("Female" = female_col, "Male" = male_col)) +
    labs(
      title = title_text,
      subtitle = "Predicted Support by Gender and Political Ideology",
      x = "Ideology (Left to Right)",
      y = "Support (1–5)",
      color = "Gender",
      fill = "Gender"
    ) +
    coord_cartesian(ylim = c(1, 5)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_line(linewidth = 0.2)
    )
  
  # Save plot
  ggsave(
    filename = paste0(file_stub, ".png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  # Save underlying predictions as HTML table
  modelsummary::datasummary_df(
    pred_df,
    output = paste0(file_stub, ".html"),
    title = paste(title_text, "— Predicted Values")
  )
  
  invisible(list(data = pred_df, plot = p))
}

# Generate all five plots
plot_climate_importance <- make_pred_plot(
  model = ols_models[["climate_importance"]],
  title_text = "Outcome: Climate Importance",
  file_stub = "predicted_climate_importance_by_gender_ideology"
)

plot_policy_general <- make_pred_plot(
  model = ols_models[["policy_general"]],
  title_text = "Outcome: General Climate Policy Support",
  file_stub = "predicted_policy_general_by_gender_ideology"
)

plot_policy_carbon_tax <- make_pred_plot(
  model = ols_models[["policy_carbon_tax"]],
  title_text = "Policy: Carbon Tax",
  file_stub = "predicted_policy_carbon_tax_by_gender_ideology"
)

plot_policy_engine <- make_pred_plot(
  model = ols_models[["policy_engine"]],
  title_text = "Policy: Combustion Engine Ban",
  file_stub = "predicted_policy_engine_by_gender_ideology"
)

plot_policy_green_infra <- make_pred_plot(
  model = ols_models[["policy_green_infra"]],
  title_text = "Policy: Green Infrastructure",
  file_stub = "predicted_policy_green_infra_by_gender_ideology"
)

##theme classic
# -----------------------------
# 10) Marginal effects / predicted values for paper figures
# -----------------------------

female_col <- "#6A3D9A"   # purple
male_col   <- "#E69F00"   # gold

make_pred_plot <- function(model, title_text, file_stub) {
  pred_df <- ggeffects::ggpredict(
    model,
    terms = c("ideology_c [-2,-1,0,1,2]", "gender")
  ) %>%
    as.data.frame() %>%
    mutate(group = factor(group, levels = c("Female", "Male")))
  
  p <- ggplot(pred_df, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high, fill = group),
      alpha = 0.15,
      color = NA
    ) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.3) +
    
    scale_color_manual(values = c("Female" = female_col, "Male" = male_col)) +
    scale_fill_manual(values = c("Female" = female_col, "Male" = male_col)) +
    
    labs(
      title = title_text,
      subtitle = "Predicted Support by Gender and Political Ideology",
      x = "Ideology (Left to Right)",
      y = "Support (1–5)",
      color = "Gender",
      fill = "Gender"
    ) +
    
    coord_cartesian(ylim = c(1, 5)) +
    
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  ggsave(
    filename = paste0(file_stub, ".png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  modelsummary::datasummary_df(
    pred_df,
    output = paste0(file_stub, ".html"),
    title = paste(title_text, "— Predicted Values")
  )
  
  invisible(p)
}
make_pred_plot(
  ols_models[["climate_importance"]],
  "Outcome: Climate Importance",
  "predicted_climate_importance_by_gender_ideology"
)

make_pred_plot(
  ols_models[["policy_general"]],
  "Outcome: General Climate Policy Support",
  "predicted_policy_general_by_gender_ideology"
)

make_pred_plot(
  ols_models[["policy_carbon_tax"]],
  "Policy: Carbon Tax",
  "predicted_policy_carbon_tax_by_gender_ideology"
)

make_pred_plot(
  ols_models[["policy_engine"]],
  "Policy: Combustion Engine Ban",
  "predicted_policy_engine_by_gender_ideology"
)

make_pred_plot(
  ols_models[["policy_green_infra"]],
  "Policy: Green Infrastructure",
  "predicted_policy_green_infra_by_gender_ideology"
)

##ANNEX binary
logit_clean <- logit_results_or %>%
  dplyr::filter(term %in% c(
    "genderMale",
    "ideology_c",
    "genderMale:ideology_c",
    "trust_gov"
  ))

modelsummary::datasummary_df(
  logit_clean,
  output = "appendix_binary_logit_clean.html",
  title = "Appendix Table A2. Binary logistic robustness (key coefficients, odds ratios)"
)

##ANNEXLOGIT
ordered_clean <- ordered_results %>%
  dplyr::filter(term %in% c(
    "genderMale",
    "ideology_c",
    "genderMale:ideology_c",
    "trust_gov"
  ))

modelsummary::datasummary_df(
  ordered_clean,
  output = "appendix_ordered_logit_clean.html",
  title = "Appendix Table A1. Ordered logit robustness (key coefficients)"
)

##ANNEXMULTINOMIAL
multinom_clean <- multinom_results %>%
  dplyr::filter(
    term %in% c("genderMale", "ideology_c", "genderMale:ideology_c"),
    !is.na(estimate)
  )

modelsummary::datasummary_df(
  multinom_clean,
  output = "appendix_multinomial_clean.html",
  title = "Appendix Table A3. Multinomial robustness (key coefficients)"
)

#POLITICALSPECSROBUSTNESS
political_clean <- political_results %>%
  dplyr::filter(term %in% c(
    "genderMale",
    "ideology_c",
    "genderMale:ideology_c",
    "trust_gov",
    "voted_catVoted"
  )) %>%
  dplyr::select(
    outcome,
    specification,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    n
  ) %>%
  dplyr::arrange(outcome, specification, term)

modelsummary::datasummary_df(
  political_clean,
  output = "appendix_political_spec_clean.html",
  title = "Appendix Table C1. Political-specification robustness (key coefficients only)"
)

##DESCRIPTIVEPLOT
## DESCRIPTIVE PLOT

# Descriptive sample used for Table 1 logic:
# youth only, valid gender, exclude missing/refused voting
desc_sample <- PL_YOUTH %>%
  filter(
    !is.na(gender),
    !is.na(voted_cat),
    voted_cat != "Missing/Refused"
  )

desc_long <- desc_sample %>%
  pivot_longer(
    cols = c(
      climate_importance,
      policy_general,
      policy_carbon_tax,
      policy_engine,
      policy_green_infra
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(variable, gender) %>%
  summarise(mean = mean(value), .groups = "drop")

desc_long <- desc_long %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "climate_importance",
        "policy_general",
        "policy_carbon_tax",
        "policy_engine",
        "policy_green_infra"
      ),
      labels = c(
        "Climate importance",
        "General policy support",
        "Carbon tax",
        "Engine ban",
        "Green infrastructure"
      )
    ),
    gender = factor(gender, levels = c("Female", "Male"))
  )

p_desc <- ggplot(desc_long, aes(x = variable, y = mean, fill = gender)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = c("Female" = "#6A3D9A", "Male" = "#E69F00")) +
  labs(
    title = "Mean climate-policy attitudes by gender",
    x = NULL,
    y = "Mean support (1–5 scale)",
    fill = "Gender"
  ) +
  coord_cartesian(ylim = c(0, 5)) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  "descriptive_barplot_gender.png",
  p_desc,
  width = 10,
  height = 6,
  dpi = 300
)

##decriptivaappendix
# -----------------------------
# APPENDIX — DISTRIBUTION TABLE (LIKERT)
# Example: combustion-engine ban
# -----------------------------

dist_engine <- PL_YOUTH %>%
  filter(voted_cat != "Missing/Refused") %>%
  filter(!is.na(policy_engine), !is.na(gender)) %>%
  count(gender, policy_engine) %>%
  group_by(gender) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    policy_engine = factor(
      policy_engine,
      levels = 1:5,
      labels = c("Strongly oppose", "Oppose", "Neutral", "Support", "Strongly support")
    )
  )

modelsummary::datasummary_df(
  dist_engine,
  output = "appendix_distribution_engine.html",
  title = "Appendix Table D1. Distribution of combustion-engine ban support by gender"
)
## for all policies
make_dist_table <- function(var, label) {
  PL_YOUTH %>%
    filter(voted_cat != "Missing/Refused") %>%
    filter(!is.na(.data[[var]]), !is.na(gender)) %>%
    count(gender, value = .data[[var]]) %>%
    group_by(gender) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      value = factor(
        value,
        levels = 1:5,
        labels = c("Strongly oppose", "Oppose", "Neutral", "Support", "Strongly support")
      )
    ) %>%
    modelsummary::datasummary_df(
      output = paste0("appendix_distribution_", var, ".html"),
      title = paste("Appendix Table D.", label, "Distribution by gender")
    )
}

make_dist_table("policy_engine", "1. Combustion-engine ban")
make_dist_table("policy_carbon_tax", "2. Carbon tax")
make_dist_table("policy_green_infra", "3. Green infrastructure")
# -----------------------------
# APPENDIX — CORRELATION MATRIX
# -----------------------------

corr_data <- PL_YOUTH %>%
  filter(voted_cat != "Missing/Refused") %>%
  dplyr::select(
    climate_importance,
    policy_general,
    policy_carbon_tax,
    policy_engine,
    policy_green_infra,
    ideology_c,
    trust_gov
  )

corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")

corr_matrix_df <- as.data.frame(round(corr_matrix, 3)) %>%
  tibble::rownames_to_column("variable")

modelsummary::datasummary_df(
  corr_matrix_df,
  output = "appendix_correlation_matrix.html",
  title = "Appendix Table D2. Correlation matrix of key variables"
)

# -----------------------------
# EXTRA ANALYSIS: Right-wing men (ideology 4–5)
# Climate importance distribution
# -----------------------------

right_men <- PL_YOUTH %>%
  filter(
    gender == "Male",
    ideology_lr %in% c(4, 5),   # <-- using your ORIGINAL (not centered) ideology
    !is.na(climate_importance)
  )

# distribution
right_men_dist <- right_men %>%
  count(climate_importance) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  arrange(climate_importance)

# print check
print(right_men_dist)

# reuse your colors
female_col <- "#6A3D9A"
male_col   <- "#E69F00"

p_right_men <- ggplot(
  right_men_dist,
  aes(x = factor(climate_importance), y = percent)
) +
  geom_col(fill = male_col, width = 0.7) +
  
  labs(
    title = "Climate Importance among Right-Wing Men (Ideology 4–5)",
    subtitle = "Distribution across 5-point scale",
    x = "Climate importance (1–5)",
    y = "Percentage (%)"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  "right_wing_men_climate_importance.png",
  p_right_men,
  width = 6,
  height = 4,
  dpi = 300
)

right_gender_compare <- PL_YOUTH %>%
  filter(
    ideology_lr %in% c(4, 5),
    !is.na(climate_importance),
    gender %in% c("Female", "Male")
  ) %>%
  count(gender, climate_importance) %>%
  group_by(gender) %>%
  mutate(percent = n / sum(n) * 100)

p_compare <- ggplot(
  right_gender_compare,
  aes(x = factor(climate_importance), y = percent, fill = gender)
) +
  geom_col(position = "dodge") +
  
  scale_fill_manual(values = c("Female" = female_col, "Male" = male_col)) +
  
  labs(
    title = "Climate Importance among Right-Leaning Respondents (Ideology 4–5)",
    x = "Climate importance (1–5)",
    y = "Percentage (%)",
    fill = "Gender"
  ) +
  
  theme_classic(base_size = 13)

ggsave("right_wing_gender_comparison.png", p_compare, width = 10, height = 6, dpi = 900)