#' Custom helper function to style flextables professionally
style_be_table <- function(ft) {
  ft <- flextable::autofit(ft)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bg(ft, part = "header", bg = "#2C3E50")
  ft <- flextable::color(ft, part = "header", color = "white")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::fontsize(ft, size = 9.5, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::padding(ft, padding.top = 4, padding.bottom = 4, part = "all")
  return(ft)
}

#' Analyze Bioequivalence Data and Generate Phoenix-Style Word Report
#' @export
analyze_be <- function(data,
                       subject_col,
                       sequence_col,
                       period_col,
                       treatment_col,
                       pk_col,
                       reference_treatment = "R",
                       design = "2x2",
                       model_type = "Mixed",
                       log_transform = TRUE,
                       output_word_file = NULL) {

  df <- data
  df$Subject <- as.factor(df[[subject_col]])
  df$Sequence <- as.factor(df[[sequence_col]])
  df$Period <- as.factor(df[[period_col]])
  df$Treatment <- as.factor(df[[treatment_col]])

  resp_name <- if (log_transform) paste0("log(", pk_col, ")") else pk_col
  if (log_transform) {
    df$PK_Response <- log(df[[pk_col]])
  } else {
    df$PK_Response <- df[[pk_col]]
  }

  # 1. Fit Model
  if (model_type == "Mixed") {
    formula_str <- paste0("PK_Response ~ Sequence + Period + Treatment + (1 | ", subject_col, ")")
    model <- lmerTest::lmer(as.formula(formula_str), data = df)
    anova_tab <- stats::anova(model)
    vc_raw <- as.data.frame(lme4::VarCorr(model))
    vc <- data.frame(
      Component = vc_raw$grp,
      Name = ifelse(is.na(vc_raw$var1), "Residual", vc_raw$var1),
      Variance = round(vc_raw$vcov, 5),
      Std_Dev = round(vc_raw$sdcor, 5)
    )
  } else {
    formula_str <- paste0("PK_Response ~ Sequence + Period + Treatment + ", subject_col)
    model <- stats::lm(as.formula(formula_str), data = df)
    anova_tab <- car::Anova(model, type = 3)
    vc <- data.frame(
      Component = "Residual Error",
      Name = "Residual",
      Variance = round(summary(model)$sigma^2, 5),
      Std_Dev = round(summary(model)$sigma, 5)
    )
  }

  # 2. LSMeans & Contrasts
  emm <- emmeans::emmeans(model, ~ Treatment)
  contrast_res <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = reference_treatment)
  contrast_summary <- summary(contrast_res, infer = TRUE, level = 0.90)

  lsmeans_df <- as.data.frame(summary(emm))
  contrast_df <- as.data.frame(contrast_summary)

  # Format numbers cleanly
  num_cols_lsm <- sapply(lsmeans_df, is.numeric)
  lsmeans_df[num_cols_lsm] <- lapply(lsmeans_df[num_cols_lsm], function(x) round(x, 4))

  num_cols_con <- sapply(contrast_df, is.numeric)
  contrast_df[num_cols_con] <- lapply(contrast_df[num_cols_con], function(x) round(x, 4))

  # 3. Geometric Means
  geom_means <- aggregate(df[[pk_col]] ~ df$Treatment, FUN = function(x) exp(mean(log(x))))
  names(geom_means) <- c("Treatment", "Geometric Mean")
  geom_means$`Geometric Mean` <- round(geom_means$`Geometric Mean`, 3)

  # 4. Descriptive Statistics
  desc_stats <- aggregate(df[[pk_col]] ~ df$Treatment, FUN = function(x) {
    c(N = length(x), Mean = round(mean(x), 2), SD = round(sd(x), 2),
      Median = round(median(x), 2), Min = round(min(x), 2), Max = round(max(x), 2))
  })
  desc_df <- data.frame(Treatment = desc_stats$`df$Treatment`, desc_stats$`df[[pk_col]]`)

  # 5. Residual Diagnostics
  sw_test <- stats::shapiro.test(residuals(model))

  analysis_results <- list(
    model = model,
    anova_table = anova_tab,
    lsmeans_summary = lsmeans_df,
    primary_contrast = contrast_df,
    geometric_means = geom_means,
    descriptive_stats = desc_df,
    shapiro_wilk = sw_test
  )

  # 6. Generate Professional Word Document
  if (!is.null(output_word_file)) {
    doc <- officer::read_docx()

    # Title
    doc <- officer::body_add_par(doc, "Bioequivalence Analysis Report", style = "heading 1")
    doc <- officer::body_add_par(doc, paste0("Generated on: ", Sys.Date()))
    doc <- officer::body_add_par(doc, "")

    # Section 1: Model Specification
    doc <- officer::body_add_par(doc, "1. Model Specification", style = "heading 2")
    doc <- officer::body_add_par(doc, paste0("Model Formula: ", resp_name, " ~ Sequence + Period + Treatment"))
    doc <- officer::body_add_par(doc, paste0("Total Observations: ", nrow(df)))
    doc <- officer::body_add_par(doc, paste0("Total Subjects: ", length(unique(df$Subject))))
    doc <- officer::body_add_par(doc, "")

    # Section 2: Variance Components
    doc <- officer::body_add_par(doc, "2. Variance Components", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(vc)))
    doc <- officer::body_add_par(doc, "")

    # Section 3: LSMeans
    doc <- officer::body_add_par(doc, "3. Least Squares Means", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(lsmeans_df)))
    doc <- officer::body_add_par(doc, "")

    # Section 4: Contrast & CIs
    doc <- officer::body_add_par(doc, "4. Reference vs Treatment Contrast & 90% CI", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(contrast_df)))
    doc <- officer::body_add_par(doc, "")

    # Section 5: ANOVA Table
    doc <- officer::body_add_par(doc, "5. Analysis of Variance (ANOVA)", style = "heading 2")
    anova_df <- as.data.frame(anova_tab)
    anova_df <- cbind(Effect = rownames(anova_df), anova_df)
    num_cols_anova <- sapply(anova_df, is.numeric)
    anova_df[num_cols_anova] <- lapply(anova_df[num_cols_anova], function(x) round(x, 4))
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(anova_df)))
    doc <- officer::body_add_par(doc, "")

    # Section 6: Descriptive Statistics
    doc <- officer::body_add_par(doc, "6. Descriptive Statistics", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(desc_df)))
    doc <- officer::body_add_par(doc, "")

    # Section 7: Geometric Means
    doc <- officer::body_add_par(doc, "7. Geometric Means", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(geom_means)))
    doc <- officer::body_add_par(doc, "")

    # Section 8: Residual Diagnostics
    doc <- officer::body_add_par(doc, "8. Residual Diagnostics", style = "heading 2")
    doc <- officer::body_add_par(doc, paste0("Shapiro-Wilk Normality Test p-value: ", round(sw_test$p.value, 5)))
    doc <- officer::body_add_par(doc, "")

    # Section 9: Dataset Used (First 20 Rows)
    doc <- officer::body_add_par(doc, "9. Dataset Used (First 20 Rows)", style = "heading 2")
    dataset_preview <- head(df, 20)
    # Round numerical columns in preview for clean output
    num_cols_data <- sapply(dataset_preview, is.numeric)
    dataset_preview[num_cols_data] <- lapply(dataset_preview[num_cols_data], function(x) round(x, 4))
    doc <- flextable::body_add_flextable(doc, style_be_table(flextable::flextable(dataset_preview)))

    print(doc, target = output_word_file)
    message("Formatted Word report successfully generated: ", output_word_file)
  }

  return(list(
    analysis_results = analysis_results,
    data_used = df
  ))
}
