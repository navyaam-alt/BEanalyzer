library(readxl)

# 1. Load your 2x2 dataset
my_2x2_data <- read_excel("C:/Users/Admin/Desktop/2x2--1/Book1.xlsx", sheet = "Sheet1")

# 2. Run the analyze_be function configured for a 2x2 design
results_2x2 <- analyze_be(
  data = my_2x2_data,
  subject_col = "Subject",       # Change if your subject column name differs
  sequence_col = "Sequence",     # Change if your sequence column name differs
  period_col = "Period",         # Change if your period column name differs
  treatment_col = "Treatment",   # Change if your treatment column name differs
  pk_col = "Cmax",               # Change to "AUC" or your parameter column name
  design = "2x2",
  model_type = "Mixed",
  reference_treatment = "R",     # Reference treatment code
  log_transform = TRUE,
  output_word_file = "2x2_BE_Report.docx"
)

# 3. View key summaries in your console
print(results_2x2$analysis_results$lsmeans_summary)
print(results_2x2$analysis_results$primary_contrast)
