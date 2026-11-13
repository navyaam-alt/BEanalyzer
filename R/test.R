#Run your test script
library(readxl)
my_be_data <- read_excel("C:/Users/Admin/Desktop/3x3--1/3x3 data.xlsx", sheet = "Sheet1")

results_auci <- analyze_be(
  data = my_be_data,
  subject_col = "Subject",
  sequence_col = "Sequence",
  period_col = "Period",
  treatment_col = "Treatment",
  pk_col = "AUCI",
  design = "3x3",
  model_type = "Mixed",
  reference_treatment = "R",
  log_transform = TRUE,
  output_word_file = "3x3_AUCI_BE_Report.docx"
)
