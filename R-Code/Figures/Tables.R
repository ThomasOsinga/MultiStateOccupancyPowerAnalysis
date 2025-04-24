
# Load libraries
library(tidyr)
library(knitr)  # For kable
library(gt)     # For gt tables

library(flextable)
library(officer)

# Filter the data for specific parameters and model 10
test <- Convergence[[1]] %>%
  filter(
    covariates==FALSE,
    #parameter %in% c("beta0", "beta0[1,1]", "beta1", "beta1[1,1]","beta1[2,1]"),
    parameter %in% c("p", 'p33',"p22", "psi", "R1", "R2"),
    model == 10
  )
# Select relevant columns and pivot the data
table_data <- test %>%
  select(parameter, NrSites, Survey, ModelType, Metric, Value,mean_sd, mean_lower, covariates,mean_upper, truth) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Value,
    values_fill = NA  # Fill missing values with NA if any
  ) %>%
  # Optionally, arrange the table for better readability
  arrange(parameter, ModelType)

# Rename columns for clarity
table_data <- table_data %>%
  rename(
    Bias = Bias,
    SD = mean_sd,
    CI_Lower = mean_lower,
    CI_Upper = mean_upper,
    Truth = truth
  ) %>%
  select(parameter, NrSites, Survey, ModelType, Bias, SD, CI_Lower, CI_Upper, Truth) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

### Calculate max 
x <- table_data %>% 
  group_by(parameter, NrSites, Survey, ModelType) %>% 
  summarise(max_abs_bias = max(abs(Bias)), .groups = "drop")

# Create a flextable object
ft <- flextable(table_data)
# Highlight cells where Bias > 0.5
(ft <- ft %>%
  fontsize(size = 12, part = "all") %>%
  font(fontname = "Arial", part = "all") %>% 
  add_header_lines("Model 10: Bias, Standard Deviation, and Confidence Intervals for Selected Parameters") %>%  # Add a header line
  set_header_labels(
    parameter = "Parameter",
    ModelType = "Model Type",
    Bias = "Bias",
    SD = "Standard Deviation",
    CI_Lower = "CI Lower",
    CI_Upper = "CI Upper",
    Truth = "Truth"
  ) %>%
  # Adjust column widths to fit content
  autofit() %>%
  # Make header text bold
  bold(part = "header") %>%
  # Center-align text in all columns
  align(align = "center", part = "all") %>%
  # Add borders for better readability
  border_outer(border = fp_border(color = "black", width = 1)) %>%
  border_inner_h(border = fp_border(color = "black", width = 0.5)) %>%
  border_inner_v(border = fp_border(color = "black", width = 0.5)))


# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, ft)

# Save the Word document
print(doc, target = "Table_Figure2.docx")

