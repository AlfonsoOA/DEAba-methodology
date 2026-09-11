# ----------------------------------------------------
# Complete Script for the Analysis of University Entrance
# Examinations (Selectividad/EBAU) (2001-2024)
# ----------------------------------------------------

# Install and load the necessary packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

# --- Environment Configuration ---
# Define the path to the Excel file and the folder where results will be saved
file_path <- "C:/Users/Alfonso/Desktop/AlfonsoOA_MSI/2Docencia/8_TrabajoCuestionesMetabolicas/0_AnalisisSelectividad/Resumen_Selectividad.xlsx"
results_path <- "C:/Users/Alfonso/Desktop/AlfonsoOA_MSI/2Docencia/8_TrabajoCuestionesMetabolicas/0_AnalisisSelectividad/PAU_analysis_results"

# Create the results folder if it does not exist
if (!dir.exists(results_path)) {
  dir.create(results_path)
}

# --- Data Loading and Preparation ---
# Read the data from the Excel file
data <- read_excel(file_path)

# Clean the data and create the binary variable 'is_question'
data <- data %>%
  mutate(
    N_questions = replace(N_questions, is.na(N_questions), ""),
    is_question = ifelse(N_questions == "x", 1, 0)
  )

# --- Utility Functions Section ---
# Function to create a count table per exam
# (This is key to consolidating the 'x' marks per exam sitting)
get_exam_summary <- function(data) {
  exam_summary <- data %>%
    group_by(Year, Call, Option) %>%
    summarise(has_metabolism_q = ifelse(sum(is_question) > 0, 1, 0), .groups = 'drop')
  return(exam_summary)
}

# ----------------------------------------------------
# 1. Is metabolism asked about less than other content blocks? (FINAL CORRECTED VERSION)
# ----------------------------------------------------
sink(file.path(results_path, "1_Frecuencia_Global.txt"))
cat("### 1. Overall Frequency of the Metabolism Block ###\n\n")

# We use the summary table to count exams, not individual questions
exam_summary <- get_exam_summary(data)

exams_with_metabolism_observed <- sum(exam_summary$has_metabolism_q)
total_exams_possible <- nrow(exam_summary)

# The null hypothesis (H0) is that every exam should include a metabolism question.
# Therefore, the expected number equals the total number of exams.
expected_exams_with_metabolism <- total_exams_possible

# Statistical test: Binomial test
# This test is more appropriate since the null hypothesis expects a proportion of 1.
binomial_test_1 <- binom.test(x = exams_with_metabolism_observed, 
                              n = total_exams_possible, 
                              p = 1,
                              alternative = "less") # The alternative is that it is asked LESS than expected

cat("--------------------------------------------\n")
cat("Analysis of the distribution of exams by content block\n")
cat("--------------------------------------------\n")
cat("Total exams (population):", total_exams_possible, "\n")
cat("Exams with metabolism questions (observed):", exams_with_metabolism_observed, "\n")
cat("Exams with metabolism questions (expected):", expected_exams_with_metabolism, "\n\n")
cat("Binomial Test Result:\n")
print(binomial_test_1)

if (binomial_test_1$p.value < 0.05) {
  cat("\nConclusion: The p-value (", binomial_test_1$p.value, ") is < 0.05. The null hypothesis is rejected.\n")
  cat("The frequency of exams with metabolism questions is significantly lower than expected.\n")
  cat("Specifically, metabolism is NOT asked about in every exam, as would be expected under a uniform, mandatory topic distribution.\n")
} else {
  cat("\nConclusion: The p-value is > 0.05. There is no statistical evidence to reject the null hypothesis.\n")
  cat("This would mean that metabolism is indeed asked about in most exams, consistent with the expected distribution.\n")
}
sink()

# ----------------------------------------------------
# 2. Has the number of metabolism questions decreased over time?
# (Corrected to count per exam, with a maximum of 4 per year)
# ----------------------------------------------------
sink(file.path(results_path, "2_Evolucion_Temporal_Preguntas.txt"))
cat("### 2. Temporal Evolution of the Number of Metabolism Questions ###\n\n")

# Obtain the summary table with 1 row per exam
exam_summary <- get_exam_summary(data)

# Count how many exams with metabolism questions there are per year
questions_per_year <- exam_summary %>%
  group_by(Year) %>%
  summarise(Total_Exams_with_Metabolism = sum(has_metabolism_q), .groups = 'drop') %>%
  mutate(Year = as.numeric(as.character(Year)))

# Join with a data frame of all years to ensure years with 0 are included
all_years <- data.frame(Year = 2001:2024)
questions_per_year <- all_years %>%
  left_join(questions_per_year, by = "Year") %>%
  mutate(Total_Exams_with_Metabolism = replace_na(Total_Exams_with_Metabolism, 0))

lm_model_2 <- lm(Total_Exams_with_Metabolism ~ Year, data = questions_per_year)

cat("--------------------------------------------\n")
cat("Annual Trend Analysis\n")
cat("--------------------------------------------\n")
cat("Annual values (number of exams with metabolism questions):\n")
print(questions_per_year)
cat("\nStatistical Test: Linear Regression Model\n")
summary_lm_2 <- summary(lm_model_2)
print(summary_lm_2)

if (summary_lm_2$coefficients["Year", "Pr(>|t|)"] < 0.05) {
  if (summary_lm_2$coefficients["Year", "Estimate"] < 0) {
    cat("\nConclusion: There is a significant negative trend. Metabolism is asked about LESS over time.\n")
  } else {
    cat("\nConclusion: There is a significant positive trend. Metabolism is asked about MORE over time.\n")
  }
} else {
  cat("\nConclusion: There is no significant linear trend in the number of exams with metabolism questions over time.\n")
}
sink()

plot_2 <- ggplot(questions_per_year, aes(x = Year, y = Total_Exams_with_Metabolism)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Annual Evolution of the Number of Exams Including Metabolism",
       x = "Year",
       y = "Total Exams with Metabolism (Max. 4)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2001, 2024, by = 2))
ggsave(file.path(results_path, "2_Evolucion_Temporal_Preguntas.png"), plot_2)


# ----------------------------------------------------
# 3. Are there significant preferences for certain subgroups (items)?
# ----------------------------------------------------
sink(file.path(results_path, "3_Preferencias_Subgrupos.txt"))
cat("### 3. Significant Preferences by Subgroup (Items) ###\n\n")

item_frequency <- data %>%
  filter(is_question == 1) %>%
  group_by(Item) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Statistical test: Chi-squared goodness-of-fit test for uniformity
chi_sq_test_3 <- chisq.test(item_frequency$Count)

cat("--------------------------------------------\n")
cat("Distribution of the most frequent items\n")
cat("--------------------------------------------\n")
cat("Top 5 items:\n")
print(head(item_frequency, 5))
cat("\nChi-squared test result:\n")
print(chi_sq_test_3)

if (chi_sq_test_3$p.value < 0.05) {
  cat("\nConclusion: The null hypothesis is rejected. There is a significant preference for certain items.\n")
} else {
  cat("\nConclusion: There is no evidence of preferences. The distribution of items is uniform.\n")
}
sink()

plot_3 <- ggplot(item_frequency, aes(x = reorder(Item, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Questions by Metabolism Subtopic",
       x = "Subtopic",
       y = "Number of times asked") +
  theme_minimal() +
  coord_flip()
ggsave(file.path(results_path, "3_Preferencias_Subgrupos.png"), plot_3, width = 10, height = 8)

# ----------------------------------------------------
# 4. Differences between examination calls? (Ordinary vs. Extraordinary)
# ----------------------------------------------------
sink(file.path(results_path, "4_Diferencias_Convocatoria.txt"))
cat("### 4. Frequency Differences Between Examination Calls ###\n\n")

exam_summary <- get_exam_summary(data)
questions_by_call <- exam_summary %>%
  group_by(Call) %>%
  summarise(Total_Exams_with_Metabolism = sum(has_metabolism_q), .groups = 'drop')

# Statistical test: Chi-squared test of independence
observed_matrix <- matrix(
  c(
    questions_by_call$Total_Exams_with_Metabolism[1],
    (48 - questions_by_call$Total_Exams_with_Metabolism[1]),
    questions_by_call$Total_Exams_with_Metabolism[2],
    (48 - questions_by_call$Total_Exams_with_Metabolism[2])
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(Call = c("Ordinary", "Extraordinary"), Status = c("With Metabolism", "Without Metabolism"))
)
chi_sq_test_4 <- chisq.test(observed_matrix)

cat("--------------------------------------------\n")
cat("Analysis of questions by examination call\n")
cat("--------------------------------------------\n")
print(questions_by_call)
cat("\nChi-squared test result:\n")
print(chi_sq_test_4)

if (chi_sq_test_4$p.value < 0.05) {
  cat("\nConclusion: The frequency of metabolism questions differs significantly between examination calls.\n")
} else {
  cat("\nConclusion: There is no evidence that frequency depends on the examination call.\n")
}
sink()

# ----------------------------------------------------
# 5. Biases regarding Option A or B?
# ----------------------------------------------------
sink(file.path(results_path, "5_Sesgos_Opcion.txt"))
cat("### 5. Frequency Biases Between Options (A and B) ###\n\n")

exam_summary <- get_exam_summary(data)
questions_by_option <- exam_summary %>%
  group_by(Option) %>%
  summarise(Total_Exams_with_Metabolism = sum(has_metabolism_q), .groups = 'drop')

# Statistical test: Chi-squared test of independence
observed_matrix_option <- matrix(
  c(
    questions_by_option$Total_Exams_with_Metabolism[1],
    (48 - questions_by_option$Total_Exams_with_Metabolism[1]),
    questions_by_option$Total_Exams_with_Metabolism[2],
    (48 - questions_by_option$Total_Exams_with_Metabolism[2])
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(Option = c("A", "B"), Status = c("With Metabolism", "Without Metabolism"))
)
chi_sq_test_5 <- chisq.test(observed_matrix_option)

cat("--------------------------------------------\n")
cat("Analysis of questions by option\n")
cat("--------------------------------------------\n")
print(questions_by_option)
cat("\nChi-squared test result:\n")
print(chi_sq_test_5)

if (chi_sq_test_5$p.value < 0.05) {
  cat("\nConclusion: The frequency of metabolism questions differs significantly between options (A and B).\n")
} else {
  cat("\nConclusion: There is no evidence that frequency depends on the option.\n")
}
sink()

# ----------------------------------------------------
# 6. Which items have never been asked about?
# ----------------------------------------------------
sink(file.path(results_path, "6_Items_Nunca_Preguntados.txt"))
cat("### 6. Items Never Asked About ###\n\n")

never_asked_items <- data %>%
  group_by(Item) %>%
  summarise(Total_Questions = sum(is_question), .groups = 'drop') %>%
  filter(Total_Questions == 0)

cat("--------------------------------------------\n")
cat("List of items with zero questions\n")
cat("--------------------------------------------\n")
if (nrow(never_asked_items) > 0) {
  print(never_asked_items)
} else {
  cat("Every item has been asked about at least once!\n")
}
sink()

# ----------------------------------------------------
# 7. Which metabolism topics are the most "profitable" to study?
# ----------------------------------------------------
sink(file.path(results_path, "7_Items_Rentables.txt"))
cat("### 7. Most 'Profitable' Topics (Consistent and Frequent) ###\n\n")

profitable_items <- data %>%
  filter(is_question == 1) %>%
  group_by(Item) %>%
  summarise(
    Times_Asked = n(),
    Years_Asked = n_distinct(Year),
    .groups = 'drop'
  ) %>%
  mutate(Consistency_Ratio = Times_Asked / Years_Asked) %>%
  arrange(desc(Times_Asked), desc(Years_Asked))

cat("--------------------------------------------\n")
cat("Top 5 items with the highest 'profitability' (frequency and consistency)\n")
cat("--------------------------------------------\n")
print(head(profitable_items, 5))
sink()

# ----------------------------------------------------
# 8. and 11. In how many exam sittings could students have taken the
# entrance exam without being tested on metabolism?
# (Count of exam calls and available options without metabolism questions)
# ----------------------------------------------------
sink(file.path(results_path, "8_y_11_Examenes_Sin_Metabolismo.txt"))

# Answers question 8
cat("### 8. Complete Exams Without Metabolism Questions ###\n\n")

exam_summary <- get_exam_summary(data)
exams_without_metabolism <- exam_summary %>%
  group_by(Year, Call) %>%
  summarise(options_without_metabolism = sum(has_metabolism_q == 0), .groups = 'drop') %>%
  filter(options_without_metabolism == 2) %>%
  nrow()

cat("--------------------------------------------\n")
cat("Conclusion (Question 8):\n")
cat("--------------------------------------------\n")
cat("Total exams (calls) where neither option included a metabolism question:", exams_without_metabolism, "\n\n")

# Answers question 11
cat("### 11. Exam Options Available Without Metabolism ###\n\n")

calls_with_option_without_metabolism <- exam_summary %>%
  group_by(Year, Call) %>%
  summarise(options_without_metabolism = sum(has_metabolism_q == 0), .groups = 'drop') %>%
  filter(options_without_metabolism > 0) %>%
  nrow()

cat("--------------------------------------------\n")
cat("Conclusion (Question 11):\n")
cat("--------------------------------------------\n")
cat("Number of exam calls in which at least one option (A or B) had no metabolism questions:", calls_with_option_without_metabolism, "\n")

sink()

# ----------------------------------------------------
# 9. Are "simpler" questions being asked in recent years?
# and 10. Has the question profile changed over time?
# ----------------------------------------------------
sink(file.path(results_path, "9_y_10_Perfil_y_Sencillez.txt"))
cat("### 9. and 10. Question Profile and Evolution of Simplicity ###\n\n")

# We analyze the 10 most frequent items (question profile)
top_10_items <- data %>%
  filter(is_question == 1) %>%
  group_by(Item) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count)) %>%
  slice_head(n = 10)

item_frequency_by_year <- data %>%
  filter(Item %in% top_10_items$Item) %>%
  group_by(Year, Item) %>%
  summarise(Annual_Frequency = sum(is_question), .groups = 'drop') %>%
  right_join(
    expand.grid(Year = 2001:2024, Item = top_10_items$Item),
    by = c("Year", "Item")
  ) %>%
  mutate(Annual_Frequency = replace_na(Annual_Frequency, 0))

# Item D1.1. is considered a "simple question"
# Run a linear regression test for this item
sencilla_analysis <- item_frequency_by_year %>%
  filter(Item == "Definition of metabolism, catabolism, and anabolism") %>%
  do({
    model <- lm(Annual_Frequency ~ Year, data = .)
    data.frame(
      Item = .$Item[1],
      Coefficient_Year = summary(model)$coefficients["Year", "Estimate"],
      P_Value = summary(model)$coefficients["Year", "Pr(>|t|)"]
    )
  })

cat("--------------------------------------------\n")
cat("Trend Analysis of 'Simple' Questions (Item D1.1.)\n")
cat("--------------------------------------------\n")
print(sencilla_analysis)
if (sencilla_analysis$P_Value < 0.05) {
  if (sencilla_analysis$Coefficient_Year > 0) {
    cat("\nConclusion: Item D1.1. is asked about significantly MORE over time.\n")
  } else {
    cat("\nConclusion: Item D1.1. is asked about significantly LESS over time.\n")
  }
} else {
  cat("\nConclusion: There is no significant trend in the frequency of item D1.1. over time.\n")
}
cat("\n")

# Plot of the evolution of the 10 most common items
plot_9_10 <- ggplot(item_frequency_by_year, aes(x = Year, y = Annual_Frequency, color = Item)) +
  geom_line() +
  geom_point() +
  labs(title = "Annual Evolution of the Frequency of the 10 Most Common Items",
       subtitle = "Change in the question profile over time",
       x = "Year",
       y = "Annual Frequency",
       color = "Item") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2001, 2024, by = 2)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))
ggsave(file.path(results_path, "9_y_10_Evolucion_Perfil.png"), plot_9_10, width = 10, height = 8)

sink()

# ----------------------------------------------------
# 12. What average proportion of the course content is asked about?
# ----------------------------------------------------
sink(file.path(results_path, "12_Media_Contenidos_Preguntados.txt"))
cat("### 12. Percentage of Syllabus Content Asked About ###\n\n")

# 1. Count the total number of items in the syllabus
total_items <- n_distinct(data$Item)

# 2. Count the number of unique items asked about per year
items_per_year <- data %>%
  filter(is_question == 1) %>%
  group_by(Year) %>%
  summarise(unique_items_asked = n_distinct(Item), .groups = 'drop')

# 3. Join with all years to include years with 0
all_years <- data.frame(Year = 2001:2024)
items_per_year_complete <- all_years %>%
  left_join(items_per_year, by = "Year") %>%
  mutate(unique_items_asked = replace_na(unique_items_asked, 0))

# 4. Calculate the percentage of items asked about per year
items_per_year_complete <- items_per_year_complete %>%
  mutate(percentage_asked = (unique_items_asked / total_items) * 100)

# 5. Calculate the average across all years
average_percentage_asked <- mean(items_per_year_complete$percentage_asked)
average_percentage_not_asked <- 100 - average_percentage_asked

cat("--------------------------------------------\n")
cat("Syllabus Coverage Analysis\n")
cat("--------------------------------------------\n")
cat("Total number of items in the metabolism syllabus:", total_items, "\n\n")
cat("Percentage of items asked about per year:\n")
print(items_per_year_complete)
cat("\n--------------------------------------------\n")
cat("Conclusion:\n")
cat("--------------------------------------------\n")
cat("On average,", round(average_percentage_asked, 2), "% of the metabolism syllabus items are asked about in the entrance exam each year.\n")
cat("This implies that, on average,", round(average_percentage_not_asked, 2), "% of the syllabus is not asked about annually.\n")
sink()

# ----------------------------------------------------
# End of script
# ----------------------------------------------------
cat("\nThe analysis has finished. Check the folder '", results_path, "' for reports and plots.\n")

# ----------------------------------------------------
# 13. Has the frequency of "basic" questions increased over time?
# ----------------------------------------------------
sink(file.path(results_path, "13_Evolucion_Preguntas_Basicas.txt"))
cat("### 13. Temporal Evolution of the Frequency of Basic Questions ###\n\n")

# Define items as 'Basic' or 'Advanced'
# This classification is based on the nature of the question (definitions vs. processes)
data_with_level <- data %>%
  mutate(item_level = case_when(
    grepl("Definition of metabolism", Item) ~ "Basico",
    grepl("Fermentation", Item) ~ "Basico",
    grepl("Photosynthesis: overall balance", Item) ~ "Basico",
    TRUE ~ "Avanzado"
  ))

# Count the number of 'Basic' and 'Advanced' items per year
questions_by_level_per_year <- data_with_level %>%
  filter(is_question == 1) %>%
  group_by(Year, item_level) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = item_level, values_from = Count, values_fill = 0) %>%
  mutate(Total_Questions = Basico + Avanzado,
         Proportion_Basico = Basico / Total_Questions)

# Run the linear regression test on the proportion of basic questions
lm_model_13 <- lm(Proportion_Basico ~ Year, data = questions_by_level_per_year)

cat("--------------------------------------------\n")
cat("Annual Trend Analysis of Basic Questions\n")
cat("--------------------------------------------\n")
cat("Proportion of basic questions per year:\n")
print(questions_by_level_per_year)
cat("\nStatistical Test: Linear Regression Model\n")
summary_lm_13 <- summary(lm_model_13)
print(summary_lm_13)

if (summary_lm_13$coefficients["Year", "Pr(>|t|)"] < 0.05) {
  if (summary_lm_13$coefficients["Year", "Estimate"] > 0) {
    cat("\nConclusion: There is a significant positive trend. The proportion of basic questions increases over time.\n")
  } else {
    cat("\nConclusion: There is a significant negative trend. The proportion of basic questions decreases over time.\n")
  }
} else {
  cat("\nConclusion: There is no significant linear trend. The proportion of basic questions remains stable over the years.\n")
}
sink()

# Create a plot to visualize the trend
plot_13 <- ggplot(questions_by_level_per_year, aes(x = Year, y = Proportion_Basico)) +
  geom_line(color = "orange") +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Evolution of the Proportion of Basic Questions",
       x = "Year",
       y = "Proportion of Basic Questions") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2001, 2024, by = 2))
ggsave(file.path(results_path, "13_Evolucion_Preguntas_Basicas.png"), plot_13)