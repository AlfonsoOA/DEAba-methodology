#----------------------------------------------------------------------
# Comprehensive R Script for Academic Performance and Perception Analysis
#
# This script loads and cleans data from multiple sources to perform
# descriptive and inferential statistical analyses, visualize trends,
# and build regression models.
#----------------------------------------------------------------------

#======================================================================
# 1. Configuration and Parameters (User-Customizable)
#======================================================================

# Define the base path for your project
base_path <- "C:/Users/Alfonso/Desktop/AlfonsoOA_MSI/2Docencia/8_Trabajo-DEAba"

# Define the output folder name
output_folder_name <- "DEA_activities_analysis_v11092026"

#-------------------
# Input File Names
#-------------------
marks_file <- "0_BQM_marks_ordered.xlsx"
surveys_file <- "1_BQM_PreviousKnowledgePlusOthers_ordered.xlsx"
methodology_file <- "2_BQM_DEASurvey_ordered.xlsx"
degree_file <- "3_BiologyIndices_ordered.xlsx"
MQ_file <- "0_BQM_MQ_marks_ordered.xlsx"

#-------------------
# Key Column Names
#-------------------
# Note: Use the exact names from your Excel files.
academic_year_col_marks <- "Academic year"
academic_year_col_surveys <- "Academic year"
academic_year_col_methodology <- "AcademicYear"
academic_year_col_degree <- "AcademicYear"
academic_year_col_QM <- "Academic year"

student_id_col <- "Student"
mark_col <- "Mark"
global_mark_col <- "GlobalMark"
first_q_mark_col <- "1Q_Mark"
second_q_mark_col <- "2Q_Mark"

# Define perception column prefixes (P1, P2, etc. in raw data)
raw_surveys_perception_cols <- paste0("P", 1:18)
raw_dea_perception_cols <- paste0("P", 1:5)

# Define the names for perception columns AFTER renaming (Q1, Q2, etc.)
surveys_perception_cols <- paste0("Q", 1:18)
dea_perception_cols <- paste0("Q", 1:5)

# Define consistent perception questions (those present in all years)
consistent_perception_cols <- paste0("Q", 1:5)

# Degree indices names (as they appear in the raw data)
degree_indices_item_col <- "Item"
degree_indices_value_col <- "Value"
degree_indices_names <- c("Tasa de Graduacion", "Tasa de Abandono", "Tasa de Rendimiento", "Tasa de Eficiencia", "Nota media de estudiantes de nuevo ingreso", "Tasa de Exito", "Duracion Media de los Estudios")

#-------------------
# Analysis Periods
#-------------------
before_new_methodology_years_marks <- c("2018/2019", "2020/2021", "2021/2022")
after_new_methodology_years_marks <- c("2022/2023", "2023/2024", "2024/2025")

before_new_methodology_years_surveys <- c("2018/2019", "2019/2020", "2021/2022")
after_new_methodology_years_surveys <- c("2022/2023", "2023/2024", "2024/2025")

covid_pre_pandemic_years <- c("2018/2019")
covid_pandemic_years <- c("2019/2020", "2020/2021")
covid_post_pandemic_years <- c("2021/2022", "2022/2023", "2023/2024", "2024/2025")

#-------------------
# Other Parameters
#-------------------
performance_group_breaks <- c(-Inf, 4.9, 6.9, 8.9, Inf)
performance_group_labels <- c("Fail", "Pass", "Good", "Excellent")
prior_knowledge_perceptions_to_analyze <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18")
fig_width <- 10
fig_height <- 6
fig_resolution <- 300

#======================================================================
# 2. Setup: Load Libraries and Prepare Environment
#======================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rstatix)
library(purrr)
library(stringr)
library(car)
library(MASS)
library(Hmisc) # For rcorr function

output_dir <- file.path(base_path, output_folder_name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(paste("Created output directory:", output_dir, "\n\n"))
}

save_plot <- function(plot_object, filename, width, height, resolution) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = resolution
  )
}

#======================================================================
# 3. Data Loading and Initial Cleaning
#======================================================================

cat("### Loading and Cleaning Data ###\n\n")

df_marks <- read_excel(file.path(base_path, marks_file))
df_surveys <- read_excel(file.path(base_path, surveys_file))
df_methodology <- read_excel(file.path(base_path, methodology_file))
df_degree <- read_excel(file.path(base_path, degree_file))

# Standardize column names for all dataframes
names(df_marks) <- make.names(names(df_marks), unique = TRUE)
names(df_surveys) <- make.names(names(df_surveys), unique = TRUE)
names(df_methodology) <- make.names(names(df_methodology), unique = TRUE)
names(df_degree) <- make.names(names(df_degree), unique = TRUE)

df_surveys <- df_surveys %>%
  dplyr::rename_with(~str_replace(., "^P", "Q"), dplyr::starts_with("P"))

df_methodology <- df_methodology %>%
  dplyr::rename_with(~str_replace(., "^P", "Q"), dplyr::starts_with("P"))

# Create a Period variable for analysis
df_marks <- df_marks %>%
  dplyr::mutate(Period = dplyr::case_when(
    !!sym(make.names(academic_year_col_marks)) %in% before_new_methodology_years_marks ~ "Before_New_Methodology",
    !!sym(make.names(academic_year_col_marks)) %in% after_new_methodology_years_marks ~ "After_New_Methodology",
    TRUE ~ "Other_Years"
  ))

df_surveys <- df_surveys %>%
  dplyr::mutate(Period = dplyr::case_when(
    !!sym(make.names(academic_year_col_surveys)) %in% before_new_methodology_years_surveys ~ "Before_New_Methodology",
    !!sym(make.names(academic_year_col_surveys)) %in% after_new_methodology_years_surveys ~ "After_New_Methodology",
    TRUE ~ "Other_Years"
  ))

df_surveys <- df_surveys %>%
  dplyr::mutate(CovidPeriod = dplyr::case_when(
    !!sym(make.names(academic_year_col_surveys)) %in% covid_pre_pandemic_years ~ "Pre_Pandemic",
    !!sym(make.names(academic_year_col_surveys)) %in% covid_pandemic_years ~ "Pandemic",
    !!sym(make.names(academic_year_col_surveys)) %in% covid_post_pandemic_years ~ "Post_Pandemic",
    TRUE ~ "Other"
  ))

# Pivot degree indices data from long to wide format
df_degree_wide <- df_degree %>%
  tidyr::pivot_wider(names_from = !!sym(make.names(degree_indices_item_col)), values_from = !!sym(make.names(degree_indices_value_col)))

cleaned_names <- names(df_degree_wide)
cleaned_names <- stringr::str_squish(cleaned_names)
cleaned_names <- make.names(cleaned_names, unique = TRUE)
names(df_degree_wide) <- cleaned_names

df_degree_wide <- df_degree_wide %>%
  dplyr::rename(
    Graduation.Rate = `Tasa.de.Graduacion`,
    Dropout.Rate = `Tasa.de.Abandono`,
    Performance.Rate = `Tasa.de.Rendimiento`,
    Efficiency.Rate = `Tasa.de.Eficiencia`,
    Avg_Entrance_Mark = `Nota.media.de.estudiantes.de.nuevo.ingreso`,
    Success.Rate = `Tasa.de.Exito`,
    Avg.Study.Duration = `Duracion.Media.de.los.Estudios`
  ) %>%
  dplyr::rename(!!make.names(academic_year_col_marks) := AcademicYear)

#======================================================================
# 4. Marks File Analysis (0_BQM_marks_ordered.xlsx) - Outputs 1_
#======================================================================

cat("\n### 1. Marks File Analysis ###\n\n")

yearly_results <- df_marks %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_marks))) %>%
  dplyr::summarise(
    Enrolled = n(),
    Presented = sum(!is.na(!!sym(make.names(mark_col)))),
    Did_Not_Present = sum(is.na(!!sym(make.names(mark_col)))),
    Completion_Rate = (Presented / Enrolled) * 100,
    Dropout_Rate = (Did_Not_Present / Enrolled) * 100,
    Passed = sum(!!sym(make.names(mark_col)) >= 5, na.rm = TRUE),
    Passed_of_Enrolled = (Passed / Enrolled) * 100,
    Passed_of_Presented = (Passed / Presented) * 100,
    Overall_Mean_Mark = mean(!!sym(make.names(mark_col)), na.rm = TRUE),
    Overall_Std_Dev = sd(!!sym(make.names(mark_col)), na.rm = TRUE),
    Passed_Mean_Mark = mean(dplyr::if_else(!!sym(make.names(mark_col)) >= 5, !!sym(make.names(mark_col)), NA_real_), na.rm = TRUE),
    Passed_Std_Dev = sd(dplyr::if_else(!!sym(make.names(mark_col)) >= 5, !!sym(make.names(mark_col)), NA_real_), na.rm = TRUE),
    .groups = 'drop'
  )
write.csv(yearly_results, file.path(output_dir, "1_yearly_descriptive_results.csv"), row.names = FALSE)
cat(paste0("Descriptive results table saved to ", output_folder_name, "/1_yearly_descriptive_results.csv\n\n"))

df_marks <- df_marks %>%
  dplyr::mutate(Performance_Group = cut(!!sym(make.names(mark_col)),
                                        breaks = performance_group_breaks,
                                        labels = performance_group_labels,
                                        right = TRUE))
group_distribution <- df_marks %>%
  dplyr::filter(!is.na(!!sym(make.names(mark_col)))) %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_marks)), Performance_Group) %>%
  dplyr::summarise(Count = n(), .groups = 'drop') %>%
  dplyr::mutate(Percentage = (Count / sum(Count)) * 100)
write.csv(group_distribution, file.path(output_dir, "1_group_distribution_results.csv"), row.names = FALSE)

plot_group_dist <- ggplot(group_distribution, aes(x = !!sym(make.names(academic_year_col_marks)), y = Percentage, fill = Performance_Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Student Performance Distribution by Academic Year",
       x = "Academic Year", y = "Percentage of Students", fill = "Performance Group") +
  scale_y_continuous(labels = scales::percent) + theme_minimal()
save_plot(plot_group_dist, "1_performance_distribution_bar_chart.png", fig_width, fig_height, fig_resolution)

plot_histograms <- ggplot(df_marks %>% dplyr::filter(!is.na(!!sym(make.names(mark_col)))), aes(x = !!sym(make.names(mark_col)))) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(as.formula(paste("~", make.names(academic_year_col_marks))), scales = "free_y") +
  labs(title = "Histogram of Marks by Academic Year",
       x = "Mark", y = "Frequency") + theme_minimal()
save_plot(plot_histograms, "1_marks_histograms.png", fig_width, fig_height, fig_resolution)

plot_boxplots <- ggplot(df_marks %>% dplyr::filter(!is.na(!!sym(make.names(mark_col)))), aes(x = !!sym(make.names(academic_year_col_marks)), y = !!sym(make.names(mark_col)))) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplots of Marks by Academic Year",
       x = "Academic Year", y = "Mark") + theme_minimal()
save_plot(plot_boxplots, "1_marks_boxplots.png", fig_width, fig_height, fig_resolution)

yearly_results_long <- yearly_results %>%
  dplyr::select(all_of(c(make.names(academic_year_col_marks), "Completion_Rate", "Dropout_Rate", "Passed_of_Enrolled", "Passed_of_Presented"))) %>%
  tidyr::pivot_longer(-!!sym(make.names(academic_year_col_marks)), names_to = "Metric", values_to = "Percentage")
plot_metrics_bar <- ggplot(yearly_results_long, aes(x = !!sym(make.names(academic_year_col_marks)), y = Percentage, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Key Performance Metrics",
       x = "Academic Year", y = "Percentage", fill = "Metric") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Completion Rate", "Dropout Rate", "Passed (of Enrolled)", "Passed (of Presented)"))
save_plot(plot_metrics_bar, "1_key_metrics_bar_chart.png", fig_width * 1.2, fig_height * 1.2, fig_resolution)
cat("All marks charts have been saved with the '1_' prefix.\n\n")

analysis_output_1 <- capture.output({
  cat("### 1.3. Inferential Analysis: Before vs. After New Methodology ###\n\n")
  
  mark_col_name <- make.names(mark_col)
  
  df_analysis_marks <- df_marks %>%
    dplyr::filter(Period %in% c("Before_New_Methodology", "After_New_Methodology")) %>%
    dplyr::rename(mark_to_analyze = !!mark_col_name)
  
  cat("--- Descriptive Statistics for Mark by Period ---\n")
  desc_stats <- df_analysis_marks %>%
    dplyr::group_by(Period) %>%
    dplyr::summarise(
      Mean = mean(mark_to_analyze, na.rm = TRUE),
      Median = median(mark_to_analyze, na.rm = TRUE),
      SD = sd(mark_to_analyze, na.rm = TRUE),
      .groups = 'drop'
    )
  print(desc_stats)
  cat("\n")
  
  cat("--- Normality Test (Shapiro-Wilk) ---\n")
  normality_test <- df_analysis_marks %>%
    dplyr::group_by(Period) %>% rstatix::shapiro_test(mark_to_analyze)
  print(normality_test)
  
  # Homogeneity of variances test (Levene's Test)
  cat("\n--- Homogeneity of Variances Test (Levene's) ---\n")
  levene_test <- tryCatch({
    car::leveneTest(mark_to_analyze ~ Period, data = df_analysis_marks)
  }, error = function(e) {
    message("Levene's test could not be performed. Not enough data per group.")
    return(NULL)
  })
  if (!is.null(levene_test)) {
    print(levene_test)
  }
  
  if (all(normality_test$p > 0.05, na.rm = TRUE)) {
    cat("\n--- Student's t-test (assuming Normality) ---\n")
    test_result <- t.test(mark_to_analyze ~ Period, data = df_analysis_marks)
  } else {
    cat("\n--- Mann-Whitney U Test (Non-parametric) ---\n")
    test_result <- wilcox.test(mark_to_analyze ~ Period, data = df_analysis_marks)
  }
  print(test_result)
  
  # Cohen's d (Effect size)
  cohen_d_result <- rstatix::cohens_d(df_analysis_marks, mark_to_analyze ~ Period)
  cat("\n--- Effect Size (Cohen's d) ---\n")
  print(cohen_d_result)
  
  df_presented <- df_analysis_marks
  df_presented$Presented_Status <- dplyr::if_else(is.na(df_presented[["mark_to_analyze"]]), "Did_Not_Present", "Presented")
  
  contingency_table_completion <- table(df_presented$Period, df_presented$Presented_Status)
  cat("\n--- Contingency Table for Completion ---\n")
  print(contingency_table_completion)
  cat("\n--- Chi-squared Test for Completion Rate ---\n")
  chi_square_completion <- chisq.test(contingency_table_completion)
  print(chi_square_completion)
  
  df_pass <- df_analysis_marks %>%
    dplyr::filter(!is.na(df_analysis_marks[["mark_to_analyze"]]))
  df_pass$Pass_Status <- dplyr::if_else(df_pass[["mark_to_analyze"]] >= 5, "Passed", "Failed")
  
  contingency_table_pass <- table(df_pass$Period, df_pass$Pass_Status)
  cat("\n--- Contingency Table for Pass/Fail (Presented Students Only) ---\n")
  print(contingency_table_pass)
  cat("\n--- Chi-squared Test for Pass Rate (Presented Students Only) ---\n")
  chi_square_pass <- chisq.test(contingency_table_pass)
  print(chi_square_pass)
  
  df_all_students <- df_analysis_marks
  df_all_students$Final_Status <- dplyr::case_when(
    is.na(df_all_students[["mark_to_analyze"]]) ~ "Not_Passed",
    df_all_students[["mark_to_analyze"]] < 5 ~ "Not_Passed",
    TRUE ~ "Passed"
  )
  
  contingency_table_all <- table(df_all_students$Period, df_all_students$Final_Status)
  cat("\n--- Contingency Table for Pass vs. Not-Passed (All Enrolled) ---\n")
  print(contingency_table_all)
  cat("\n--- Chi-squared Test for Pass vs. Not-Passed (All Enrolled) ---\n")
  chi_square_all <- chisq.test(contingency_table_all)
  print(chi_square_all)
})
writeLines(analysis_output_1, file.path(output_dir, "1_inferential_analysis_results.txt"))
cat(paste0("Inferential analysis results saved to ", output_folder_name, "/1_inferential_analysis_results.txt\n\n"))

#======================================================================
# 5. Surveys File Analysis (1_BQM_PreviousKnowledgePlusOthers_ordered.xlsx) - Outputs 2_
#======================================================================

cat("\n### 2. Survey Analysis ###\n\n")

df_attendance_marks <- df_marks %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_marks))) %>%
  dplyr::summarise(Enrolled = n(), .groups = 'drop')
df_attendance_surveys <- df_surveys %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_surveys))) %>%
  dplyr::summarise(Surveyed = n(), .groups = 'drop')
df_attendance <- dplyr::right_join(df_attendance_surveys, df_attendance_marks, by = setNames(make.names(academic_year_col_marks), make.names(academic_year_col_surveys))) %>%
  dplyr::mutate(Attendance_Percentage = (Surveyed / Enrolled) * 100)
write.csv(df_attendance, file.path(output_dir, "2_attendance_rate.csv"), row.names = FALSE)
plot_attendance <- ggplot(df_attendance, aes(x = !!sym(make.names(academic_year_col_surveys)), y = Attendance_Percentage)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Survey Attendance Percentage", x = "Academic Year", y = "Attendance (%)") +
  theme_minimal()
save_plot(plot_attendance, "2_attendance_bar_chart.png", fig_width, fig_height, fig_resolution)
cat(paste0("Attendance chart saved to ", output_folder_name, "/2_attendance_bar_chart.png\n\n"))

df_marks_long <- df_surveys %>%
  dplyr::select(all_of(c(make.names(academic_year_col_surveys), make.names(global_mark_col), make.names(first_q_mark_col), make.names(second_q_mark_col)))) %>%
  tidyr::pivot_longer(
    cols = all_of(c(make.names(global_mark_col), make.names(first_q_mark_col), make.names(second_q_mark_col))),
    names_to = "Mark_Type",
    values_to = "Mark"
  ) %>%
  dplyr::mutate(Mark_Type = dplyr::case_when(
    Mark_Type == make.names(global_mark_col) ~ "Global Mark",
    Mark_Type == make.names(first_q_mark_col) ~ "1st Semester Mark",
    Mark_Type == make.names(second_q_mark_col) ~ "2nd Semester Mark",
    TRUE ~ Mark_Type
  ))
plot_marks_boxplots <- ggplot(df_marks_long, aes(x = !!sym(make.names(academic_year_col_surveys)), y = Mark, fill = Mark_Type)) +
  geom_boxplot() +
  labs(title = "Questionnaire Marks Boxplots", x = "Academic Year", y = "Mark (0-10)", fill = "Mark Type") +
  theme_minimal()
save_plot(plot_marks_boxplots, "2_questionnaire_marks_boxplots.png", fig_width * 1.2, fig_height * 1.2, fig_resolution)
cat(paste0("Questionnaire marks boxplots saved to ", output_folder_name, "/2_questionnaire_marks_boxplots.png\n\n"))

analysis_output_2 <- capture.output({
  cat("### 2.3. Difference between 1st and 2nd Semester Marks ###\n\n")
  
  cat("--- Descriptive Statistics for 1Q and 2Q Marks ---\n")
  desc_stats_2q <- df_surveys %>%
    dplyr::summarise(
      Mean_X1Q = mean(!!sym(make.names(first_q_mark_col)), na.rm = TRUE),
      Median_X1Q = median(!!sym(make.names(first_q_mark_col)), na.rm = TRUE),
      Mean_X2Q = mean(!!sym(make.names(second_q_mark_col)), na.rm = TRUE),
      Median_X2Q = median(!!sym(make.names(second_q_mark_col)), na.rm = TRUE)
    )
  print(desc_stats_2q)
  cat("\n")
  
  # Check for paired normality
  diff_col_name <- "Mark_Difference"
  df_temp_diff <- df_surveys %>%
    dplyr::filter(!is.na(!!sym(make.names(first_q_mark_col))) & !is.na(!!sym(make.names(second_q_mark_col)))) %>%
    dplyr::mutate(!!diff_col_name := !!sym(make.names(first_q_mark_col)) - !!sym(make.names(second_q_mark_col)))
  
  shapiro_test_diff <- tryCatch(
    rstatix::shapiro_test(df_temp_diff, !!sym(diff_col_name)),
    error = function(e) {
      cat("Warning: Shapiro-Wilk test could not be performed due to insufficient data.\n")
      return(tibble(p = 0)) # Force non-parametric test
    }
  )
  
  if (shapiro_test_diff$p > 0.05) {
    cat("Normal difference, using paired Student's t-test.\n")
    test_result <- t.test(df_surveys[[make.names(first_q_mark_col)]], df_surveys[[make.names(second_q_mark_col)]], paired = TRUE)
  } else {
    cat("Non-normal difference, using paired Wilcoxon test.\n")
    test_result <- wilcox.test(df_surveys[[make.names(first_q_mark_col)]], df_surveys[[make.names(second_q_mark_col)]], paired = TRUE)
  }
  print(test_result)
})
writeLines(analysis_output_2, file.path(output_dir, "2_1Q_vs_2Q_marks_test.txt"))
cat(paste0("1Q vs 2Q comparison results saved to ", output_folder_name, "/2_1Q_vs_2Q_marks_test.txt\n\n"))

#-------------------------------------------------------------------
# 2.4 Prior Knowledge (1Q_Mark and 2Q_Mark): Before vs. After
#     New Methodology (COVID-influenced years excluded)
#-------------------------------------------------------------------

analyze_period_mark_comparison <- function(df, mark_col_name, mark_label, file_prefix) {
  
  analysis_output <- capture.output({
    cat(paste0("### 2.4. Inferential Analysis: ", mark_label,
               " Before vs. After New Methodology (COVID years excluded) ###\n\n"))
    
    df_analysis <- df %>%
      dplyr::filter(Period %in% c("Before_New_Methodology", "After_New_Methodology"),
                    CovidPeriod != "Pandemic") %>%
      dplyr::rename(mark_value = !!mark_col_name) %>%
      dplyr::filter(!is.na(mark_value))
    
    cat(paste0("--- Descriptive Statistics for ", mark_label, " by Period ---\n"))
    desc_stats <- df_analysis %>%
      dplyr::group_by(Period) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Mean = mean(mark_value, na.rm = TRUE),
        Median = median(mark_value, na.rm = TRUE),
        SD = sd(mark_value, na.rm = TRUE),
        .groups = 'drop'
      )
    print(desc_stats)
    cat("\n")
    
    cat("--- Normality Test (Shapiro-Wilk) ---\n")
    normality_test <- df_analysis %>%
      dplyr::group_by(Period) %>% rstatix::shapiro_test(mark_value)
    print(normality_test)
    
    cat("\n--- Homogeneity of Variances Test (Levene's) ---\n")
    levene_test <- tryCatch({
      car::leveneTest(mark_value ~ Period, data = df_analysis)
    }, error = function(e) {
      message("Levene's test could not be performed. Not enough data per group.")
      return(NULL)
    })
    if (!is.null(levene_test)) {
      print(levene_test)
    }
    
    use_parametric <- all(normality_test$p > 0.05, na.rm = TRUE)
    
    if (use_parametric) {
      cat("\n--- Student's t-test (assuming Normality) ---\n")
      test_result <- t.test(mark_value ~ Period, data = df_analysis)
    } else {
      cat("\n--- Mann-Whitney U Test (Non-parametric) ---\n")
      test_result <- wilcox.test(mark_value ~ Period, data = df_analysis)
    }
    print(test_result)
    
    cohen_d_result <- rstatix::cohens_d(df_analysis, mark_value ~ Period)
    cat("\n--- Effect Size (Cohen's d) ---\n")
    print(cohen_d_result)
    
    list(df_analysis = df_analysis, desc_stats = desc_stats,
         test_result = test_result, use_parametric = use_parametric,
         cohen_d_result = cohen_d_result)
  })
  
  # Re-run silently to capture the returned objects (capture.output discards the return value)
  results <- withCallingHandlers({
    df_analysis <- df %>%
      dplyr::filter(Period %in% c("Before_New_Methodology", "After_New_Methodology"),
                    CovidPeriod != "Pandemic") %>%
      dplyr::rename(mark_value = !!mark_col_name) %>%
      dplyr::filter(!is.na(mark_value))
    
    desc_stats <- df_analysis %>%
      dplyr::group_by(Period) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Mean = mean(mark_value, na.rm = TRUE),
        Median = median(mark_value, na.rm = TRUE),
        SD = sd(mark_value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    normality_test <- df_analysis %>%
      dplyr::group_by(Period) %>% rstatix::shapiro_test(mark_value)
    
    use_parametric <- all(normality_test$p > 0.05, na.rm = TRUE)
    
    test_result <- if (use_parametric) {
      t.test(mark_value ~ Period, data = df_analysis)
    } else {
      wilcox.test(mark_value ~ Period, data = df_analysis)
    }
    
    cohen_d_result <- rstatix::cohens_d(df_analysis, mark_value ~ Period)
    
    list(df_analysis = df_analysis, desc_stats = desc_stats,
         test_result = test_result, use_parametric = use_parametric,
         cohen_d_result = cohen_d_result)
  }, message = function(m) invokeRestart("muffleMessage"))
  
  writeLines(analysis_output, file.path(output_dir, paste0("2_", file_prefix, "_before_after_methodology.txt")))
  cat(paste0(mark_label, " Before vs. After comparison saved to ", output_folder_name,
             "/2_", file_prefix, "_before_after_methodology.txt\n\n"))
  
  # --- Boxplot ---
  plot_obj <- ggplot(results$df_analysis, aes(x = Period, y = mark_value, fill = Period)) +
    geom_boxplot() +
    labs(title = paste0(mark_label, " Before vs. After New Methodology"),
         subtitle = "COVID-influenced years excluded",
         x = "Period", y = mark_label) +
    theme_minimal() +
    theme(legend.position = "none")
  save_plot(plot_obj, paste0("2_", file_prefix, "_before_after_boxplot.png"), fig_width, fig_height, fig_resolution)
  cat(paste0(mark_label, " boxplot saved to ", output_folder_name,
             "/2_", file_prefix, "_before_after_boxplot.png\n\n"))
  
  # --- Plain-language summary ---
  desc_stats <- results$desc_stats
  test_result <- results$test_result
  use_parametric <- results$use_parametric
  cohen_d_result <- results$cohen_d_result
  
  summary_lines <- c(
    paste0("SUMMARY: ", mark_label, " Before vs. After New Methodology"),
    "(COVID-influenced years excluded)",
    "",
    paste0("Sample sizes: Before = ", desc_stats$N[desc_stats$Period == "Before_New_Methodology"],
           ", After = ", desc_stats$N[desc_stats$Period == "After_New_Methodology"]),
    paste0("Mean ", mark_label, " - Before: ", round(desc_stats$Mean[desc_stats$Period == "Before_New_Methodology"], 2),
           " (SD = ", round(desc_stats$SD[desc_stats$Period == "Before_New_Methodology"], 2), ")"),
    paste0("Mean ", mark_label, " - After:  ", round(desc_stats$Mean[desc_stats$Period == "After_New_Methodology"], 2),
           " (SD = ", round(desc_stats$SD[desc_stats$Period == "After_New_Methodology"], 2), ")"),
    ""
  )
  
  test_name <- if (use_parametric) "Student's t-test" else "Mann-Whitney U test"
  p_value <- test_result$p.value
  
  summary_lines <- c(summary_lines,
                     paste0("Test used: ", test_name, " (chosen based on Shapiro-Wilk normality results)"),
                     paste0("p-value: ", signif(p_value, 4))
  )
  
  if (p_value < 0.05) {
    mean_before <- desc_stats$Mean[desc_stats$Period == "Before_New_Methodology"]
    mean_after  <- desc_stats$Mean[desc_stats$Period == "After_New_Methodology"]
    direction_txt <- if (mean_after > mean_before) "HIGHER" else "LOWER"
    
    summary_lines <- c(summary_lines,
                       "",
                       paste0("RESULT: The difference in ", mark_label, " between periods IS statistically significant (p < 0.05)."),
                       paste0("Students in the 'After_New_Methodology' period show ", direction_txt,
                              " ", mark_label, " on average compared to 'Before_New_Methodology'.")
    )
  } else {
    summary_lines <- c(summary_lines,
                       "",
                       paste0("RESULT: The difference in ", mark_label, " between periods is NOT statistically significant (p >= 0.05)."),
                       paste0("This suggests that, based on the available data, ", mark_label,
                              " levels were comparable before and after the introduction of the new (DEAba) methodology,"),
                       "excluding COVID-influenced years."
    )
  }
  
  abs_d <- abs(cohen_d_result$effsize)
  effect_size_label <- dplyr::case_when(
    abs_d < 0.2 ~ "negligible",
    abs_d < 0.5 ~ "small",
    abs_d < 0.8 ~ "medium",
    TRUE ~ "large"
  )
  
  summary_lines <- c(summary_lines,
                     "",
                     paste0("Effect size (Cohen's d): ", round(cohen_d_result$effsize, 3), " (", effect_size_label, ")"),
                     "",
                     "Interpretation note:",
                     paste0("This analysis addresses whether observed changes in course outcomes between the 'Before' and"),
                     paste0("'After' methodology periods could be confounded by differences in students' ", mark_label,
                            ", rather than being attributable to the DEAba methodology itself."),
                     paste0("See the accompanying boxplot: 2_", file_prefix, "_before_after_boxplot.png")
  )
  
  writeLines(summary_lines, file.path(output_dir, paste0("2_", file_prefix, "_before_after_summary.txt")))
  cat(paste0(mark_label, " results summary saved to ", output_folder_name,
             "/2_", file_prefix, "_before_after_summary.txt\n\n"))
}

# Run for both quarters
analyze_period_mark_comparison(df_surveys, make.names(first_q_mark_col), "1Q_Mark (Prior Knowledge)", "prior_knowledge_1Q")
analyze_period_mark_comparison(df_surveys, make.names(second_q_mark_col), "2Q_Mark", "second_quarter_mark")

df_perceptions_long <- df_surveys %>%
  dplyr::select(!!sym(make.names(academic_year_col_surveys)), all_of(surveys_perception_cols)) %>%
  tidyr::pivot_longer(cols = all_of(surveys_perception_cols), names_to = "Question", values_to = "Response") %>%
  dplyr::mutate(
    Response = as.numeric(Response),
    Question = factor(Question, levels = surveys_perception_cols)
  )
plot_perceptions_boxplots <- ggplot(df_perceptions_long, aes(x = Question, y = Response)) +
  geom_boxplot() +
  facet_wrap(as.formula(paste("~", make.names(academic_year_col_surveys)))) +
  labs(title = "Boxplots of Perception Responses per Year", x = "Question", y = "Response (1-10)") +
  coord_cartesian(ylim = c(0, 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
save_plot(plot_perceptions_boxplots, "2_perceptions_boxplots.png", fig_width * 1.2, fig_height * 1.2, fig_resolution)

plot_perceptions_boxplots_global <- ggplot(df_perceptions_long, aes(x = Question, y = Response)) +
  geom_boxplot() +
  labs(title = "Boxplots of Perception Responses (All Years)", x = "Question", y = "Response (1-10)") +
  coord_cartesian(ylim = c(0, 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
save_plot(plot_perceptions_boxplots_global, "2_perceptions_boxplots_global.png", fig_width * 1.2, fig_height * 1.2, fig_resolution)
cat("Perception response boxplots saved with '2_' prefix.\n\n")

analysis_output_3 <- capture.output({
  cat("### 2.5. Analysis of Perceptions by COVID Period ###\n\n")
  
  df_period_analysis <- df_surveys %>%
    # Use make.names() to ensure a valid column name
    dplyr::filter(CovidPeriod %in% c("Pre_Pandemic", "Pandemic", "Post_Pandemic")) %>%
    dplyr::select(CovidPeriod, all_of(surveys_perception_cols)) %>%
    tidyr::pivot_longer(
      cols = all_of(surveys_perception_cols),
      names_to = "Question",
      values_to = "Response"
    ) %>%
    dplyr::mutate(Response = as.numeric(Response))
  
  df_period_results <- df_period_analysis %>%
    dplyr::group_by(Question) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      kruskal_result = purrr::map(data, ~ {
        filtered_data <- na.omit(.x)
        if (n_distinct(filtered_data$CovidPeriod) >= 2 && nrow(filtered_data) >= 2) {
          result <- tryCatch(
            kruskal.test(Response ~ CovidPeriod, data = filtered_data),
            error = function(e) {
              list(p.value = NA)
            }
          )
          result
        } else {
          list(p.value = NA)
        }
      }),
      p_value = purrr::map_dbl(kruskal_result, "p.value")
    ) %>%
    dplyr::select(Question, p_value) %>%
    dplyr::mutate(p_adj = p.adjust(p_value, method = "BH"))
  
  print(df_period_results)
})
writeLines(analysis_output_3, file.path(output_dir, "2_perceptions_covid_analysis.txt"))
cat(paste0("Analysis of perceptions by COVID period saved to ", output_folder_name, "/2_perceptions_covid_analysis.txt\n\n"))

analysis_output_4 <- capture.output({
  cat("### 2.6. Correlation between Perceptions (Q1-Q18) and Final Mark ###\n\n")
  
  # Join marks and surveys data
  df_correlation <- df_surveys %>%
    dplyr::inner_join(df_marks, by = c(make.names(academic_year_col_surveys), make.names(student_id_col))) %>%
    dplyr::select(all_of(c(make.names(mark_col), surveys_perception_cols))) %>%
    dplyr::mutate_at(vars(all_of(surveys_perception_cols)), as.numeric) %>%
    na.omit()
  
  # Check if there is enough data for correlation
  if (nrow(df_correlation) > 1) {
    cor_results_list <- list()
    for (q in surveys_perception_cols) {
      cor_result <- tryCatch(
        cor.test(df_correlation[[make.names(mark_col)]], df_correlation[[q]], method = "pearson"),
        error = function(e) {
          list(p.value = NA, estimate = NA)
        }
      )
      
      cor_results_list[[q]] <- tibble(
        Question = q,
        p_value = cor_result$p.value,
        estimate = cor_result$estimate
      )
    }
    
    if (length(cor_results_list) > 0) {
      cor_results_df <- do.call(rbind, cor_results_list) %>%
        dplyr::mutate(p_adj = p.adjust(p_value, method = "BH")) # BH Correction
      print(cor_results_df)
    } else {
      cat("Not enough data to calculate correlations.\n")
    }
    
  } else {
    cat("Not enough data to calculate correlations.\n")
  }
})
writeLines(analysis_output_4, file.path(output_dir, "2_correlations_all_perceptions_marks.txt"))
cat(paste0("All perceptions vs. final marks correlation analysis saved to ", output_folder_name, "/2_correlations_all_perceptions_marks.txt\n\n"))

analysis_output_5 <- capture.output({
  cat("### 2.7. Specific Correlations: Prior Knowledge (1Q_Mark) vs. Perceptions ###\n\n")
  
  cor_results_list <- list()
  
  for (q in prior_knowledge_perceptions_to_analyze) {
    df_temp <- df_surveys %>%
      dplyr::filter(!is.na(!!sym(make.names(first_q_mark_col))) & !is.na(!!sym(q))) %>%
      dplyr::mutate(q_numeric = as.numeric(!!sym(q)))
    
    if (nrow(df_temp) > 1) {
      cor_result <- cor.test(df_temp[[make.names(first_q_mark_col)]], df_temp$q_numeric, method = "pearson")
      cor_results_list[[q]] <- tibble(
        question = q,
        statistic = cor_result$statistic,
        p_value = cor_result$p.value,
        estimate = cor_result$estimate
      )
    } else {
      cat(paste0("Not enough data to calculate correlation for ", make.names(first_q_mark_col), " and ", q, ".\n\n"))
    }
  }
  
  if (length(cor_results_list) > 0) {
    cor_results_df <- do.call(rbind, cor_results_list) %>%
      dplyr::mutate(p_adj = p.adjust(p_value, method = "BH")) # BH Correction
    
    for (q in prior_knowledge_perceptions_to_analyze) {
      if (q %in% cor_results_df$question) {
        cat(paste0("--- Correlation between ", make.names(first_q_mark_col), " and ", q, " ---\n"))
        mean_X1Q_Mark <- mean(df_surveys[[make.names(first_q_mark_col)]], na.rm = TRUE)
        mean_Q_q <- mean(df_surveys[[q]], na.rm = TRUE)
        cat(paste0("Mean of ", make.names(first_q_mark_col), ": ", round(mean_X1Q_Mark, 2), "\n"))
        cat(paste0("Mean of ", q, ": ", round(mean_Q_q, 2), "\n"))
        print(cor_results_df %>% dplyr::filter(question == q))
        cat("\n")
      }
    }
  } else {
    cat("No correlations could be calculated due to insufficient data.\n")
  }
})
writeLines(analysis_output_5, file.path(output_dir, "2_prior_knowledge_perceptions_correlations.txt"))
cat(paste0("Specific correlations saved to ", output_folder_name, "/2_prior_knowledge_perceptions_correlations.txt\n\n"))

#======================================================================
# 6. Methodology Survey Analysis (2_BQM_DEASurvey_ordered.xlsx) - Outputs 3_
#======================================================================

cat("\n### 3. Analysis of DEA Methodology Survey ###\n\n")

analysis_output_dea_1 <- capture.output({
  cat("--- Student Count per Academic Year ---\n")
  student_count <- df_methodology %>%
    dplyr::group_by(!!sym(make.names(academic_year_col_methodology))) %>%
    dplyr::summarise(Surveyed = n(), .groups = 'drop')
  print(student_count)
  
  cat("\n--- Descriptive Statistics for DEA Survey Questions (Q1-Q5) ---\n")
  df_dea_long <- df_methodology %>%
    dplyr::select(!!sym(make.names(academic_year_col_methodology)), all_of(dea_perception_cols)) %>%
    tidyr::pivot_longer(cols = all_of(dea_perception_cols), names_to = "Question", values_to = "Response")
  
  desc_stats_dea <- df_dea_long %>%
    dplyr::group_by(!!sym(make.names(academic_year_col_methodology)), Question) %>%
    dplyr::summarise(
      Mean = mean(Response, na.rm = TRUE),
      Median = median(Response, na.rm = TRUE),
      SD = sd(Response, na.rm = TRUE),
      .groups = 'drop'
    )
  print(desc_stats_dea)
})
writeLines(analysis_output_dea_1, file.path(output_dir, "3_dea_descriptive_stats.txt"))
cat(paste0("DEA survey descriptive statistics saved to ", output_folder_name, "/3_dea_descriptive_stats.txt\n\n"))

df_dea_long <- df_methodology %>%
  dplyr::select(!!sym(make.names(academic_year_col_methodology)), all_of(dea_perception_cols)) %>%
  tidyr::pivot_longer(cols = all_of(dea_perception_cols), names_to = "Question", values_to = "Response") %>%
  dplyr::mutate(
    Response = as.numeric(Response),
    Question = factor(Question, levels = dea_perception_cols)
  )

plot_dea_boxplots_yearly <- ggplot(df_dea_long, aes(x = Question, y = Response)) +
  geom_boxplot() +
  facet_wrap(as.formula(paste("~", make.names(academic_year_col_methodology)))) +
  labs(title = "Boxplots of DEA Survey Responses per Year", x = "Question", y = "Response") +
  theme_minimal()
save_plot(plot_dea_boxplots_yearly, "3_dea_boxplots_yearly.png", fig_width, fig_height, fig_resolution)

plot_dea_boxplots_global <- ggplot(df_dea_long, aes(x = Question, y = Response)) +
  geom_boxplot() +
  labs(title = "Boxplots of DEA Survey Responses (All Years)", x = "Question", y = "Response") +
  theme_minimal()
save_plot(plot_dea_boxplots_global, "3_dea_boxplots_global.png", fig_width * 0.8, fig_height * 0.8, fig_resolution)

cat("DEA survey boxplots saved with '3_dea_' prefix.\n\n")

analysis_output_dea_2 <- capture.output({
  cat("--- Internal Correlations within DEA Survey (Q1-Q5) ---\n")
  df_dea_numeric <- df_methodology %>% dplyr::select(all_of(dea_perception_cols)) %>% dplyr::mutate_all(as.numeric)
  
  if (nrow(df_dea_numeric) >= 2) {
    cor_matrix <- cor(df_dea_numeric, use = "complete.obs")
    print(cor_matrix)
    
    cor_p_values <- Hmisc::rcorr(as.matrix(df_dea_numeric), type = "pearson")$P
    
    p_values_bh <- p.adjust(cor_p_values, method = "BH") # BH correction
    dim(p_values_bh) <- dim(cor_p_values)
    dimnames(p_values_bh) <- dimnames(cor_p_values)
    
    cat("\n--- Specific Pearson's Correlations ---\n")
    
    cor_q1_q4 <- cor.test(df_methodology$Q1, df_methodology$Q4, method = "pearson")
    cat("\nCorrelation between Q1 (Interest) and Q4 (Usefulness):\n")
    print(cor_q1_q4)
    cat(paste0("Adjusted p-value (BH): ", p_values_bh["Q1", "Q4"], "\n"))
    
    cor_q4_q5 <- cor.test(df_methodology$Q4, df_methodology$Q5, method = "pearson")
    cat("\nCorrelation between Q4 (Usefulness) and Q5 (Helped Theory):\n")
    print(cor_q4_q5)
    cat(paste0("Adjusted p-value (BH): ", p_values_bh["Q4", "Q5"], "\n"))
    
  } else {
    cat("Not enough data to calculate correlations.\n")
  }
})
writeLines(analysis_output_dea_2, file.path(output_dir, "3_dea_internal_correlations.txt"))
cat(paste0("DEA survey internal correlations saved to ", output_folder_name, "/3_dea_internal_correlations.txt\n\n"))

analysis_output_dea_3 <- capture.output({
  cat("--- Trend Analysis of DEA Perceptions across Years (Q1-Q5) ---\n")
  
  trend_results <- tibble()
  for (q in dea_perception_cols) {
    df_temp <- df_methodology %>%
      dplyr::select(!!sym(make.names(academic_year_col_methodology)), all_of(q)) %>%
      na.omit()
    
    if (n_distinct(df_temp[[make.names(academic_year_col_methodology)]]) > 1 && nrow(df_temp) > 2) {
      formula <- as.formula(paste(q, "~", make.names(academic_year_col_methodology)))
      kruskal_result <- kruskal.test(formula, data = df_temp)
      
      trend_results <- trend_results %>%
        bind_rows(tibble(
          Question = q,
          Kruskal_Chi_squared = kruskal_result$statistic,
          df = kruskal_result$parameter,
          p_value = kruskal_result$p.value
        ))
    }
  }
  
  if(nrow(trend_results) > 0) {
    trend_results <- trend_results %>%
      dplyr::mutate(p_adj = p.adjust(p_value, method = "BH")) # BH Correction
    print(trend_results)
  } else {
    cat("Not enough data to run trend analysis for any question.\n")
  }
})
writeLines(analysis_output_dea_3, file.path(output_dir, "3_dea_trend_analysis.txt"))
cat(paste0("DEA survey trend analysis saved to ", output_folder_name, "/3_dea_trend_analysis.txt\n\n"))


#======================================================================
# 7. Degree Indices Analysis (3_BiologyIndices_ordered.xlsx) - Outputs 4_
#======================================================================

cat("\n### 4. Analysis of Degree Indices ###\n\n")

yearly_averages_marks <- df_marks %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_marks))) %>%
  dplyr::summarise(Avg_Final_Mark = mean(!!sym(make.names(mark_col)), na.rm = TRUE), .groups = 'drop')

yearly_averages_surveys <- df_surveys %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_surveys))) %>%
  dplyr::summarise(
    Avg_Global_Mark = mean(!!sym(make.names(global_mark_col)), na.rm = TRUE),
    Avg_X1Q_Mark = mean(!!sym(make.names(first_q_mark_col)), na.rm = TRUE),
    Avg_2Q_Mark = mean(!!sym(make.names(second_q_mark_col)), na.rm = TRUE),
    .groups = 'drop'
  )

yearly_averages_dea <- df_methodology %>%
  dplyr::group_by(!!sym(make.names(academic_year_col_methodology))) %>%
  dplyr::summarise(
    Avg_Q1_DEA = mean(!!sym("Q1"), na.rm = TRUE),
    Avg_Q4_DEA = mean(!!sym("Q4"), na.rm = TRUE),
    Avg_Q5_DEA = mean(!!sym("Q5"), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::rename(!!make.names(academic_year_col_marks) := !!sym(make.names(academic_year_col_methodology)))

df_all_yearly_data <- dplyr::left_join(yearly_averages_marks, yearly_averages_surveys, by = make.names(academic_year_col_marks)) %>%
  dplyr::left_join(yearly_averages_dea, by = make.names(academic_year_col_marks)) %>%
  dplyr::left_join(df_degree_wide, by = make.names(academic_year_col_marks))

df_all_yearly_data$Numeric.Year <- as.numeric(stringr::str_extract(df_all_yearly_data[[make.names(academic_year_col_marks)]], "^\\d{4}"))

analysis_output_biology_trends <- capture.output({
  cat("### 4.1. Analysis of Biology Indices Trends ###\n\n")
  indices_to_analyze <- c("Graduation.Rate", "Dropout.Rate", "Performance.Rate", "Efficiency.Rate")
  
  print(df_all_yearly_data %>% dplyr::select(!!sym(make.names(academic_year_col_marks)), all_of(indices_to_analyze)))
})
writeLines(analysis_output_biology_trends, file.path(output_dir, "4_biology_indices_trends.txt"))
cat(paste0("Biology indices trend analysis saved to ", output_folder_name, "/4_biology_indices_trends.txt\n\n"))

df_biology_long_for_plot <- df_all_yearly_data %>%
  dplyr::select(!!sym(make.names(academic_year_col_marks)), Graduation.Rate, Dropout.Rate, Performance.Rate, Efficiency.Rate) %>%
  tidyr::pivot_longer(cols = -!!sym(make.names(academic_year_col_marks)), names_to = "Index", values_to = "Value")

plot_biology_trends <- ggplot(df_biology_long_for_plot, aes(x = !!sym(make.names(academic_year_col_marks)), y = Value, group = Index, color = Index)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Key Degree Program Indices", x = "Academic Year", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(plot_biology_trends, "4_biology_indices_trends.png", fig_width, fig_height, fig_resolution)
cat(paste0("Biology indices trend plot saved as ", output_folder_name, "/4_biology_indices_trends.png\n\n"))

analysis_output_correlations <- capture.output({
  cat("### 4.2. Correlations between Degree Indices and DEA Perceptions ###\n\n")
  
  df_dea_indices_corr <- df_all_yearly_data %>%
    dplyr::filter(!is.na(Avg_Q1_DEA)) %>%
    dplyr::select(!!sym(make.names(academic_year_col_marks)), Graduation.Rate, Dropout.Rate, Performance.Rate, Efficiency.Rate, Avg_Q1_DEA, Avg_Q4_DEA, Avg_Q5_DEA, Numeric.Year)
  
  cat("--- Yearly data being correlated ---\n")
  print(df_dea_indices_corr)
  
  dea_perceptions_avg <- c("Avg_Q1_DEA", "Avg_Q4_DEA", "Avg_Q5_DEA")
  indices_to_analyze <- c("Graduation.Rate", "Dropout.Rate", "Performance.Rate", "Efficiency.Rate")
  
  cor_results_list <- list()
  
  for (index in indices_to_analyze) {
    for (perception in dea_perceptions_avg) {
      df_temp_corr <- df_dea_indices_corr %>%
        dplyr::select(all_of(index), all_of(perception)) %>%
        na.omit()
      
      if (nrow(df_temp_corr) >= 2) {
        result <- tryCatch({
          cor_test_result <- cor.test(df_temp_corr[[index]], df_temp_corr[[perception]])
          tibble(
            Index = index,
            Perception = perception,
            estimate = cor_test_result$estimate,
            p_value = cor_test_result$p.value
          )
        }, error = function(e) {
          tibble(Index = index, Perception = perception, estimate = NA, p_value = NA, error = e$message)
        })
        cor_results_list <- append(cor_results_list, list(result))
      } else {
        cat(paste0("Not enough data points (min 2 years) to calculate a meaningful correlation for ", index, " and ", perception, ".\n"))
      }
    }
  }
  
  if (length(cor_results_list) > 0) {
    cor_results_df <- do.call(rbind, cor_results_list)
    if (!("error" %in% names(cor_results_df))) {
      cor_results_df <- cor_results_df %>%
        dplyr::mutate(p_adj = p.adjust(p_value, method = "BH"))
    }
    print(cor_results_df)
  } else {
    cat("No correlations could be calculated due to insufficient data.\n")
  }
})
writeLines(analysis_output_correlations, file.path(output_dir, "4_correlations_biology_dea.txt"))
cat(paste0("Correlations between biology indices and DEA perceptions saved to ", output_folder_name, "/4_correlations_biology_dea.txt\n\n"))

analysis_output_new_marks_dea <- capture.output({
  cat("### 4.3. Correlation between Average Final Mark and Average DEA Perceptions ###\n\n")
  
  df_marks_dea_corr <- df_all_yearly_data %>%
    dplyr::select(!!sym(make.names(academic_year_col_marks)), Avg_Final_Mark, Avg_Q1_DEA, Avg_Q4_DEA, Avg_Q5_DEA) %>%
    na.omit()
  
  cat("--- Yearly data used for correlation ---\n")
  print(df_marks_dea_corr)
  
  if (nrow(df_marks_dea_corr) >= 2) {
    perceptions_to_correlate <- c("Avg_Q1_DEA", "Avg_Q4_DEA", "Avg_Q5_DEA")
    
    cor_results_list <- list()
    for (perception in perceptions_to_correlate) {
      result <- cor.test(df_marks_dea_corr$Avg_Final_Mark, df_marks_dea_corr[[perception]])
      cor_results_list[[perception]] <- tibble(
        Perception = perception,
        estimate = result$estimate,
        p_value = result$p.value
      )
    }
    
    cor_results_df <- do.call(rbind, cor_results_list) %>%
      dplyr::mutate(p_adj = p.adjust(p_value, method = "BH")) # BH Correction
    
    print(cor_results_df)
  } else {
    cat("\nNot enough data points (min 2 years) to calculate a meaningful correlation.\n")
  }
})
writeLines(analysis_output_new_marks_dea, file.path(output_dir, "4_correlations_marks_dea.txt"))
cat(paste0("Correlations between average final mark and DEA perceptions saved to ", output_folder_name, "/4_correlations_marks_dea.txt\n\n"))

analysis_output_cross <- capture.output({
  cat("### 4.4. Cross-File Correlations between Avg Marks and Avg Perceptions ###\n")
  
  cat("\n--- Correlation between Final Mark and DEA Perceptions ---\n")
  df_corr_marks_dea <- df_all_yearly_data %>%
    dplyr::filter(!is.na(Avg_Final_Mark) & !is.na(Avg_Q1_DEA) & !is.na(Avg_Q4_DEA))
  print(df_corr_marks_dea %>% dplyr::select(!!sym(make.names(academic_year_col_marks)), Avg_Final_Mark, Avg_Q1_DEA, Avg_Q4_DEA))
  
  if (nrow(df_corr_marks_dea) >= 2) {
    cat("\nCorrelation between Avg_Final_Mark and Avg_Q1_DEA (Interest):\n")
    print(cor.test(df_corr_marks_dea$Avg_Final_Mark, df_corr_marks_dea$Avg_Q1_DEA))
    
    cat("\nCorrelation between Avg_Final_Mark and Avg_Q4_DEA (Usefulness):\n")
    print(cor.test(df_corr_marks_dea$Avg_Final_Mark, df_corr_marks_dea$Avg_Q4_DEA))
  } else {
    cat("Not enough data points (min 2 years) to calculate correlations.\n")
  }
  
  cat("\n--- Correlation between Prior Knowledge (1Q Mark) and DEA Perceptions ---\n")
  df_corr_prior_dea <- df_all_yearly_data %>%
    dplyr::filter(!is.na(Avg_X1Q_Mark) & !is.na(Avg_Q1_DEA) & !is.na(Avg_Q4_DEA))
  print(df_corr_prior_dea %>% dplyr::select(!!sym(make.names(academic_year_col_marks)), Avg_X1Q_Mark, Avg_Q1_DEA, Avg_Q4_DEA))
  
  if (nrow(df_corr_prior_dea) >= 2) {
    cat("\nCorrelation between Avg_X1Q_Mark and Avg_Q1_DEA (Interest):\n")
    print(cor.test(df_corr_prior_dea$Avg_X1Q_Mark, df_corr_prior_dea$Avg_Q1_DEA))
    
    cat("\nCorrelation between Avg_X1Q_Mark and Avg_Q4_DEA (Usefulness):\n")
    print(cor.test(df_corr_prior_dea$Avg_X1Q_Mark, df_corr_prior_dea$Avg_Q4_DEA))
  } else {
    cat("Not enough data points (min 2 years) to calculate correlations.\n")
  }
})
writeLines(analysis_output_cross, file.path(output_dir, "4_crossfile_dea_correlations.txt"))
cat(paste0("Cross-file correlations saved to ", output_folder_name, "/4_crossfile_dea_correlations.txt\n\n"))

#======================================================================
# 8. Regression Models - Outputs 8_
#======================================================================

cat("\n### 8. Regression Models ###\n\n")

analysis_output_regression <- capture.output({
  cat("### 8.1. Regression Analysis: Impact of Perceptions and Methodology on Final Mark ###\n")
  
  # Join marks and surveys, filtering for the "Before" and "After" periods only
  df_merged_regression <- df_marks %>%
    dplyr::inner_join(df_surveys, by = c(make.names(academic_year_col_marks), make.names(student_id_col))) %>%
    dplyr::filter(Period.x %in% c("Before_New_Methodology", "After_New_Methodology"))
  
  # Select relevant variables and ensure consistent perception columns are numeric
  df_regression <- df_merged_regression %>%
    dplyr::select(
      !!sym(make.names(mark_col)),
      Period.x,
      all_of(consistent_perception_cols)
    ) %>%
    dplyr::mutate(dplyr::across(all_of(consistent_perception_cols), as.numeric)) %>%
    na.omit()
  
  # Rename the mark column to a safe name for the formula
  df_regression <- df_regression %>%
    dplyr::rename(mark_to_predict = !!make.names(mark_col))
  
  num_predictors <- length(consistent_perception_cols) + 1 # +1 for Period.x
  if (nrow(df_regression) > num_predictors && n_distinct(df_regression$Period.x) > 1) {
    
    predictors_formula <- paste(c("Period.x", consistent_perception_cols), collapse = " + ")
    regression_formula <- as.formula(paste("mark_to_predict", "~", predictors_formula))
    
    cat(paste0("Running regression model with ", nrow(df_regression), " observations.\n"))
    cat("The model formula is:\n")
    print(regression_formula)
    cat("\n")
    
    regression_model <- lm(regression_formula, data = df_regression)
    
    cat("--- Variance Inflation Factor (VIF) ---\n")
    vif_results <- tryCatch(
      car::vif(regression_model),
      error = function(e) {
        cat("Error calculating VIF: Not enough observations or perfect collinearity among predictors.\n")
        return(NULL)
      }
    )
    if (!is.null(vif_results)) {
      print(vif_results)
      cat("\n")
    }
    
    cat("--- Regression Model Summary ---\n")
    model_summary <- summary(regression_model)
    print(model_summary)
    
    plot_residuals <- ggplot(regression_model, aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    save_plot(plot_residuals, "8_regression_residuals.png", fig_width, fig_height, fig_resolution)
    
  } else {
    cat("Not enough data to run a meaningful regression model.\n")
    cat("Please ensure there are at least two distinct methodology periods (Before/After) ")
    cat("and enough observations to perform the regression.\n")
  }
})
writeLines(analysis_output_regression, file.path(output_dir, "8_regression_model_results.txt"))
cat(paste0("Regression model results saved to ", output_folder_name, "/8_regression_model_results.txt\n\n"))

#======================================================================
# 9. Lagged Analysis: Entrance Marks vs. Course Marks - Outputs 5_
#======================================================================

cat("\n### 9. Lagged Analysis: Entrance Marks vs. Course Marks ###\n\n")

analysis_output_lagged <- capture.output({
  df_lagged_data <- df_all_yearly_data %>%
    dplyr::select(
      !!sym(make.names(academic_year_col_marks)),
      Avg_Final_Mark,
      Avg_2Q_Mark,
      Avg_Entrance_Mark
    ) %>%
    na.omit()
  
  cat("--- Data used for lagged analysis (0-year lag) ---\n")
  print(df_lagged_data)
  
  if (nrow(df_lagged_data) >= 2) {
    cat("\n--- Correlation between Final Mark and Entrance Mark ---\n")
    cor_final_lagged <- cor.test(df_lagged_data$Avg_Final_Mark, df_lagged_data$Avg_Entrance_Mark)
    print(cor_final_lagged)
    
    cat("\n--- Correlation between 2nd Semester Mark and Entrance Mark ---\n")
    cor_2nd_semester_lagged <- cor.test(df_lagged_data$Avg_2Q_Mark, df_lagged_data$Avg_Entrance_Mark)
    print(cor_2nd_semester_lagged)
    
  } else {
    cat("Not enough data points (min 2 years) to calculate correlations.\n")
  }
})
writeLines(analysis_output_lagged, file.path(output_dir, "5_lagged_correlations.txt"))
cat(paste0("Lagged correlation analysis saved to ", output_folder_name, "/5_lagged_correlations.txt\n\n"))

if (exists("df_lagged_data") && nrow(df_lagged_data) >= 2) {
  df_plot_long <- df_lagged_data %>%
    tidyr::pivot_longer(
      cols = c(Avg_Final_Mark, Avg_2Q_Mark),
      names_to = "Mark_Type",
      values_to = "Mark_Value"
    )
  
  plot_lagged <- ggplot(df_plot_long, aes(x = Avg_Entrance_Mark, y = Mark_Value, color = Mark_Type)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c("Avg_Final_Mark" = "steelblue", "Avg_2Q_Mark" = "orange"),
                       labels = c("Avg_Final_Mark" = "Final Mark", "Avg_2Q_Mark" = "2nd Semester Mark")) +
    labs(
      title = "Correlation: Course Marks vs. Entrance Mark",
      subtitle = "Each point represents an academic year",
      x = "Average Entrance Mark",
      y = "Average Course Mark",
      color = "Mark Type"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  save_plot(plot_lagged, "5_lagged_scatterplot.png", fig_width, fig_height, fig_resolution)
  cat(paste0("Correlation scatter plot saved as ", output_folder_name, "/5_lagged_scatterplot.png\n\n"))
} else {
  cat("Not enough data to generate the lagged correlation scatter plot.\n\n")
}

#======================================================================
# 10. Visualization of Marks and Entrance Mark Trends - Outputs 6_
#======================================================================

cat("\n### 10. Visualization of Mark and Entrance Mark Trends ###\n\n")

df_marks_long_for_plot <- df_all_yearly_data %>%
  dplyr::select(!!sym(make.names(academic_year_col_marks)), Avg_Final_Mark, Avg_2Q_Mark, Avg_Entrance_Mark) %>%
  tidyr::pivot_longer(
    cols = -!!sym(make.names(academic_year_col_marks)),
    names_to = "Mark_Type",
    values_to = "Mark_Value"
  )

plot_marks_trends <- ggplot(df_marks_long_for_plot, aes(x = !!sym(make.names(academic_year_col_marks)), y = Mark_Value, group = Mark_Type, color = Mark_Type)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = c("Avg_Final_Mark" = "steelblue", "Avg_2Q_Mark" = "orange", "Avg_Entrance_Mark" = "darkgreen"),
                     labels = c("Avg_Final_Mark" = "Final Mark (Average)", "Avg_2Q_Mark" = "2nd Semester Mark (Average)", "Avg_Entrance_Mark" = "Entrance Mark (Average)")) +
  labs(
    title = "Trends of Course and Entrance Marks",
    subtitle = "Average marks by academic year",
    x = "Academic Year",
    y = "Average Score",
    color = "Mark Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(plot_marks_trends, "6_marks_and_entrance_trends.png", fig_width, fig_height, fig_resolution)
cat(paste0("Marks and entrance trends plot saved as ", output_folder_name, "/6_marks_and_entrance_trends.png
"))