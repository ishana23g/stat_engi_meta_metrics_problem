library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# assuming some data is in a file saved in "student_responses.csv"
file_path <- "student_responses.csv"

# Actually checking if it exists.
# In this case it doesn't exist, so we can just generate some random data to
# help fill in and test how it goes.
if (file.exists(file_path)) {
  # If the file exists, read it into a tibble
  cat("File 'student_responses.csv' found. Loading data.\n")
  student_data <- read_csv(file_path, show_col_types = FALSE)
} else {
  # If the file doesn't exist, generate a random data set
  cat("File not found. Generating a random dataset.\n")
  
  num_students <- 1000
  num_tests <- 10
  # generate a random number of 1's for each test items
  set.seed(42) # for reproducibility
  test_perc_correct <- runif(num_tests, min = .2, max = 0.99)
  test_correct <- floor(test_perc_correct * num_students)
  
  # creating a tibble with student IDs and their responses
  student_data <- tibble(student_id = paste0("student_", 1:num_students)) %>%
    bind_cols(as_tibble(
      sapply(test_correct, function(correct) {
        # Generate a binary response for each test item
        responses <- c(rep(1, correct), rep(0, num_students - correct))
        sample(responses)  # Shuffle the responses
      }),
      .name_repair = ~ paste0("item_", 1:num_tests)
    ))
  
  # Save the newly generated data for future use
  write_csv(student_data, file_path)
  cat("New random dataset saved as 'student_responses.csv'.\n")
}

# make sure that colums with item_1 to item_10 are binary factors
student_data <- student_data %>%
  mutate(across(starts_with("item_"), ~ factor(.x, levels = c(0, 1))))

# do a quick look at the data
print(head(student_data))

# see the structure of the data set
str(student_data)

# print out the summary statistics
summary(student_data)

# Some example analyses that can be done
# --------------------------------------

# some more of the basic exploratory data analysis
# do a basic pair wise correlation between items in a
# plot with ggplot2


# calculate a total score for each student
# rank the top students
# visualize the distribution of scores
# 