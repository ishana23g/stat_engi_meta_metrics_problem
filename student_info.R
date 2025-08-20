library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# assuming some data is in a file saved in "student_responses.csv"
# include the path to the file
curr_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
file_path <- file.path(curr_path, "student_responses.csv")

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
      .name_repair = ~ paste0("test_", 1:num_tests)
    ))
  
  # Save the newly generated data for future use
  write_csv(student_data, file_path)
  cat("New random dataset saved as 'student_responses.csv'.\n")
}


# do a quick look at the data
print(head(student_data))

# see the structure of the data set
str(student_data)

# print out the summary statistics
summary(student_data)

# Some example analyses that can be done
# Main question trying to answer is:
# How would you conduct an initial evaluation of item or test performance?
# --------------------------------------


# calculate a total score for each student
# NOTE that I am keeping the data as numeric for the 0s and 1s
# so that I can add them easily together
student_scores <- student_data %>%
  rowwise() %>%
  mutate(total_score = sum(c_across(starts_with("test_")))) %>%
  ungroup()

# view the first few rows of the scores
print(head(student_scores))

# rank the top students
top_students <- student_scores %>%
  arrange(desc(total_score)) %>%
  slice_head(n = 10)
print(top_students)

# rank the bottom students
bottom_students <- student_scores %>%
  arrange(total_score) %>%
  slice_head(n = 10)
print(bottom_students)


# looking at these individual students also trying to group students together that are all having 
# similar scores could be interesting with a corr matrix between students, i.e. just doing a 
# inner product between one student's set of scores and another's scores.

# visualize the distribution of scores
ggplot(student_scores, aes(x = total_score)) +
  geom_histogram(binwidth = 1, fill = "#00BFC4", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Scores", x = "Total Score", y = "Number of Students") +
  theme_minimal()

# from this we can see get a read of the majority of students and where most of them 
# fall score wise.

# Another graph to look at is a box plot with jittered points that can help showcase where the
# majority, minority, and if there is some kind of weird spread in the data.

ggplot(student_scores, aes(x = "", y = total_score)) +
  geom_boxplot(outlier.shape = NA, fill = "#00BFC4", alpha = 0.7) +
  geom_jitter(width = 0.1, color = "black", alpha = 0.5) +
  labs(title = "Box Plot of Total Scores", x = "", y = "Total Score") +
  theme_minimal()

# in this case I would say this plot does not help actually give much information about the data.
# However, we can tell that getting a really low score such as a 3 or even a full 10 
# is pretty rare. 
# Moreover, from these plots we actually see we have no student that got an all 0 score.

# For looking at the each of the test performances
# However here when we are looking at the distribution of 0s and 1s
# with a bar chart is a better idea since they are actually looking at categorical factors.

# making sure that the tests are actually binary factors
student_data <- student_data %>%
  mutate(across(starts_with("test_"), ~ factor(.x, levels = c(0, 1))))

# first we should count the data for the tests. 
test_data = student_data %>%
  pivot_longer(cols = starts_with("test_"), names_to = "test", values_to = "response") %>%
  group_by(test, response) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(test_data, aes(x = test, y = count, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Test Responses", x = "Test", y = "Count") +
  scale_fill_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"), labels = c("Incorrect", "Correct")) +
  theme_minimal()

   