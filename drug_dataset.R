library(tidyverse)

# Load the dataset
df <- read.csv("Medicine_Details.csv", stringsAsFactors = FALSE)

# View basic structure
glimpse(df)

# Convert column names to be more R-friendly
df <- df %>% 
  rename(Medicine_Name = `Medicine.Name`,
         Composition = `Composition`,
         Uses = `Uses`,
         Side_Effects = `Side_effects`,
         Manufacturer = `Manufacturer`,
         Excellent_Review = `Excellent.Review..`,
         Average_Review = `Average.Review..`,
         Poor_Review = `Poor.Review..`)

# Convert review columns to numeric
df <- df %>% mutate(across(Excellent_Review:Poor_Review, as.numeric))

# Sentiment Analysis: Review Distribution
review_distribution <- df %>%
  summarise(
    Excellent = mean(Excellent_Review),
    Average = mean(Average_Review),
    Poor = mean(Poor_Review)
  ) %>% 
  pivot_longer(cols = everything(), names_to = "Review_Type", values_to = "Percentage")

# Plot Review Distribution
ggplot(review_distribution, aes(x = Review_Type, y = Percentage, fill = Review_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Percentage, 1)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Overall Distribution of Medicine Reviews", y = "Percentage (%)", x = "Review Type") +
  scale_fill_manual(values = c("Excellent" = "steelblue", "Average" = "orange", "Poor" = "red"))

# Medicine Popularity Analysis
# Top 10 best-rated medicines
top_medicines <- df %>% arrange(desc(Excellent_Review)) %>% head(10)

# Worst 10 medicines
worst_medicines <- df %>% arrange(desc(Poor_Review)) %>% head(10)

# Side Effects Frequency Analysis
side_effects <- df %>% select(Side_Effects) %>% 
  separate_rows(Side_Effects, sep = ", ") %>% 
  count(Side_Effects, sort = TRUE)

# Top 10 most common side effects
top_side_effects <- side_effects %>% head(10)

# Manufacturer Performance
manufacturer_reviews <- df %>%
  group_by(Manufacturer) %>%
  summarise(Average_Excellent = mean(Excellent_Review, na.rm = TRUE)) %>%
  arrange(desc(Average_Excellent))

# Top 10 manufacturers by excellent review
top_manufacturers <- manufacturer_reviews %>% head(10)

# Plot Manufacturer Performance
ggplot(top_manufacturers, aes(x = reorder(Manufacturer, -Average_Excellent), y = Average_Excellent, fill = Manufacturer)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Manufacturers by Excellent Review Percentage", x = "Manufacturer", y = "Average Excellent Review (%)") +
  theme(legend.position = "none")

# Uses vs. Reviews
uses_reviews <- df %>%
  group_by(Uses) %>%
  summarise(Average_Excellent = mean(Excellent_Review, na.rm = TRUE)) %>%
  arrange(desc(Average_Excellent))

