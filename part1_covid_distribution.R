# COMP1013 Analytics Programming - Part 1
# Student ID: 22155089
# Task: COVID-19 Distribution by County and Age Group

# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# ---- Import Datasets ----
patients   <- read_csv("patientsUG (2).csv", show_col_types = FALSE)
encounters <- read_csv("encountersUG (1).csv", show_col_types = FALSE)
conditions <- read_csv("conditionsUG (1).csv", show_col_types = FALSE)

# ---- Filter for COVID-19 or Suspected COVID Conditions ----
covid_conditions <- conditions %>%
  filter(grepl("COVID", DESCRIPTION, ignore.case = TRUE))

# ---- Merge Filtered Conditions with Patient Details ----
covid_patients <- inner_join(covid_conditions, patients, by = c("PATIENT" = "Id"))

# ---- Count Number of COVID Patients by County ----
covid_by_county <- covid_patients %>%
  count(COUNTY, name = "Count") %>%
  arrange(desc(Count))

# ---- Display Top 10 Counties ----
head(covid_by_county, 10)

# ---- Plot COVID-19 Distribution Across Counties ----
ggplot(covid_by_county, aes(x = reorder(COUNTY, Count), y = Count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "COVID-19 Patients by County",
       x = "County", y = "Number of Patients") +
  theme_minimal()

# ============================================================
# ---- Robust BirthDate Parsing + Age Group Analysis ----
# ============================================================

birth_candidates <- c("BirthDate","BIRTHDATE","birthdate","DOB","DoB")
birth_col <- intersect(names(patients), birth_candidates)[1]
if (is.na(birth_col)) stop("Birth date column not found. Use names(patients).")

birth_raw <- patients[[birth_col]]

parse_birth <- function(x){
  if (is.numeric(x)) {
    d <- as.Date(NA)
    if (any(!is.na(x) & x > 10000 & x < 60000)) {
      d <- as.Date(x, origin = "1899-12-30")
    } else if (any(!is.na(x) & x > 1e8)) {
      d <- as.Date(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }
    return(d)
  }
  if (is.factor(x)) x <- as.character(x)
  x <- trimws(as.character(x))
  x2 <- gsub("T", " ", x, fixed = TRUE)
  x2 <- sub("Z$", "", x2)
  
  d <- suppressWarnings(ymd_hms(x2, tz = "UTC", quiet = TRUE))
  d[is.na(d)] <- suppressWarnings(ymd(x2[is.na(d)], quiet = TRUE))
  d[is.na(d)] <- suppressWarnings(mdy(x2[is.na(d)], quiet = TRUE))
  d[is.na(d)] <- suppressWarnings(dmy(x2[is.na(d)], quiet = TRUE))
  ix <- is.na(d) & nchar(x2) >= 10 & grepl("^\\d{4}-\\d{2}-\\d{2}", x2)
  d[ix] <- suppressWarnings(ymd(substr(x2[ix], 1, 10), quiet = TRUE))
  as.Date(d)
}

patients_age <- patients %>%
  mutate(
    Birth_parsed = parse_birth(.data[[birth_col]]),
    Age = floor(time_length(interval(Birth_parsed, today()), "years")),
    AgeGroup = cut(
      Age,
      breaks = c(0,18,35,50,120),
      labels = c("0-18","19-35","36-50","51+"),
      right = FALSE)
  )

cat("Unparsed birthdates:", sum(is.na(patients_age$Birth_parsed)), "\n")
print(head(data.frame(raw = head(birth_raw,5), parsed = head(patients_age$Birth_parsed,5)), 5))

# ---- Count + Plot by Age Group ----
age_group_counts <- patients_age %>%
  filter(!is.na(AgeGroup)) %>%
  count(AgeGroup, name = "Count")

print(age_group_counts)

library(knitr)
kable(age_group_counts, caption = "Table 2. Number of COVID-19 Patients by Age Group")

ggplot(patients_age, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age (years) – Histogram", x = "Age", y = "Count") +
  theme_minimal()

ggplot(age_group_counts, aes(x = AgeGroup, y = Count, fill = AgeGroup)) +
  geom_col() +
  labs(title = "Patients by Age Group",
       x = "Age Group", y = "Number of Patients") +
  theme_minimal()

# ---- Summary Findings ----
cat("\nTop County for COVID-19 Patients:\n")
print(slice_max(covid_by_county, Count, n = 1, with_ties = TRUE))

cat("\nLargest Age Group (Overall Patients):\n")
print(slice_max(age_group_counts, Count, n = 1, with_ties = TRUE))


