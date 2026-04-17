# CSC 308 Final Project
# Script: data_cleaning.R
# Author: Makena Jelks
# Date:   April 2026
#
# Purpose: Load genres_v2.csv, perform all cleaning/preprocessing steps,
#          produce the shared spotify_df, and generate
#          basic descriptive statistics for the Dataset section of the paper.

#Load libraries

library(dplyr)
library(ggplot2)
library(tidyr)


#1. Load raw data

spotify_df_raw <- read.csv("genres_v2.csv",
                           stringsAsFactors = FALSE,
                           low_memory       = FALSE)

cat("Raw dimensions:", nrow(spotify_df_raw), "rows x",
    ncol(spotify_df_raw), "cols\n")


#2. Inspect raw data

# Column names
names(spotify_df_raw)

# Missing values per column
missing_raw <- colSums(is.na(spotify_df_raw))
print(missing_raw[missing_raw > 0])

# Unique genres
cat("Unique genres:", length(unique(spotify_df_raw$genre)), "\n")
print(sort(unique(spotify_df_raw$genre)))

# Duplicate rows
cat("Duplicate rows:", sum(duplicated(spotify_df_raw)), "\n")


#3. Cleaning Step 1 — Select analytical columns

keep_cols <- c("danceability", "energy", "key", "loudness", "mode",
               "speechiness", "acousticness", "instrumentalness",
               "liveness", "valence", "tempo", "duration_ms",
               "time_signature", "genre", "song_name")

spotify_df <- spotify_df_raw %>%
  select(all_of(keep_cols))

cat("Columns after selection:", ncol(spotify_df), "\n")


#4. Cleaning Step 2 — Correct data types

num_features <- c("danceability", "energy", "loudness", "speechiness",
                  "acousticness", "instrumentalness", "liveness",
                  "valence", "tempo", "duration_ms")

spotify_df <- spotify_df %>%
  mutate(
    across(all_of(num_features), as.numeric),
    key            = as.integer(key),
    mode           = as.factor(mode),          # 0 = minor, 1 = major
    time_signature = as.integer(time_signature),
    genre          = as.factor(genre)
  )


#5. Cleaning Step 3 — Remove rows with NA in core features

before <- nrow(spotify_df)
spotify_df <- spotify_df %>%
  filter(complete.cases(select(., all_of(num_features))))
after <- nrow(spotify_df)

cat("Rows removed (NA in audio features):", before - after, "\n")


#6. Cleaning Step 4 — Remove physical outliers

before <- nrow(spotify_df)
spotify_df <- spotify_df %>%
  filter(
    loudness    <= 0,   # values > 0 dB are physically invalid
    duration_ms >  0,   # 0-ms rows are corrupt
    tempo       >  0    # tempo must be positive
  )
after <- nrow(spotify_df)

cat("Rows removed (physical outliers):", before - after, "\n")
cat("Final cleaned dataset size:", nrow(spotify_df), "tracks\n")


#7. Descriptive statistics

# Overall summary
summary(spotify_df[num_features])

# Mean/SD table (paper-ready)
stats_table <- spotify_df %>%
  select(all_of(num_features)) %>%
  summarise(across(everything(),
    list(mean = mean, sd = sd, median = median, min = min, max = max),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to  = c("Feature", "Stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(stats_table)

# Paper stats: rows, features, genres, missing
cat("\n=== PAPER NUMBERS (copy into Section 3.2) ===\n")
cat("Tracks (rows):", nrow(spotify_df), "\n")
cat("Features (cols):", ncol(spotify_df), "\n")
cat("Unique genres:", length(levels(spotify_df$genre)), "\n")
cat("Missing values in audio features:\n")
print(colSums(is.na(spotify_df[num_features])))


#8. Genre distribution plot

genre_plot <- spotify_df %>%
  count(genre, sort = TRUE) %>%
  ggplot(aes(x = reorder(genre, n), y = n, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Track Count per Genre",
       x = "Genre",
       y = "Number of Tracks") +
  theme_minimal()

print(genre_plot)
ggsave("genre_distribution.png", genre_plot, width = 7, height = 5, dpi = 300)


#9. Save cleaned data

save(spotify_df, file = "spotify_df_clean.RData")
cat("Saved spotify_df_clean.RData — team can load with: load('spotify_df_clean.RData')\n")
