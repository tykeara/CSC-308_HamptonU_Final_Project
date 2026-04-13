library(ggplot2)
library(dplyr)
library(ggcorrplot)

# --- Load data
spotify_df <- read.csv("genres_v2.csv", stringsAsFactors = FALSE)
# Pick a consistent theme
my_theme <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

# ── 1. HISTOGRAMS ──────────────────────────────────────────────────
# Distribution of danceability
p1 <- ggplot(spotify_df, aes(x = danceability)) +
  geom_histogram(bins = 40, fill = "#7F77DD", color = "white") +
  labs(title = "Distribution of danceability",
       x = "Danceability (0–1)", y = "Count") +
  my_theme
p1
# Distribution of energy
p2 <- ggplot(spotify_df, aes(x = energy)) +
  geom_histogram(bins = 40, fill = "#1D9E75", color = "white") +
  labs(title = "Distribution of energy",
       x = "Energy (0–1)", y = "Count") +
  my_theme
p2 
# Distribution of tempo
p3 <- ggplot(spotify_df, aes(x = tempo)) +
  geom_histogram(bins = 40, fill = "#D85A30", color = "white") +
  labs(title = "Distribution of tempo",
       x = "Tempo (BPM)", y = "Count") +
  my_theme
p3
ggsave("hist_danceability.png", p1, dpi = 300, width = 7, height = 4)
ggsave("hist_energy.png", p2, dpi = 300, width = 7, height = 4)
ggsave("hist_tempo.png", p3, dpi = 300, width = 7, height = 4)


# ── 2. SCATTERPLOT ─────────────────────────────────────────────────
p4 <- ggplot(spotify_df, aes(x = energy, y = loudness)) +
  geom_point(alpha = 0.25, size = 1.2, color = "#185FA5") +
  geom_smooth(method = "lm", color = "#D85A30", se = FALSE) +
  labs(title = "Energy vs. loudness",
       x = "Energy (0–1)", y = "Loudness (dB)") +
  my_theme

ggsave("scatter_energy_loudness.png", p4, dpi = 300, width = 7, height = 5)


# ── 3. BOXPLOTS ────────────────────────────────────────────────────
# Limit to top genres so the plot isn't overcrowded
top_genres <- spotify_df %>%
  count(genre, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(genre)

p5 <- spotify_df %>%
  filter(genre %in% top_genres) %>%
  ggplot(aes(x = reorder(genre, danceability, median), 
             y = danceability, fill = genre)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.5, alpha = 0.8) +
  coord_flip() +
  labs(title = "Danceability by genre (top 10)",
       x = "Genre", y = "Danceability (0–1)") +
  my_theme

ggsave("boxplot_danceability_genre.png", p5, dpi = 300, width = 8, height = 5)


# ── 4. CORRELATION HEATMAP ─────────────────────────────────────────
num_vars <- spotify_df %>%
  select(danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence, tempo)

corr_matrix <- cor(num_vars, use = "complete.obs")

p6 <- ggcorrplot(corr_matrix,
                 method = "square",
                 type = "lower",
                 lab = TRUE,
                 lab_size = 3,
                 colors = c("#D85A30", "white", "#185FA5"),
                 title = "Correlation between audio features") +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),
    axis.title = element_blank()  
  )


p6

ggsave("heatmap_correlation.png", p6, dpi = 300, width = 8, height = 7)
