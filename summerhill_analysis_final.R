###############################################################
# Analyse Summerhill – Mathilde Hasson
# Version texte intégré : pas besoin de lire les PDF
###############################################################

# 1. Packages
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(textdata)
library(readr)

# 2. Importer les textes (manuellement collés ici)
data <- tibble::tibble(
  id = c("isabel_mary", "john_dave", "ron_mike", "thomas_boris"),
  text = c(
    # TEXTE 1
    "It allowed me to explore my interests. I didn’t have to stick to the strict English system. I felt empowered, supported. Teachers saw us as whole people.",
    # TEXTE 2
    "I spent most of my time making films. Teachers helped me, acted in my movies, showed me how to build props. I felt seen, respected, free to learn my way.",
    # TEXTE 3
    "We learned emotional responsibility. Summerhill made me feel free, accepted. Teachers were like parents. They trusted us.",
    # TEXTE 4
    "I was bullied in traditional school. At Summerhill I learned to speak, to lead, to live in community. The meeting taught me democracy. It was hard to leave."
  )
)

# 3. Traitement des mots
words <- data %>%
  unnest_tokens(word, text)

# 4. Lexique NRC filtré
nrc <- textdata::lexicon_nrc() %>%
  filter(sentiment %in% c("joy", "trust", "anger", "sadness")) %>%
  mutate(polarity = case_when(
    sentiment %in% c("joy", "trust") ~ "positive",
    TRUE ~ "negative"
  ))

# 5. Analyse de sentiment
sentiment_df <- words %>%
  inner_join(nrc, by = "word") %>%
  count(id, polarity, name = "n") %>%
  pivot_wider(names_from = polarity, values_from = n, values_fill = 0)

# 6. Calcul de l’indice
satisfaction_df <- sentiment_df %>%
  mutate(sat_index = (positive - negative) / (positive + negative + 1)) %>%
  select(id, positive, negative, sat_index)

# 7. Graphique
plot <- ggplot(satisfaction_df, aes(reorder(id, sat_index), sat_index)) +
  geom_col(fill = "#F4C430") +
  coord_flip() +
  labs(
    title = "Indice de satisfaction basé sur les récits d’anciens élèves de Summerhill",
    x = NULL, y = "Satisfaction index (-1 à +1)"
  ) +
  theme_minimal()

# 8. Export
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/satisfaction_plot.png", plot, width = 8, height = 4, dpi = 300)
write_csv(satisfaction_df, "outputs/resume_satisfaction.csv")

message("✅ Analyse Summerhill terminée avec succès.")

