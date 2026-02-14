library(tidyverse)

# Read data
df <- read.csv("bio_active.csv", check.names = FALSE)
names(df)[names(df) == "% AA"] <- "AA"

# Remove NA
df <- df %>%
  filter(!is.na(AA),
         !is.na(Microgreen),
         !is.na(Type),
         !is.na(Assay))

# Summary mean ± SD
df_sum <- df %>%
  group_by(Assay, Microgreen, Type) %>%
  summarise(
    mean_AA = mean(AA, na.rm = TRUE),
    sd_AA   = sd(AA, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(df_sum,
       aes(x = Microgreen,
           y = mean_AA,
           fill = Type)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.75),
           width = 0.6) +
  
  geom_errorbar(aes(ymin = mean_AA - sd_AA,
                    ymax = mean_AA + sd_AA),
                position = position_dodge(width = 0.75),
                width = 0.25,
                linewidth = 0.8) +
  
  facet_wrap(~ Assay, nrow = 1) +
  
  scale_fill_manual(values = c("FD" = "#4C72B0",
                               "Fresh" = "#DD8452")) +
  
  labs(title = "Antioxidant Activity (%AA)",
       x = "Microgreen",
       y = "%AA",
       fill = "Processing") +
  
  theme_minimal(base_size = 15) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.position = "top"
  ) + theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank())

