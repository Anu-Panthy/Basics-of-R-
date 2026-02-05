# Make sure treatment is a factor
df$Tray_Friday <- factor(df$Tray_Friday)

# Boxplot: log CFU by treatment
boxplot(
  log.CFU.g ~ Tray_Friday,
  data = df,
  xlab = "Treatment (XLT4, TSA, DBRC)",
  ylab = "log CFU/g",
  main = "log CFU by Treatment"
)

stripchart(
  log.CFU.g ~ Tray_Friday,
  data = df,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  add = TRUE
)




# Make sure Treatment is a factor and keep your desired order
df$Tray_Friday <- factor(df$Tray_Friday, levels = c("XLT4", "TSA", "DBRC"))

# Optional: remove missing log values
df_plot <- df[!is.na(df$log.CFU.g), ]

# Paper-style plot settings (base R)
par(mar = c(5, 5, 3, 1))  # margins: bottom, left, top, right

boxplot(
  log.CFU.g ~ Tray_Friday,
  data = df_plot,
  xlab = "Treatment",
  ylab = "log CFU/g",
  main = "Salmonella load by treatment",
  pch = NA,              # no default outlier points (we add our own)
  las = 1,               # y-axis labels horizontal (easier to read)
  cex.lab = 1.2,         # axis label size
  cex.axis = 1.1,        # axis tick size
  cex.main = 1.2,        # title size
  outline = FALSE        # hide outlier dots (we'll show all points)
)

# Add raw data points (jitter) = looks like real papers
stripchart(
  log.CFU.g ~ Tray_Friday,
  data = df_plot,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  cex = 1,
  add = TRUE
)

# Add a light grid (optional but common)
grid(nx = NA, ny = NULL)



tiff("Figure_logCFU_boxplot.tiff", width = 6, height = 4, units = "in", res = 300)

df$Tray_Friday <- factor(df$Tray_Friday, levels = c("XLT4", "TSA", "DBRC"))
df_plot <- df[!is.na(df$log.CFU.g), ]

par(mar = c(5, 5, 3, 1))

boxplot(
  log.CFU.g ~ Tray_Friday,
  data = df_plot,
  xlab = "Treatment",
  ylab = "log CFU/g",
  main = "Salmonella load by treatment",
  pch = NA,
  las = 1,
  cex.lab = 1.2,
  cex.axis = 1.1,
  cex.main = 1.2,
  outline = FALSE
)

stripchart(
  log.CFU.g ~ Tray_Friday,
  data = df_plot,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  cex = 1,
  add = TRUE
)

grid(nx = NA, ny = NULL)

dev.off()

