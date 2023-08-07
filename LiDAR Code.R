# ForestScanner vs. Arboreal Forest vs. Logger Tape

# Packages

library(tidyverse)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(MASS)
library(car)
library(agricolae)
library(BlandAltmanLeh)

# Loading data from GitHub - tally = tree measurements    timing = time measurements

tally <- read.csv("https://raw.githubusercontent.com/ArkCaptain/AF_LiDAR/main/LiDAR_Master_Data.csv",
                   header = T)

timing <- read.csv("https://raw.githubusercontent.com/ArkCaptain/AF_LiDAR/main/LiDAR_Master_Time.csv",
                    header = T)

# Long format data

tally_long <- gather(tally, type, DBH, D_LT:D_ARB, factor_key = T)

timing_long <- timing %>%
  gather(type, time, Time_LT:TIME_ARB, factor_key = T) %>%
  mutate(type = recode_factor(type,
                              "Time_LT" = "Logger Tape",
                              "Time_FS" = "ForestScanner",
                              "TIME_ARB" = "Arboreal Forest"))

# Scatterplots

# Logger Tape vs. ForestScanner

LT_FS <- ggscatter(tally, x = "D_LT", y = "D_FS", add = "reg.line",
                    shape = 16,
                    color = "grey19",
                    size = 1,
                    add.params = list(color = "red",
                                      fill = "black",
                                      linetype = "dashed",
                                      size = 1),
                    conf.int = F, conf.int.level = 0.95, legend = "bottom",
                    xlab = "DBH from Tape (cm)",  
                    ylab = "DBH from App (cm)",
                    title = "Logger Tape vs. ForestScanner App") +
  #  scale_x_continuous(limits = c(0, 30)) +
  #  scale_y_continuous(limits = c(0, 30)) +
  stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           label.x = 20, label.y = 30, p.accuracy = 0.001, r.accuracy = 0.01,
           cor.coef.name = "r", method = "pearson", size = 6, color = "black") +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

LT_FS

# Logger Tape vs. Arboreal
LT_ARB <- ggscatter(tally, x = "D_LT", y = "D_ARB", add = "reg.line",
                     shape = 16,
                     color = "grey19",
                     size = 1,
                     add.params = list(color = "red",
                                       fill = "black",
                                       linetype = "dashed",
                                       size = 1),
                     conf.int = F, conf.int.level = 0.95, legend = "bottom",
                     xlab = "DBH from Tape (cm)",  
                     ylab = "DBH from App (cm)",
                     title = "Logger Tape vs. Arboreal Forest App") +
  #  scale_x_continuous(limits = c(0, 30)) +
  #  scale_y_continuous(limits = c(0, 30)) +
  stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           label.x = 20, label.y = 30, p.accuracy = 0.001, r.accuracy = 0.01,
           cor.coef.name = "r", method = "pearson", size = 6, color = "black") +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

LT_ARB

# ForestScanner vs. Arboreal
FS_ARB <- ggscatter(tally, x = "D_FS", y = "D_ARB", add = "reg.line",
                    shape = 16,
                    color = "grey19",
                    size = 1,
                    add.params = list(color = "red",
                                      fill = "black",
                                      linetype = "dashed",
                                      size = 1),
                    conf.int = F, conf.int.level = 0.95, legend = "bottom",
                    xlab = "DBH from ForestScanner App (cm)",  
                    ylab = "DBH from Arboreal Forest App (cm)",
                    title = "ForestScanner App vs. Arboreal Forest App") +
  #  scale_x_continuous(limits = c(0, 30)) +
  #  scale_y_continuous(limits = c(0, 30)) +
  stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           label.x = 20, label.y = 30, p.accuracy = 0.001, r.accuracy = 0.01,
           cor.coef.name = "r", method = "pearson", size = 6, color = "black") +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

FS_ARB

# Bland-Altman plots

# Bland-Altman plots are a way to check the agreement of different measurement methods or the retest-reliability
# of a single measurement method.
# In this graphical method the differences between the two methods are plotted against the averages of the
# two methods.
# The x-axis of the plot displays the average measurement of the two instruments.
# The y-axis displays the difference in measurements between the two instruments.
# Other plotted lines:
# 1) The average difference in measurements between the two methods.
# 2) The upper limit of the 95% CI for the average difference.
# 3) The lower limit of the 95% CI for the average difference.
# This type of plot is useful for determining two things:
# 1) What is the average difference in measurements between the two methods?
#    The horizontal line drawn in the middle of the plot shows the average difference in measurements between the
#    two methods. This value is often referred to as the “bias” between the methods.
# 2) What is the typical range of agreement between the two methods?
#    The upper and lower CIs lines gives us an idea of the typical range of agreement between the two methods.
#    In general, 95% of the differences between the two methods fall within these confidence limits.
#    The wider the CI, the wider the range of differences in measurements between the two methods.

# Logger Tape vs. ForestScanner

BA_LT_FS_stats <- bland.altman.stats(tally$D_LT, tally$D_FS)

BA_LT_FS_data <- cbind(BA_LT_FS_stats$means, BA_LT_FS_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

BA_LT_FS <- ggscatter(BA_LT_FS_data, x = "means", y = "differences",
                       shape = 1,
                       color = "grey19",
                       size = 3,
                       xlab = "Average measurement (cm)",  
                       ylab = "Differences between measurements (cm)",
                       title = "Logger Tape vs ForestScanner App") +
  geom_hline(yintercept = BA_LT_FS_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_LT_FS_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_LT_FS_stats$lower.limit, linetype =2 ) +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

BA_LT_FS

# Logger Tape Vs Arboreal Forest

BA_LT_ARB_stats <- bland.altman.stats(tally$D_LT, tally$D_ARB)

BA_LT_ARB_data <- cbind(BA_LT_ARB_stats$means, BA_LT_ARB_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

BA_LT_ARB <- ggscatter(BA_LT_ARB_data, x = "means", y = "differences",
                        shape = 1,
                        color = "grey19",
                        size = 3,
                        xlab = "Average measurement (cm)",  
                        ylab = "Differences between measurements (cm)",
                        title = "Logger Tape vs Arboreal Forest App") +
  geom_hline(yintercept = BA_LT_ARB_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_LT_ARB_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_LT_ARB_stats$lower.limit, linetype =2 ) +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

BA_LT_ARB

# ForestScanner Vs Arboreal Forest

BA_FS_ARB_stats <- bland.altman.stats(tally$D_FS, tally$D_ARB)

BA_FS_ARB_data <- cbind(BA_FS_ARB_stats$means, BA_FS_ARB_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

BA_FS_ARB <- ggscatter(BA_FS_ARB_data, x = "means", y = "differences",
                       shape = 1,
                       color = "grey19",
                       size = 3,
                       xlab = "Average measurement (cm)",  
                       ylab = "Differences between measurements (cm)",
                       title = "ForestScanner App vs Arboreal Forest App") +
  geom_hline(yintercept = BA_FS_ARB_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_FS_ARB_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_FS_ARB_stats$lower.limit, linetype =2 ) +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  theme_classic() 

BA_FS_ARB

# PANE

pane_all <- cowplot::plot_grid(LT_FS + rremove("xlab") + rremove("ylab"),
                               BA_LT_FS + rremove("xlab") + rremove("ylab"),
                               LT_ARB,
                               BA_LT_ARB,
                               align = "hv",
                               ncol = 2,
                               nrow = 2)

pane_all

# Timing ANOVA

# ANOVA

res_aov <- aov(time ~ type, data = timing_long)

out_tuk <- HSD.test(res_aov, "type", console = T)

hist(res_aov$residuals)

qqPlot(res_aov$residuals,
       id = FALSE)

shapiro.test(res_aov$residuals)

plot(res_aov, which = 3)

# Violin plots

# A violin plot is a hybrid of a box plot and a kernel density plot, which shows peaks in the data.
# It is used to visualize the distribution of numerical data. Unlike a box plot that can only show summary
# statistics, violin plots depict summary statistics and the density of each variable.
# The shape of the violin represents the shape of the distribution.
# The boxes are boxplots.
# The diamonds are means.
# Black dots are outliers.

times <- ggviolin(timing_long,
                  x = "type",
                  y = "time",
                  orientation = "horiz",
                  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                  fill = "type",
                  alpha = 0.5,
                  add = "boxplot",
                  add.params = list(fill = "white"),
                  xlab = "",  
                  ylab = "Time (seconds)") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "red") +
  #  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.25) +
  #  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  font("title", size = 22) +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 14) +
  guides(fill = "none") +
  theme_classic() +
  annotate("text", x = c(1, 2, 3), y = c(350, 200, 200), label = c("A", "B", "B"))

times
