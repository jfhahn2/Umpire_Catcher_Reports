library(tidyverse)
setwd("~/Documents/Hyannis")

# Set Umpire Name
ump_name <- "Marvic Gomez"
last_name <- substring(ump_name, regexpr(" ", ump_name)+1)[1]

# Read in Trackman File
data <- read_csv("Trackman/HYA_July_24.csv") 

# Filter for taken pitches
calls <- data %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled"))

# Draw Home Plate
pentagon <- data.frame(
  x = c(-17/24, -17/24, 0, 17/24, 17/24),
  y = c(0, .3, .58, .3, 0)
)
# Plot for split by batter hand
umpire_title <- paste("Umpire", unique(calls$Date), unique(calls$AwayTeam), "at", unique(calls$HomeTeam))
umpire_plot <- ggplot(calls) +
  aes(PlateLocSide, PlateLocHeight, color = PitchCall) +
  geom_point(size = 2.95) +
  geom_rect(xmin = -17/24, xmax = 17/24, ymin = 1.6, ymax = 3.38, col = "red", alpha = 0) +
  geom_polygon(data = pentagon, aes(x, y), color = "black", fill = "white", alpha = 0.5) +
  facet_grid(. ~ BatterSide) +
  scale_color_manual(values = c("BallCalled" = "green3", "StrikeCalled" = "red")) +
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-3, 3)) +
  scale_y_continuous(limits = c(0,4.5)) +
  ggtitle(umpire_title)

file_title <- paste0("plots/", "Umpire_Split_", unique(calls$GameID),".png")
ggsave(file_title, umpire_plot, width = 8, height = 6)

# Plot for umpire overall
umpire_plot <- ggplot(calls) +
  aes(PlateLocSide, PlateLocHeight, color = PitchCall) +
  geom_point(size = 7) +
  geom_rect(xmin = -17/24, xmax = 17/24, ymin = 1.6, ymax = 3.38, col = "red", alpha = 0) +
  geom_polygon(data = pentagon, aes(x, y), color = "black", fill = "white", alpha = 0.5) +
  scale_color_manual(values = c("BallCalled" = "green3", "StrikeCalled" = "red")) +
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-3, 3)) +
  scale_y_continuous(limits = c(0,4.5)) +
  ggtitle(umpire_title)


file_title <- paste0("plots/", "Umpire_Total_", unique(calls$GameID),".png")
ggsave(file_title, umpire_plot, width = 8, height = 6)


# Split the data by catcher and create individual plots
unique_catchers <- unique(calls$Catcher)

for (catcher in unique_catchers) {
  catcher_calls <- calls %>% filter(Catcher == catcher)
  
  catcher_title <- paste(catcher, unique(catcher_calls$Date[1]), catcher_calls$PitcherTeam[1])
  plt <- ggplot(catcher_calls) +
    aes(PlateLocSide, PlateLocHeight, color = PitchCall) +
    geom_point(size = 2.95) +
    geom_rect(xmin = -17/24, xmax = 17/24, ymin = 1.6, ymax = 3.38, col = "red", alpha = 0) +
    geom_polygon(data = pentagon, aes(x, y), color = "black", fill = "white", alpha = 0.5) +
    facet_grid(. ~ BatterSide) +
    scale_color_manual(values = c("BallCalled" = "green3", "StrikeCalled" = "red"), labels = c("Ball", "Strike")) +
    ggtitle(catcher_title) +
    scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-3, 3)) + 
    scale_y_continuous(limits = c(0,4.5))


  last_name <- substring(catcher, 1, regexpr(",", catcher) - 1)
  file_title <- paste0("plots/", last_name, "_", unique(calls$GameID),".png")
  ggsave(file_title, plt, width = 8, height = 6)
}
