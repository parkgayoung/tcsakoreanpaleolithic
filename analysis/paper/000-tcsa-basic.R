library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

mydata <- read_excel(here("analysis/data/raw_data/TCSA_raw_data.xlsx"))

tcsa <- mydata %>%
  mutate(min_tcsa = 0.5 *Width * Thickness) %>%
  mutate(max_tcsa = 0.5 *Length * Thickness) %>%
  mutate(id = row_number())

cv <- function(x, ... ) sd(x, ...) / mean(x, ...) *100

mean(tcsa$min_tcsa)
sd(tcsa$min_tcsa)
cv(tcsa$min_tcsa)

blank_tcsa <-tcsa %>%
  filter(Blank == "blade")

ggplot (blank_tcsa, aes(min_tcsa)) +
  geom_boxplot()


#### histogram for min tcsa
all <- ggplot(tcsa, aes(min_tcsa)) +
  geom_histogram(color="black", fill="green") +
  theme_bw()

#Facet by raw material
all +  facet_wrap(~Blank, ncol = 3)


#Facet by raw site
all +  facet_wrap(~Site_name, ncol = 4) +
      coord_cartesian(xlim = c(0,1000), ylim = c(0,20))

#Facet by size class



########figure 5#############
# use two columns for x-axis like Lombard's figure 5
library(reshape2)
two_tcsa <- melt(tcsa, id.vars = c("min_tcsa", "max_tcsa"))

## combine min and max tcsas to one x-axis, and then make min again for y-axis
two_tcsa <- tcsa %>%
  pivot_longer(c(min_tcsa, max_tcsa), names_to = "var", values_to = "value") %>%
  mutate(MIN_tcsa = 0.5 *Width * Thickness)

ggplot(two_tcsa, aes(x = value, y = MIN_tcsa, color = var)) +
  geom_point(size = 0.5) +
  coord_cartesian(xlim = c(0,1000), ylim = c(0,1000)) +
  theme_linedraw()

