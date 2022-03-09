library(readxl)
library(here)
library(tidyr)
library(dplyr)

mydata <- read_excel(here("analysis/data/raw_data/TCSA_raw_data.xlsx"))

tcsa <- mydata %>%
  mutate(Tcsa = 0.5 *Width * Thickness) %>%
  mutate(id = row_number())

cv <- function(x, ... ) sd(x, ...) / mean(x, ...) *100

mean(tcsa$Tcsa)
sd(tcsa$Tcsa)
cv(tcsa$Tcsa)

## SP made out of blade
tcsa_blade <- tcsa %>%
  filter (Blank == "blade")

mean(tcsa_blade$Tcsa)
sd(tcsa_blade$Tcsa)
cv(tcsa_blade$Tcsa)

library(ggplot2)
ggplot(tcsa, aes(Tcsa, id)) +
  geom_point()
