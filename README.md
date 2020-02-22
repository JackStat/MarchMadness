
```{r}
library(tidyverse)
# install.packages("remotes")
# remotes::install_github("JackStat/MarchMadness")
library(MarchMadness)

reg <- read_csv("data/DataFiles/RegularSeasonDetailedResults.csv")
teams <- read_csv("data/DataFiles/Teams.csv")
turn <- read_csv("data/DataFiles/NCAATourneyDetailedResults.csv")

AllGames <- prepareData(reg)

save(AllGames, file = "data/AllGames.rda")

TournAll <- prepareData(turn)

save(TournAll, file = "data/TournAll.rda")

```


