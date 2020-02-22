
Download the data from the kaggle webpage here https://www.kaggle.com/c/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/data

If the names of the files don't line up I am sorry this is code from last year and I am hoping it doesn't need to be re-written.


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


