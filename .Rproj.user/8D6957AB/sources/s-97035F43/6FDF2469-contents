#' @title processSubmission
#' @description This function will set up a \code{data.frame} that can be passed to your model for predictions. Note: this function assumes that your team 2 variables are the same as team 1 and are named T2{variablename}
#' @param subm the raw file for the kaggle submission
#' @param teams a \code{data.frame} that has the ids and team names for each team in the tournament
#' @param year specify the year you would like to process
#'
#'
#' @export

processSubmission <- function(subm, predictorDF, teams, year = 2018){

  colNames <- names(predictorDF)
  predictorDF <- filter(predictorDF, season == year)

  ## renaming for second team
  predictorDF2 <- predictorDF
  names(predictorDF2) <- paste0("T2", names(predictorDF2))
  ## Assuming that the team column appears in the 1st column
  names(predictorDF)[1] = 't1_name'
  names(predictorDF2)[1] = 't2_name'


  Subs <-
  subm %>%
    mutate(
      Year = as.numeric(substr(ID, 1,4))
      ,t1ID = substr(ID, 6,9)
      ,t2ID = substr(ID, 11,16)
      )

  Subs$t1_name <-
    factor(Subs$t1ID, levels=teams$TeamID, labels=teams$TeamName)
  Subs$t2_name <-
    factor(Subs$t2ID, levels=teams$TeamID, labels=teams$TeamName)

  Subs %>%
    left_join(predictorDF, by = 't1_name') %>%
    left_join(predictorDF2, by = 't2_name') %>%
    mutate(
      t1ID = as.integer(t1ID)
      ,t2ID = as.integer(t2ID)
    )

}
