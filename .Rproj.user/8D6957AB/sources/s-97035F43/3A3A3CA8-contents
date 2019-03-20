#' @title assignWinner
#'
#' @param RoundData a \code{data.frame} that includes the team matchups
#' @param Subs a \code{data.frame} that has t1ID, t2ID and Pred
#' @param teams a \code{data.frame} that has the ids and team names for each team in the tournament
#'
#' @export


assignWinner <- function(RoundData, Subs, teams, type = "normal"){

  s1 <-
  RoundData %>%
  left_join(Subs %>% dplyr::select(t1ID, t2ID, t1Prob = Pred)) %>%
  left_join(Subs %>% dplyr::select(t2ID = t1ID, t1ID = t2ID, t2Prob = Pred)) %>%
  mutate(
    t1Prob = ifelse(is.na(t1Prob), 1 - t2Prob, t1Prob)
    ,t2Prob = ifelse(is.na(t2Prob), 1 - t1Prob, t2Prob)
    ,Round = case_when(
      nrow(RoundData) == 32 ~ 1,
      nrow(RoundData) == 16 ~ 2,
      nrow(RoundData) == 8 ~ 3,
      nrow(RoundData) == 4 ~ 4,
      nrow(RoundData) == 2 ~ 5,
      nrow(RoundData) == 1 ~ 6
    )
  )

  if(type == 'normal'){
    s1 %>%
      mutate(
        Winner = ifelse(t1Prob > .5, as.character(t1Name), as.character(t2Name))
        ,WinnerID = ifelse(t1Prob > .5, t1ID, t2ID)
      )
  } else if(type == "simulate"){
    s1 %>%
    group_by(t1Name, t2Name) %>%
      mutate(
        Winner = sample(c(as.character(t1Name), as.character(t2Name)), 1, c(t1Prob, t2Prob), replace = TRUE)
      ) %>%
      left_join(teams %>% transmute(Winner = TeamName, WinnerID = TeamID))
  }
}
