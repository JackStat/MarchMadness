#' @title prepareData
#'
#' @description Creates a more useful dataset for modeling purposes. Takes in detailed results data
#'
#' @param DF detailed results data (either from the tournament or regular season)
#'
#' @export

prepareData <- function(DF){
  names(DF) = tolower(names(DF))

  Set1 <- DF
  names(Set1) = gsub("^w", "t1_", names(Set1))
  names(Set1) = gsub("^l", "t2_", names(Set1))

  Set2 <- DF
  names(Set2) = gsub("^w", "t2_", names(Set2))
  names(Set2) = gsub("^l", "t1_", names(Set2))

  Set2p <-
  Set2 %>%
    mutate(
      t1_loc = case_when(
        t2_loc == 'H' ~ 'A',
        t2_loc == 'A' ~ 'H',
        t2_loc == 'N' ~ 'N'
      )
    ) %>%
    select(-t2_loc)

  AllGames <-
    rbind(Set1, Set2p) %>%
    mutate(
      t1_win = as.numeric(t1_score > t2_score)
      ,t1_margin = t1_score - t2_score
      ,t1_fgpct = 100*t1_fgm/t1_fga
      ,t2_fgpct = 100*t2_fgm/t2_fga

      ,t2_margin = t2_score - t1_score
      ,t2_win = as.numeric(t2_score > t1_score)

    ) %>%
    left_join(
      teams %>%
        select(
          t1_name = TeamName
          ,TeamID
        ), by = c('t1_teamid' = 'TeamID')) %>%
    left_join(
      teams %>%
        select(
          t2_name = TeamName
          ,TeamID
        ), by = c('t2_teamid' = 'TeamID'))
}





