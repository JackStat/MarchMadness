#' @title createBracket
#'
#' @param seeds a \code{data.frame} of the seeds in the tournament
#' @param Subs a \code{data.frame} that has t1ID, t2ID and Pred
#' @param teams a \code{data.frame} that has the ids and team names for each team in the tournament
#' @param season specify the bracket season
#' @param type specify the type of bracket you would like to build. \code{type = 'normal'} will return always select the team with the highest probability as the winner. \code{type = 'simulate'} will randomly assign the winner based on the probabilities
#'
#' @export


createBracket <- function(seeds, Subs, teams, season = 2019, type = "normal"){

  ## reduce down to specific season
  seeds <- seeds[seeds$Season == season,]

  #dropping out the secondary teams
  seeds <- seeds[!grepl("[b]",seeds$Seed),]

  seeds$seedNum <- c(1:16,1:16,1:16,1:16)
  seeds$seedAlpha <- c(rep('W', 16), rep('X',16), rep('Y', 16), rep('Z', 16))

  #   seeds[seeds$seedAlpha=='Z',]
  ord <- c(c(1,16,2,15,3,14,4,13,5,12,6,11,7,10,8,9)
         ,c(1,16,2,15,3,14,4,13,5,12,6,11,7,10,8,9)+16
         ,c(1,16,2,15,3,14,4,13,5,12,6,11,7,10,8,9)+32
         ,c(1,16,2,15,3,14,4,13,5,12,6,11,7,10,8,9)+48)


  top<-seq(2,64,2)
  bot<-seq(1,63,2)

  FirstRound1 <-
    data.frame(
      t1ID = seeds$TeamID[ord[top]]
      ,t2ID = seeds$TeamID[ord[bot]]

      ,stringsAsFactors = FALSE
      )

  FirstRound1$t1Name <-
    factor(FirstRound1$t1ID, levels=teams$TeamID, labels=teams$TeamName)
  FirstRound1$t2Name <-
    factor(FirstRound1$t2ID, levels=teams$TeamID, labels=teams$TeamName)


  FirstRound <- assignWinner(FirstRound1, Subs, teams, type)

  ord<-c(
    c(1,8,2,7,3,6,4,5)
    ,c(1,8,2,7,3,6,4,5)+8
    ,c(1,8,2,7,3,6,4,5)+16
    ,c(1,8,2,7,3,6,4,5)+24
    )

    top<-seq(2,32,2)
    bot<-seq(1,31,2)

  SecondRound1 <-
    data.frame(
      t1Name=FirstRound$Winner[ord[bot]]
      ,t1ID = FirstRound$WinnerID[ord[bot]]
      ,t2Name=FirstRound$Winner[ord[top]]
      ,t2ID = FirstRound$WinnerID[ord[top]]
      )

  SecondRound <- assignWinner(SecondRound1, Subs, teams, type)


  ord<-c(c(1,4,2,3)
    ,c(1,4,2,3)+4
    ,c(1,4,2,3)+8
    ,c(1,4,2,3)+12)

    top<-seq(2,16,2)
    bot<-seq(1,15,2)

  ThirdRound1 <-
    data.frame(
      t1Name=SecondRound$Winner[ord[bot]]
      ,t1ID = SecondRound$WinnerID[ord[bot]]
      ,t2Name=SecondRound$Winner[ord[top]]
      ,t2ID = SecondRound$WinnerID[ord[top]]
      )

  ThirdRound <- assignWinner(ThirdRound1, Subs, teams, type)


  ord<-c(c(1,2)
    ,c(1,2)+2
    ,c(1,2)+4
    ,c(1,2)+6)

    top<-seq(2,8,2)
    bot<-seq(1,7,2)

  FourthRound1 <-
    data.frame(
      t1Name=ThirdRound$Winner[ord[bot]]
      ,t1ID = ThirdRound$WinnerID[ord[bot]]
      ,t2Name=ThirdRound$Winner[ord[top]]
      ,t2ID = ThirdRound$WinnerID[ord[top]]
      )


  FourthRound <- assignWinner(FourthRound1, Subs, teams, type)

  ord<-c(c(1,2)
    ,c(1,2)+2)

  top<-seq(2,4,2)
  bot<-seq(1,3,2)


  FifthRound1 <-
    data.frame(
      t1Name = FourthRound$Winner[ord[bot]]
      ,t1ID = FourthRound$WinnerID[ord[bot]]
      ,t2Name = FourthRound$Winner[ord[top]]
      ,t2ID = FourthRound$WinnerID[ord[top]]
      )

  FifthRound <- assignWinner(FifthRound1, Subs, teams, type)

  ord<-c(1,2)

  SixthRound1 <-
    data.frame(
      t1Name=FifthRound$Winner[ord[1]]
      ,t1ID = FifthRound$WinnerID[ord[1]]
      ,t2Name=FifthRound$Winner[ord[2]]
      ,t2ID = FifthRound$WinnerID[ord[2]]
      )

  SixthRound <- assignWinner(SixthRound1, Subs, teams, type)

  rbind(
    FirstRound = FirstRound
    ,SecondRound = SecondRound
    ,ThirdRound = ThirdRound
    ,FourthRound = FourthRound
    ,FifthRound = FifthRound
    ,SixthRound = SixthRound
  )

}



