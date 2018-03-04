p_f_m <- function(home_scored, home_conceded, average_home_scored, average_home_conceded,away_scored,away_conceded)
{
  average_away_conceded = average_home_scored
  average_away_scored=average_home_conceded
  
  
  scaled_home=home_scored/average_home_scored
  scaled_away=away_scored/average_away_scored
  scaled_away_defence = away_conceded/average_away_conceded
  scaled_home_defence = home_conceded/average_home_conceded
  
  XteamA = rpois(1000,home_scored*scaled_away_defence)
  XteamB = rpois(1000,away_scored*scaled_home_defence)
  
  totalgoals=XteamA+XteamB
  meangoals=mean(totalgoals)
  cat("\nhome_scored:",home_scored)
  cat("\nMean Goals:",meangoals)
  cat("\nHome wins from 1000:",sum(XteamA>XteamB))
  cat("\nDraws from 1000:",sum(XteamA==XteamB))
  cat("\nAway wins from 1000:",sum(XteamA<XteamB))
  cat("\n3 or more goals from 1000:",sum(totalgoals>2))
  cat("\nBoth teams to score:",sum(XteamB>0 & XteamA>0))
  cat("\nMean Home:",mean(XteamA))
  cat("\nMean Away:",mean(XteamB))
}
p_f_m(2.25,1.25,1.62,1.19,0.88,1)