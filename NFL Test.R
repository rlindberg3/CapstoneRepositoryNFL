
library(dplyr)
library(tidyr)
library(ggplot2)

nfl_data <- read.csv("NFL_offense.csv")

#make all null temperatures at game time "room" temperature
nfl_data$temp[nfl_data$temp == "NULL"] <- 70
nfl_data$temp <- as.integer(nfl_data$temp)

#highlight temp extremes
nfl_data <- mutate(nfl_data, cold_weather= ifelse(temp < 45, 1,0))
nfl_data <- mutate(nfl_data, hot_weather=  ifelse(temp > 85, 1,0))

#weather factors
nfl_data <- mutate(nfl_data, grass_1 = ifelse(surf == "DD GrassMaster" | surf == "Grass",
                                              1,0))
nfl_data <- mutate(nfl_data, bad_weather_1 = ifelse(cond == "Light Rain" | 
                                                      cond == "Rain" |
                                                      cond == "Flurries" |
                                                      cond == "Snow" |
                                                      cond == "Foggy" |
                                                      cond == "Windy" |
                                                      cond == "Hazy" |
                                                      cond == "Thunderstorms"|
                                                      cond == "Light Snow" |
                                                      cond == "Light Showers" ,1,0))


#identify home team
nfl_data$h <- as.character(nfl_data$h)
nfl_data$team <- as.character(nfl_data$team)
nfl_data <- mutate(nfl_data, home_team_1=  ifelse(h == team, 1,0))

#identify position
nfl_data <- mutate(nfl_data, is_WR =  ifelse(pos1 == "WR", 1,0))
nfl_data <- mutate(nfl_data, is_TE =  ifelse(pos1 == "TE", 1,0))
nfl_data <- mutate(nfl_data, is_RB =  ifelse(pos1 == "RB", 1,0))
nfl_data <- mutate(nfl_data, is_QB =  ifelse(pos1 == "QB", 1,0))
#age
nfl_data <- mutate(nfl_data, age = year - yob)
#replace 0 forty with avg for position
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(forty1 = ifelse(forty == 0, mean(forty[forty>0]), forty))
#replace 0 vertical with average for position
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(vertical1 = ifelse(vertical == 0, mean(vertical[vertical>0]), vertical))
#replace 0 arm length with formula for 40% of height is arm
nfl_data$arm <- ifelse(nfl_data$arm == 0, nfl_data$height*0.4, nfl_data$arm)

nfl_data$broad <- as.numeric(nfl_data$broad)

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(broad1 = ifelse(broad == 0, mean(broad[broad>0]), broad))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(shuttle1 = ifelse(shuttle == 0, mean(shuttle[shuttle>0]), shuttle))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(cone1 = ifelse(cone == 0, mean(cone[cone>0]), cone))
#clean teams and give each team a field
nfl_data <- mutate(nfl_data, Teams =  ifelse(team == "STL" | team == "LA", "STL/LA",team))
nfl_data <- mutate(nfl_data, ARI = ifelse(Teams == "ARI",1,0))
nfl_data <- mutate(nfl_data, ATL = ifelse(Teams == "ATL",1,0))
nfl_data <- mutate(nfl_data, BAL = ifelse(Teams == "BAL",1,0))
nfl_data <- mutate(nfl_data, BUF = ifelse(Teams == "BUF",1,0))
nfl_data <- mutate(nfl_data, CAR = ifelse(Teams == "CAR",1,0))
nfl_data <- mutate(nfl_data, CHI = ifelse(Teams == "CHI",1,0))
nfl_data <- mutate(nfl_data, CIN = ifelse(Teams == "CIN",1,0))
nfl_data <- mutate(nfl_data, CLE = ifelse(Teams == "CLE",1,0))
nfl_data <- mutate(nfl_data, DAL = ifelse(Teams == "DAL",1,0))
nfl_data <- mutate(nfl_data, DEN = ifelse(Teams == "DEN",1,0))
nfl_data <- mutate(nfl_data, DET = ifelse(Teams == "DET",1,0))
nfl_data <- mutate(nfl_data, GB = ifelse(Teams == "GB",1,0))
nfl_data <- mutate(nfl_data, HOU = ifelse(Teams == "HOU",1,0))
nfl_data <- mutate(nfl_data, IND = ifelse(Teams == "IND",1,0))
nfl_data <- mutate(nfl_data, JAC = ifelse(Teams == "JAC",1,0))
nfl_data <- mutate(nfl_data, KC = ifelse(Teams == "KC",1,0))
nfl_data <- mutate(nfl_data, MIA = ifelse(Teams == "MIA",1,0))
nfl_data <- mutate(nfl_data, MINN = ifelse(Teams == "MIN",1,0))
nfl_data <- mutate(nfl_data, NE = ifelse(Teams == "NE",1,0))
nfl_data <- mutate(nfl_data, NOR = ifelse(Teams == "NO",1,0))
nfl_data <- mutate(nfl_data, NYG = ifelse(Teams == "NYG",1,0))
nfl_data <- mutate(nfl_data, NYJ = ifelse(Teams == "NYJ",1,0))
nfl_data <- mutate(nfl_data, OAK = ifelse(Teams == "OAK",1,0))
nfl_data <- mutate(nfl_data, PHI = ifelse(Teams == "PHI",1,0))
nfl_data <- mutate(nfl_data, PIT = ifelse(Teams == "PIT",1,0))
nfl_data <- mutate(nfl_data, SD = ifelse(Teams == "SD",1,0))
nfl_data <- mutate(nfl_data, SEA = ifelse(Teams == "SEA",1,0))
nfl_data <- mutate(nfl_data, SF = ifelse(Teams == "SF",1,0))
nfl_data <- mutate(nfl_data, STL = ifelse(Teams == "STL/LA",1,0))
nfl_data <- mutate(nfl_data, TB = ifelse(Teams == "TB",1,0))
nfl_data <- mutate(nfl_data, TEN = ifelse(Teams == "TEN",1,0))
nfl_data <- mutate(nfl_data, WAS = ifelse(Teams == "WAS",1,0))



#calculate the averages by player, position, and team
#receiving
nfl_data <- nfl_data %>%
              group_by(player.1)%>%
                mutate(avg_recy_plyr = mean(recy))
nfl_data <- nfl_data %>%
              group_by(pos1)%>%
                mutate(avg_recy_pos = mean(recy))
nfl_data <- nfl_data %>%
              group_by(Teams)%>%
                mutate(avg_recy_team = mean(recy))
nfl_data <- nfl_data %>%
              group_by(player.1)%>%
                mutate(avg_rec_plyr = mean(rec))
nfl_data <- nfl_data %>%
              group_by(pos1)%>%
                mutate(avg_rec_pos = mean(rec))
nfl_data <- nfl_data %>%
              group_by(Teams)%>%
                mutate(avg_rec_team = mean(rec))
nfl_data <- nfl_data %>%
              group_by(player.1)%>%
                mutate(avg_trg_plyr = mean(trg))
nfl_data <- nfl_data %>%
              group_by(pos1)%>%
                mutate(avg_trg_pos = mean(trg))
nfl_data <- nfl_data %>%
              group_by(Teams)%>%
                mutate(avg_trg_team = mean(trg))
nfl_data <- nfl_data %>%
              group_by(player.1)%>%
                mutate(avg_rectd_plyr = mean(tdrec))
nfl_data <- nfl_data %>%
              group_by(pos1)%>%
                mutate(avg_rectd_pos = mean(tdrec))
nfl_data <- nfl_data %>%
              group_by(Teams)%>%
                mutate(avg_rectd_team = mean(tdrec))



#running
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rbra_plyr = mean(ra))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rbra_team = mean(ra))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rbra_pos = mean(ra))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rbry_plyr = mean(ry))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rbry_team = mean(ry))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rbry_pos = mean(ry))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_fuml_plyr = mean(fuml))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_fuml_team = mean(fuml))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_fuml_pos = mean(fuml))
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_tdr_plyr = mean(tdr))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_tdr_pos = mean(tdr))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_tdr_team = mean(tdr))



#passing
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpy_plyr = mean(py))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpy_team = mean(py))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpy_pos = mean(py))


nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpc_plyr = mean(pc))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpc_team = mean(pc))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpc_pos = mean(pc))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbints_plyr = mean(ints))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbints_team = mean(ints))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbints_pos = mean(ints))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbtdp_plyr = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbtdp_team = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbtdp_pos = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpa_plyr = mean(pa))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpa_team = mean(pa))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpa_pos = mean(pa))

#age

nfl_data <- nfl_data %>%
  group_by(age, pos1)%>%
  mutate(avg_recy_age = mean(recy))

nfl_data <- nfl_data %>%
  group_by(age, pos1)%>%
  mutate(age_count = n())
                  
#wr regression

linRegrecy <- lm(recy ~ height+ weight + cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                   age+ forty1 + vertical1  + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                   CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                   avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                   avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                   avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                   avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                   avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                   avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                   avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                   avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                   avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                   avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                   avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team +
                   grass_1 + bad_weather_1, data = nfl_data)
summary(linRegrecy)

linRegrecy2 <- lm(recy ~ age + ATL + BAL + BUF + CHI+
                    CIN + CLE + GB + HOU  + JAC + KC + NE + NOR + NYG+
                    OAK + SD + TB + TEN + WAS+ avg_recy_plyr+grass_1+bad_weather_1, data = nfl_data)

summary(linRegrecy2)





linRegrec <- lm(rec ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                   age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                   CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                  avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                  avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                  avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                  avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                  avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                  avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                  avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                  avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                  avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                  avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                  avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 + bad_weather_1, data = nfl_data)

summary(linRegrec)
  
linRegrec2 <- lm(rec ~ temp+ ATL + BAL + BUF + CHI+
                   CIN + CLE + GB + HOU + JAC + KC + NOR + NYG+
                   OAK +SD + WAS + avg_rec_plyr+ grass_1+
                   bad_weather_1, data = nfl_data)

summary(linRegrec2)

linRegtrg <- lm(trg ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                  age+ forty1 + vertical1  + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                  CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                  NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS +avg_recy_plyr+avg_recy_pos + 
                  avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                  avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                  avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                  avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                  avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                  avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                  avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                  avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                  avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                  avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                  avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 + bad_weather_1 , data = nfl_data)

summary(linRegtrg)

linRegtrg2 <- lm(trg ~ home_team_1+ ARI + ATL + BAL + BUF + CAR + CHI + CIN + CLE + 
                   DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + SD + STL + TB + TEN + WAS + avg_trg_plyr,
                 data = nfl_data)

summary(linRegtrg2)

linRegRecTD <- lm(trg ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                  age+ forty1 + vertical1  + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                  CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                  NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS +avg_recy_plyr+avg_recy_pos + 
                  avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                  avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                  avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                  avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                  avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                  avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                  avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                  avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                  avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                  avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                  avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 + bad_weather_1 , data = nfl_data)

summary(linRegRecTD)


linRegRecTD2 <- lm(tdrec ~weight+home_team_1+ ATL+ DAL + DEN + GB + NE + NOR + NYG+
                  SD + avg_recy_plyr+ avg_rec_plyr, data = nfl_data)

summary(linRegRecTD2)


linRegQBpyds <- lm(py ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                  age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                  CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                  NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                    avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                    avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                    avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                    avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                    avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                    avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                    avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                    avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                    avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                    avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                    avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 +
                  bad_weather_1, data = nfl_data)

summary(linRegQBpyds)

linRegQBpyds2 <- lm(py ~ cold_weather + BUF + CLE+ HOU + OAK + PHI+ avg_qbpy_plyr+ grass_1 + 
                     bad_weather_1, data = nfl_data)

summary(linRegQBpyds2)

linRegQBpc <- lm(pc ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                   age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                   CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS +avg_recy_plyr+avg_recy_pos + 
                   avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                   avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                   avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                   avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                   avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                   avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                   avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                   avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                   avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                   avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                   avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 +
                   bad_weather_1 , data = nfl_data)
summary(linRegQBpc)

linRegQBpc2 <- lm(pc ~ cold_weather + BUF + CLE+ HOU + KC+ OAK + PHI+ avg_qbpc_plyr+ grass_1 + 
                    bad_weather_1, data = nfl_data)

summary(linRegQBpc2)

linRegQBInts <- lm(ints ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                   age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                   CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS avg_recy_plyr+avg_recy_pos + 
                     avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                     avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                     avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                     avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                     avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                     avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                     avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                     avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                     avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                     avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                     avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 +
                   bad_weather_1, data = nfl_data)

summary(linRegQBInts) 

linRegQBInts2 <- lm(ints ~ avg_qbints_plyr, data = nfl_data)

summary(linRegQBInts2)

linRegQBpa <- lm(pa ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                     age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                     CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                     NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS +avg_recy_plyr+avg_recy_pos + 
                   avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                   avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                   avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                   avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                   avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                   avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                   avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                   avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                   avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                   avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                   avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team + grass_1 +
                     bad_weather_1 , data = nfl_data)

summary(linRegQBpa)

linRegQBpa2 <- lm(pa~ + cold_weather+ home_team_1+ age+ BUF + CLE+ DAL+ HOU + OAK + PHI+ 
                    avg_qbpc_plyr+avg_qbints_plyr+avg_qbtdp_plyr, data = nfl_data)

summary(linRegQBpa2)


linRegRushYd <- lm(ry ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                   age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                   CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                   NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                   avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                   avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                   avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                   avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                   avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                   avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                   avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                   avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                   avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                   avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                   avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team +
                   grass_1 + bad_weather_1 , data = nfl_data)

summary(linRegRushYd)

linRegRushYd2 <- lm(ry ~ home_team_1 + age +avg_rbry_plyr, data = nfl_data)

summary(linRegRushYd2)

linRegRushAtt <- lm(ra ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                     age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                     CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                     NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                     avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                     avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                      avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                     avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                     avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                     avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                     avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                     avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                     avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                     avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                     avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team +
                     grass_1 + bad_weather_1 , data = nfl_data)

summary(linRegRushAtt)

linRegRushAtt2 <- lm(ry ~ home_team_1 + age +avg_rbra_plyr, data = nfl_data)

summary(linRegRushAtt2)


linRegFumble <- lm(fuml ~ height+ weight+cold_weather + hot_weather + home_team_1+ temp+ is_WR + is_TE + is_RB + is_QB+
                      age+ forty1 + vertical1 + shuttle1+ cone1 + ARI + ATL + BAL + BUF + CAR + CHI+
                      CIN + CLE + DAL + DEN + DET + GB + HOU + IND + JAC + KC + MIA + MINN + NE + NOR + NYG+
                      NYJ + OAK + PHI + PIT +SD + SEA + STL + TB + TEN + WAS + avg_recy_plyr+avg_recy_pos + 
                      avg_recy_team + avg_rec_plyr +avg_rec_pos + avg_rec_team +avg_trg_plyr + avg_trg_pos +
                      avg_trg_team + avg_rectd_plyr + avg_rectd_pos +avg_rectd_team+
                      avg_tdr_plyr + avg_tdr_pos + avg_tdr_team +
                      avg_rbra_plyr + avg_rbra_pos +avg_rbra_team +
                      avg_rbry_plyr + avg_rbry_pos +avg_rbry_team +
                      avg_fuml_plyr + avg_fuml_pos +avg_fuml_team +
                      avg_qbpy_plyr + avg_qbpy_pos +avg_qbpy_team +
                      avg_qbpa_plyr + avg_qbpa_pos +avg_qbpa_team+
                      avg_qbpc_plyr + avg_qbpc_pos +avg_qbpc_team +
                      avg_qbints_plyr + avg_qbints_pos +avg_qbints_team +
                      avg_qbtdp_plyr + avg_qbtdp_pos +avg_qbtdp_team +
                      grass_1 + bad_weather_1 , data = nfl_data)

summary(linRegFumble)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point()+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 75), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "TE"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 50 & pos1 == "TE"), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "RB"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 30 & pos1 == "RB"), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "WR"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 70 & pos1 == "WR"), aes(label = pname), size = 2.5)


ggplot(data = subset(nfl_data, pos1 =="WR"), aes(x = age, y = avg_recy_age/age_count, col = Teams))+
  geom_bar(data = subset(nfl_data, pos1 == "WR"), stat = "identity")



write.csv(nfl_data, "C:/Users/Rich Lindberg/Documents/R/NFLCapstoneTest/testfile.csv")

