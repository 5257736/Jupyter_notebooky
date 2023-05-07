#Druhy zapas
P <- matrix(c(39/110,1-20/92,1-39/110,20/92), nrow=2, ncol=2)
print(P)


# function to simulate one volleyball game 
volley_game_mc_sim <- function(P, max_points = 25) {
  
  # number of possible states
  num_states <- nrow(P)
  
  # stores the states X_t through time
  # initialize variable for first state 
  states <- rbinom(1,1,0.5)+1 # determination of the first serving team  
  A_points <- 0 
  B_points <- 0 
  n_A <- 0 
  n_B <- 0
  t <- 2 # time or round 
  
  while((A_points < max_points && B_points < max_points) || (abs(A_points-B_points)<2)) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states <- c(states, which(rmultinom(1, 1, p) == 1))  
    if(states[t] == 1) {
      A_points <- A_points+1 
      if(states[t-1] == 1) {
        n_A <- n_A+1
      }
    } else {
      B_points <- B_points+1 
      if(states[t-1] == 2) {
        n_B <- n_B+1
      }
    }
    t <- t+1 
  }
  return(list('states' = states, 'A_points' = A_points, 'B_points' = B_points, 'n_A' = n_A, 'n_B' = n_B))
} 


# function to count the number of runs of particular length (team A, team B) 
count_runs <- function(game, run_length) {
  if(run_length < 9) {
    num_runs_A <- 0 
    num_runs_B <- 0 
    len_val <- rle(game[-1]) # first service doesn't mean point 
    indices <- which(len_val$lengths %in% run_length)
    for(i in indices) {
      if(len_val$values[i] == 1) {
        num_runs_A <- num_runs_A+1
      } else {
        num_runs_B <- num_runs_B+1
      }
    }
    return(list('num_runs_A' = num_runs_A, 'num_runs_B' = num_runs_B)) 
    # number of runs with lenght >= 9 
  } else {
    num_runs_A <- 0 
    num_runs_B <- 0 
    len_val <- rle(game[-1]) # first service doesn't mean point 
    max_len_run <- max(len_val$lengths) 
    if(max_len_run >= run_length) { 
      for(j in run_length:max_len_run) {
        indices <- which(len_val$lengths %in% j)
        for(k in indices) {
          if(len_val$values[k] == 1) {
            num_runs_A <- num_runs_A+1
          } else {
            num_runs_B <- num_runs_B+1
          }
        }
      }
      return(list('num_runs_A' = num_runs_A, 'num_runs_B' = num_runs_B))
    } else {
      return(list('num_runs_A' = 0, 'num_runs_B' = 0))
    }
  }
  
}

library(plyr)
# the following function returns: 
# the counts of runs of particular length, the number of services 
# n_A, n_B, A_points, B_points 
count_runs_services_sideout <- function(game, runs_lengths) {
  num_runs <- matrix(NA,2,length(runs_lengths)) 
  col_names <- vector() 
  for(i in 1:length(runs_lengths)) {
    cr <- count_runs(game,runs_lengths[i])
    num_runs[1,i] <- cr$num_runs_A
    num_runs[2,i] <- cr$num_runs_B
    col_names <- c(col_names,as.character(runs_lengths[i]))
  }
  colnames(num_runs) <- col_names 
  row.names(num_runs) <- c('A','B')
  
  A_points <- 0 
  B_points <- 0 
  n_A <- 0 
  n_B <- 0
  for(t in 2:length(game)) {
    if(game[t] == 1) {
      A_points <- A_points+1 
      if(game[t-1] == 1) {
        n_A <- n_A+1
      }
    } else {
      B_points <- B_points+1 
      if(game[t-1] == 2) {
        n_B <- n_B+1
      }
    }
    t <- t+1
  }
  
  services <- count(game[-length(game)])
  A_services <- services$freq[1]
  B_services <- services$freq[2]
  
  # A_sideout_perc <- n_A/A_services
  # B_sideout_perc <- n_B/B_services 
  
  return(list('num_runs' = num_runs, 'A_points' = A_points, 'B_points' = B_points, 'n_A' = n_A, 'n_B' = n_B, 'A_services' = A_services, 'B_services' = B_services))
  
}

#https://uclabruins.com/sports/mens-volleyball/stats/2022/ohio-state/boxscore/29576
# UCLA vs Ohio State 
# 25:18  
# 25:15  
# 23:25
# 23:25
# 15:8

ohiostate21 <- read.csv2('z2ohio_state.csv', sep=";", header = FALSE)[1:44,1]
ohiostate22 <- read.csv2('z2ohio_state.csv', sep=";", header = FALSE)[1:41,2]
ohiostate23 <- read.csv2('z2ohio_state.csv', sep=";", header = FALSE)[,3]
ohiostate24 <- read.csv2('z2ohio_state.csv', sep=";", header = FALSE)[,4]
ohiostate25 <- read.csv2('z2ohio_state.csv', sep=";", header = FALSE)[1:24,5]

ohiostate21
ohiostate22
ohiostate23
ohiostate24
ohiostate25

ohiostate21_sum<- count_runs_services_sideout(ohiostate21,1:9) 
ohiostate21_sum$num_runs
ohiostate21_sum$A_points
ohiostate21_sum$B_points
ohiostate21_sum$n_A
ohiostate21_sum$n_B
ohiostate21_sum$A_services
ohiostate21_sum$B_services

ohiostate22_sum <- count_runs_services_sideout(ohiostate22,1:9) 
ohiostate22_sum$num_runs
ohiostate22_sum$A_points
ohiostate22_sum$B_points
ohiostate22_sum$n_A
ohiostate22_sum$n_B
ohiostate22_sum$A_services
ohiostate22_sum$B_services

ohiostate23_sum <- count_runs_services_sideout(ohiostate23,1:9)
ohiostate23_sum$num_runs
ohiostate23_sum$A_points
ohiostate23_sum$B_points
ohiostate23_sum$n_A
ohiostate23_sum$n_B
ohiostate23_sum$A_services
ohiostate23_sum$B_services

ohiostate24_sum <- count_runs_services_sideout(ohiostate24,1:9)
ohiostate24_sum$num_runs
ohiostate24_sum$A_points
ohiostate24_sum$B_points
ohiostate24_sum$n_A
ohiostate24_sum$n_B
ohiostate24_sum$A_services
ohiostate24_sum$B_services

ohiostate25_sum <- count_runs_services_sideout(ohiostate25,1:9)
ohiostate25_sum$num_runs
ohiostate25_sum$A_points
ohiostate25_sum$B_points
ohiostate25_sum$n_A
ohiostate25_sum$n_B
ohiostate25_sum$A_services
ohiostate25_sum$B_services

num_runs <- rbind(rbind(rbind(rbind(ohiostate21_sum$num_runs,ohiostate22_sum$num_runs),ohiostate23_sum$num_runs),ohiostate24_sum$num_runs),ohiostate25_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(ohiostate21_sum$n_A)/(ohiostate21_sum$n_A+(ohiostate21_sum$B_points- ohiostate21_sum$n_B))
(ohiostate21_sum$n_B)/(ohiostate21_sum$n_B+(ohiostate21_sum$A_points- ohiostate21_sum$n_A))

P1 <- matrix(c(0.36,1-0.1111111,1-0.36,0.1111111), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(ohiostate22_sum$n_A)/(ohiostate22_sum$n_A+(ohiostate22_sum$B_points- ohiostate22_sum$n_B))
(ohiostate22_sum$n_B)/(ohiostate22_sum$n_B+(ohiostate22_sum$A_points- ohiostate22_sum$n_A))

P2 <- matrix(c(0.4583333,1-0.125,1-0.4583333,0.125), nrow=2, ncol=2)
print(P2)

#Pre treti set
(ohiostate23_sum$n_A)/(ohiostate23_sum$n_A+(ohiostate23_sum$B_points- ohiostate23_sum$n_B))
(ohiostate23_sum$n_B)/(ohiostate23_sum$n_B+(ohiostate23_sum$A_points- ohiostate23_sum$n_A))

P3 <- matrix(c(0.2916667,1-0.3333333,1-0.2916667,0.3333333), nrow=2, ncol=2)
print(P3)

#Pre stvrty set
(ohiostate24_sum$n_A)/(ohiostate24_sum$n_A+(ohiostate24_sum$B_points- ohiostate24_sum$n_B))
(ohiostate24_sum$n_B)/(ohiostate24_sum$n_B+(ohiostate24_sum$A_points- ohiostate24_sum$n_A))

P4 <- matrix(c(0.2608696,1-0.32,1-0.2608696,0.32), nrow=2, ncol=2)
print(P4)

#Pre piaty set
(ohiostate25_sum$n_A)/(ohiostate25_sum$n_A+(ohiostate25_sum$B_points- ohiostate25_sum$n_B))
(ohiostate25_sum$n_B)/(ohiostate25_sum$n_B+(ohiostate25_sum$A_points- ohiostate25_sum$n_A))

P5 <- matrix(c(0.4285714,1-0,1-0.4285714,0), nrow=2, ncol=2)
print(P5)

# function to simulate games with given parameters 
# returns the counts of runs of particular length for each game 
sim_volley_games_proc <- function(P, max_points = 25, num_games, A_points, B_points) {
  num_runs <- matrix(NA,num_games,9) 
  games_counter <- 0 
  while(games_counter < num_games) {
    sim_game <- volley_game_mc_sim(P, max_points)
    if(sim_game$A_points == A_points && sim_game$B_points == B_points) {
      games_counter <- games_counter+1
      idx <- 1 
      for(k in 1:9) {
        counts <- count_runs(sim_game$states, k) 
        num_runs[games_counter,idx] <- counts$num_runs_A + counts$num_runs_B 
        idx <- idx+1
      }
      # check 
      # print(paste('game length: ', length(sim_game$states))) 
      # print(paste('sum of runsÂ´ lengths: ', t(num_runs[games_counter,])%*%1:9))
    }
  }
  # names of columns 
  colnames(num_runs) <- c('1','2','3','4','5','6','7','8','>=9')
  
  return(num_runs) 
}

#Simulacia prvy set
ohiostate21_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=25, B_points=18)
#Simulacia druhy set
ohiostate22_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=25, B_points=15)
#Simulacia treti set
ohiostate23_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=23, B_points=25)
#Simulacia stvrty set
ohiostate24_sim_1000 <- sim_volley_games_proc(P4, max_points=25, num_games=1000, A_points=23, B_points=25)
#Simulacia piaty set
ohiostate25_sim_1000 <- sim_volley_games_proc(P5, max_points=15, num_games=1000, A_points=15, B_points=8)


num_runs_sim <- rbind(rbind(rbind(rbind(ohiostate21_sim_1000,ohiostate22_sim_1000),ohiostate23_sim_1000),ohiostate24_sim_1000),ohiostate25_sim_1000)
colSums(num_runs_sim/1000)

#chi-kvadrát
body <- matrix(c(39,72,71,20), nrow=2, ncol=2)
print(body)
per<-matrix(c(60,51,50,41), nrow=2, byrow = TRUE)
print(per)
chisq.test(body,p=per)


body <- matrix(c(39, 72, 71, 20), nrow = 2, ncol = 2)
print(body)
exp_counts <- margin.table(body, 1) %*% t(margin.table(body, 2)) / sum(body)
print(exp_counts)
chisq.test(body, p = exp_counts)

