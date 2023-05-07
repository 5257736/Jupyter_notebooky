#Desiaty zapas
P <- matrix(c(41/109,1-37/105,1-41/109,37/105), nrow=2, ncol=2)
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

#https://uclabruins.com/sports/mens-volleyball/stats/2022/long-beach-state/boxscore/29603
#UCLA vs Long Beach State
# 25:20 
# 25:23
# 23:25
# 21:25
# 15:12

longbeachstate101 <- read.csv2('z10long_beach_state.csv', sep=";", header = FALSE)[1:46,1]
longbeachstate102 <- read.csv2('z10long_beach_state.csv', sep=";", header = FALSE)[1:49,2] 
longbeachstate103 <- read.csv2('z10long_beach_state.csv', sep=";", header = FALSE)[1:49,3]
longbeachstate104 <- read.csv2('z10long_beach_state.csv', sep=";", header = FALSE)[1:47,4] 
longbeachstate105 <- read.csv2('z10long_beach_state.csv', sep=";", header = FALSE)[1:28,5]
longbeachstate101
longbeachstate102
longbeachstate103
longbeachstate104
longbeachstate105

longbeachstate101_sum<- count_runs_services_sideout(longbeachstate101,1:9) 
longbeachstate101_sum$num_runs
longbeachstate101_sum$A_points
longbeachstate101_sum$B_points
longbeachstate101_sum$n_A
longbeachstate101_sum$n_B
longbeachstate101_sum$A_services
longbeachstate101_sum$B_services

longbeachstate102_sum <- count_runs_services_sideout(longbeachstate102,1:9) 
longbeachstate102_sum$num_runs
longbeachstate102_sum$A_points
longbeachstate102_sum$B_points
longbeachstate102_sum$n_A
longbeachstate102_sum$n_B
longbeachstate102_sum$A_services
longbeachstate102_sum$B_services

longbeachstate103_sum <- count_runs_services_sideout(longbeachstate103,1:9)
longbeachstate103_sum$num_runs
longbeachstate103_sum$A_points
longbeachstate103_sum$B_points
longbeachstate103_sum$n_A
longbeachstate103_sum$n_B
longbeachstate103_sum$A_services
longbeachstate103_sum$B_services

longbeachstate104_sum <- count_runs_services_sideout(longbeachstate104,1:9)
longbeachstate104_sum$num_runs
longbeachstate104_sum$A_points
longbeachstate104_sum$B_points
longbeachstate104_sum$n_A
longbeachstate104_sum$n_B
longbeachstate104_sum$A_services
longbeachstate104_sum$B_services

longbeachstate105_sum <- count_runs_services_sideout(longbeachstate105,1:9)
longbeachstate105_sum$num_runs
longbeachstate105_sum$A_points
longbeachstate105_sum$B_points
longbeachstate105_sum$n_A
longbeachstate105_sum$n_B
longbeachstate105_sum$A_services
longbeachstate105_sum$B_services

num_runs <- rbind(rbind(rbind(rbind(longbeachstate101_sum$num_runs,longbeachstate102_sum$num_runs),longbeachstate103_sum$num_runs),longbeachstate104_sum$num_runs),longbeachstate105_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(longbeachstate101_sum$n_A)/(longbeachstate101_sum$n_A+(longbeachstate101_sum$B_points- longbeachstate101_sum$n_B))
(longbeachstate101_sum$n_B)/(longbeachstate101_sum$n_B+(longbeachstate101_sum$A_points- longbeachstate101_sum$n_A))

P1 <- matrix(c(0.4166667,1-0.2857143,1-0.4166667,0.2857143), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(longbeachstate102_sum$n_A)/(longbeachstate102_sum$n_A+(longbeachstate102_sum$B_points- longbeachstate102_sum$n_B))
(longbeachstate102_sum$n_B)/(longbeachstate102_sum$n_B+(longbeachstate102_sum$A_points- longbeachstate102_sum$n_A))

P2 <- matrix(c(0.32,1-0.2608696,1-0.32,0.2608696), nrow=2, ncol=2)
print(P2)

#Pre treti set
(longbeachstate103_sum$n_A)/(longbeachstate103_sum$n_A+(longbeachstate103_sum$B_points- longbeachstate103_sum$n_B))
(longbeachstate103_sum$n_B)/(longbeachstate103_sum$n_B+(longbeachstate103_sum$A_points- longbeachstate103_sum$n_A))

P3 <- matrix(c(0.3043478,1-0.36,1-0.3043478,0.36), nrow=2, ncol=2)
print(P3)

#Pre stvrty set
(longbeachstate104_sum$n_A)/(longbeachstate104_sum$n_A+(longbeachstate104_sum$B_points- longbeachstate104_sum$n_B))
(longbeachstate104_sum$n_B)/(longbeachstate104_sum$n_B+(longbeachstate104_sum$A_points- longbeachstate104_sum$n_A))

P4 <- matrix(c(0.3636364,1-0.4583333,1-0.3636364,0.4583333), nrow=2, ncol=2)
print(P4)

#Pre piaty set
(longbeachstate105_sum$n_A)/(longbeachstate105_sum$n_A+(longbeachstate105_sum$B_points- longbeachstate105_sum$n_B))
(longbeachstate105_sum$n_B)/(longbeachstate105_sum$n_B+(longbeachstate105_sum$A_points- longbeachstate105_sum$n_A))

P5 <- matrix(c(0.5333333,1-0.4166667,1-0.5333333,0.4166667), nrow=2, ncol=2)
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
      # print(paste('sum of runs´ lengths: ', t(num_runs[games_counter,])%*%1:9))
    }
  }
  # names of columns 
  colnames(num_runs) <- c('1','2','3','4','5','6','7','8','>=9')
  
  return(num_runs) 
}

#Simulacia prvy set
longbeachstate101_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=25, B_points=20)
#Simulacia druhy set
longbeachstate102_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=25, B_points=23)
#Simulacia treti set
longbeachstate103_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=23, B_points=25)
#Simulacia stvrty set
longbeachstate104_sim_1000 <- sim_volley_games_proc(P4, max_points=25, num_games=1000, A_points=21, B_points=25)
#Simulacia piaty set
longbeachstate105_sim_1000 <- sim_volley_games_proc(P5, max_points=15, num_games=1000, A_points=15, B_points=12)

num_runs_sim <- rbind(rbind(rbind(rbind(longbeachstate101_sim_1000,longbeachstate102_sim_1000),longbeachstate103_sim_1000),longbeachstate104_sim_1000),longbeachstate105_sim_1000)
colSums(num_runs_sim/1000)
