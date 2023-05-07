#Treti zapas
P <- matrix(c(32/94,1-30/93,1-32/94,30/93), nrow=2, ncol=2)
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

#https://uclabruins.com/sports/mens-volleyball/stats/2022/penn-state/boxscore/29597
# UCLA vs Penn State 
# 17:25  
# 28:26
# 25:22
# 25:19


penstate31 <- read.csv2('z3penn_state.csv', sep=";", header = FALSE)[1:43,1]
penstate32 <- read.csv2('z3penn_state.csv', sep=";", header = FALSE)[1:55,2]
penstate33 <- read.csv2('z3penn_state.csv', sep=";", header = FALSE)[1:48,3]
penstate34 <- read.csv2('z3penn_state.csv', sep=";", header = FALSE)[1:45,4]

penstate31
penstate32
penstate33
penstate34

penstate31_sum<- count_runs_services_sideout(penstate31,1:9) 
penstate31_sum$num_runs
penstate31_sum$A_points
penstate31_sum$B_points
penstate31_sum$n_A
penstate31_sum$n_B
penstate31_sum$A_services
penstate31_sum$B_services

penstate32_sum <- count_runs_services_sideout(penstate32,1:9) 
penstate32_sum$num_runs
penstate32_sum$A_points
penstate32_sum$B_points
penstate32_sum$n_A
penstate32_sum$n_B
penstate32_sum$A_services
penstate32_sum$B_services

penstate33_sum <- count_runs_services_sideout(penstate33,1:9)
penstate33_sum$num_runs
penstate33_sum$A_points
penstate33_sum$B_points
penstate33_sum$n_A
penstate33_sum$n_B
penstate33_sum$A_services
penstate33_sum$B_services

penstate34_sum <- count_runs_services_sideout(penstate34,1:9)
penstate34_sum$num_runs
penstate34_sum$A_points
penstate34_sum$B_points
penstate34_sum$n_A
penstate34_sum$n_B
penstate34_sum$A_services
penstate34_sum$B_services


num_runs <- rbind(rbind(rbind(penstate31_sum$num_runs,penstate32_sum$num_runs),penstate33_sum$num_runs),penstate34_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(penstate31_sum$n_A)/(penstate31_sum$n_A+(penstate31_sum$B_points- penstate31_sum$n_B))
(penstate31_sum$n_B)/(penstate31_sum$n_B+(penstate31_sum$A_points- penstate31_sum$n_A))

P1 <- matrix(c(0.1111111,1-0.375,1-0.1111111,0.375), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(penstate32_sum$n_A)/(penstate32_sum$n_A+(penstate32_sum$B_points- penstate32_sum$n_B))
(penstate32_sum$n_B)/(penstate32_sum$n_B+(penstate32_sum$A_points- penstate32_sum$n_A))

P2 <- matrix(c(0.3703704,1-0.3333333,1-0.3703704,0.3333333), nrow=2, ncol=2)
print(P2)

#Pre treti set
(penstate33_sum$n_A)/(penstate33_sum$n_A+(penstate33_sum$B_points- penstate33_sum$n_B))
(penstate33_sum$n_B)/(penstate33_sum$n_B+(penstate33_sum$A_points- penstate33_sum$n_A))

P3 <- matrix(c(0.32,1-0.2272727,1-0.32,0.2272727), nrow=2, ncol=2)
print(P3)

#Pre stvrty set
(penstate34_sum$n_A)/(penstate34_sum$n_A+(penstate34_sum$B_points- penstate34_sum$n_B))
(penstate34_sum$n_B)/(penstate34_sum$n_B+(penstate34_sum$A_points- penstate34_sum$n_A))

P4 <- matrix(c(0.5,1-0.35,1-0.5,0.35), nrow=2, ncol=2)
print(P4)


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
penstate31_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=17, B_points=25)
#Simulacia druhy set
penstate32_sim_1000 <- sim_volley_games_proc(P2, max_points=28, num_games=1000, A_points=28, B_points=26)
#Simulacia treti set
penstate33_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=25, B_points=22)
#Simulacia stvrty set
penstate34_sim_1000 <- sim_volley_games_proc(P4, max_points=25, num_games=1000, A_points=25, B_points=19)


num_runs_sim <- rbind(rbind(rbind(penstate31_sim_1000,penstate32_sim_1000),penstate33_sim_1000),penstate34_sim_1000)
colSums(num_runs_sim/1000)
