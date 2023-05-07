#Strnasty zapas
P <- matrix(c(32/73,1-18/61,1-32/73,18/73), nrow=2, ncol=2)
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

#https://uclabruins.com/sports/mens-volleyball/stats/2022/grand-canyon/boxscore/29606
#UCLA vs Grand Canyon
# 25:18 
# 25:20
# 25:21

gc141 <- read.csv2('z14grand_canyon.csv', sep=";", header = FALSE)[1:44,1]
gc142 <- read.csv2('z14grand_canyon.csv', sep=";", header = FALSE)[1:46,2] 
gc143 <- read.csv2('z14grand_canyon.csv', sep=";", header = FALSE)[1:47,3]
gc141
gc142
gc143

gc141_sum<- count_runs_services_sideout(gc141,1:9) 
gc141_sum$num_runs
gc141_sum$A_points
gc141_sum$B_points
gc141_sum$n_A
gc141_sum$n_B
gc141_sum$A_services
gc141_sum$B_services

gc142_sum <- count_runs_services_sideout(gc142,1:9) 
gc142_sum$num_runs
gc142_sum$A_points
gc142_sum$B_points
gc142_sum$n_A
gc142_sum$n_B
gc142_sum$A_services
gc142_sum$B_services

gc143_sum <- count_runs_services_sideout(gc143,1:9)
gc143_sum$num_runs
gc143_sum$A_points
gc143_sum$B_points
gc143_sum$n_A
gc143_sum$n_B
gc143_sum$A_services
gc143_sum$B_services

num_runs <- rbind(rbind(gc141_sum$num_runs,gc142_sum$num_runs),gc143_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(gc141_sum$n_A)/(gc141_sum$n_A+(gc141_sum$B_points- gc141_sum$n_B))
(gc141_sum$n_B)/(gc141_sum$n_B+(gc141_sum$A_points- gc141_sum$n_A))

P1 <- matrix(c(0.4583333,1-0.2631579,1-0.4583333,0.2631579), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(gc142_sum$n_A)/(gc142_sum$n_A+(gc142_sum$B_points- gc142_sum$n_B))
(gc142_sum$n_B)/(gc142_sum$n_B+(gc142_sum$A_points- gc142_sum$n_A))

P2 <- matrix(c(0.48,1-0.35,1-0.48,0.35), nrow=2, ncol=2)
print(P2)

#Pre treti set
(gc143_sum$n_A)/(gc143_sum$n_A+(gc143_sum$B_points- gc143_sum$n_B))
(gc143_sum$n_B)/(gc143_sum$n_B+(gc143_sum$A_points- gc143_sum$n_A))

P3 <- matrix(c(0.375,1-0.2727273,1-0.375,0.2727273), nrow=2, ncol=2)
print(P3)


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
gc141_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=25, B_points=18)
#Simulacia druhy set
gc142_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=25, B_points=20)
#Simulacia treti set
gc143_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=25, B_points=21)

num_runs_sim <- rbind(rbind(gc141_sim_1000,gc142_sim_1000),gc143_sim_1000)
colSums(num_runs_sim/1000)
