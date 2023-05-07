#Osmy zapas
P <- matrix(c(40/96,1-26/83,1-40/96,26/83), nrow=2, ncol=2)
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

#https://uclabruins.com/sports/mens-volleyball/stats/2022/uc-san-diego/boxscore/29609
#UCLA vs UC San Diego
# 22:25 
# 25:18
# 25:23
# 25:16

ucsandiego81 <- read.csv2('z8uc_san_diego.csv', sep=";", header = FALSE)[1:48,1]
ucsandiego82 <- read.csv2('z8uc_san_diego.csv', sep=";", header = FALSE)[1:44,2]
ucsandiego83 <- read.csv2('z8uc_san_diego.csv', sep=";", header = FALSE)[1:49,3]
ucsandiego84 <- read.csv2('z8uc_san_diego.csv', sep=";", header = FALSE)[1:42,4]

ucsandiego81
ucsandiego82
ucsandiego83
ucsandiego84

ucsandiego81_sum<- count_runs_services_sideout(ucsandiego81,1:9) 
ucsandiego81_sum$num_runs
ucsandiego81_sum$A_points
ucsandiego81_sum$B_points
ucsandiego81_sum$n_A
ucsandiego81_sum$n_B
ucsandiego81_sum$A_services
ucsandiego81_sum$B_services

ucsandiego82_sum <- count_runs_services_sideout(ucsandiego82,1:9) 
ucsandiego82_sum$num_runs
ucsandiego82_sum$A_points
ucsandiego82_sum$B_points
ucsandiego82_sum$n_A
ucsandiego82_sum$n_B
ucsandiego82_sum$A_services
ucsandiego82_sum$B_services

ucsandiego83_sum <- count_runs_services_sideout(ucsandiego83,1:9)
ucsandiego83_sum$num_runs
ucsandiego83_sum$A_points
ucsandiego83_sum$B_points
ucsandiego83_sum$n_A
ucsandiego83_sum$n_B
ucsandiego83_sum$A_services
ucsandiego83_sum$B_services

ucsandiego84_sum <- count_runs_services_sideout(ucsandiego84,1:9)
ucsandiego84_sum$num_runs
ucsandiego84_sum$A_points
ucsandiego84_sum$B_points
ucsandiego84_sum$n_A
ucsandiego84_sum$n_B
ucsandiego84_sum$A_services
ucsandiego84_sum$B_services


num_runs <- rbind(rbind(rbind(ucsandiego81_sum$num_runs,ucsandiego82_sum$num_runs),ucsandiego83_sum$num_runs),ucsandiego84_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(ucsandiego81_sum$n_A)/(ucsandiego81_sum$n_A+(ucsandiego81_sum$B_points- ucsandiego81_sum$n_B))
(ucsandiego81_sum$n_B)/(ucsandiego81_sum$n_B+(ucsandiego81_sum$A_points- ucsandiego81_sum$n_A))

P1 <- matrix(c(0.2727273,1-0.36,1-0.2727273,0.36), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(ucsandiego82_sum$n_A)/(ucsandiego82_sum$n_A+(ucsandiego82_sum$B_points- ucsandiego82_sum$n_B))
(ucsandiego82_sum$n_B)/(ucsandiego82_sum$n_B+(ucsandiego82_sum$A_points- ucsandiego82_sum$n_A))

P2 <- matrix(c(0.44,1-0.2222222,1-0.44,0.2222222), nrow=2, ncol=2)
print(P2)

#Pre treti set
(ucsandiego83_sum$n_A)/(ucsandiego83_sum$n_A+(ucsandiego83_sum$B_points- ucsandiego83_sum$n_B))
(ucsandiego83_sum$n_B)/(ucsandiego83_sum$n_B+(ucsandiego83_sum$A_points- ucsandiego83_sum$n_A))

P3 <- matrix(c(0.5,1-0.4583333,1-0.5,0.4583333), nrow=2, ncol=2)
print(P3)

#Pre stvrty set
(ucsandiego84_sum$n_A)/(ucsandiego84_sum$n_A+(ucsandiego84_sum$B_points- ucsandiego84_sum$n_B))
(ucsandiego84_sum$n_B)/(ucsandiego84_sum$n_B+(ucsandiego84_sum$A_points- ucsandiego84_sum$n_A))

P4 <- matrix(c(0.44,1-0.125,1-0.44,0.125), nrow=2, ncol=2)
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
ucsandiego81_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=22, B_points=25)
#Simulacia druhy set
ucsandiego82_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=25, B_points=18)
#Simulacia treti set
ucsandiego83_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=25, B_points=23)
#Simulacia stvrty set
ucsandiego84_sim_1000 <- sim_volley_games_proc(P4, max_points=25, num_games=1000, A_points=25, B_points=16)


num_runs_sim <- rbind(rbind(rbind(ucsandiego81_sim_1000,ucsandiego82_sim_1000),ucsandiego83_sim_1000),ucsandiego84_sim_1000)
colSums(num_runs_sim/1000)
