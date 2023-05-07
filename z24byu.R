#Dvadsiaty stvrty zapas
P <- matrix(c(33/110,1-30/108,1-33/110,30/108), nrow=2, ncol=2)
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

#https://uclabruins.com/sports/mens-volleyball/stats/2022/brigham-young/boxscore/29612
# UCLA vs BYU 
# 26:28  
# 20:25  
# 25:23
# 25:22
# 15:9

byu241 <- read.csv2('z24byu.csv', sep=";", header = FALSE)[1:55,1]
byu242 <- read.csv2('z24byu.csv', sep=";", header = FALSE)[1:46,2]
byu243 <- read.csv2('z24byu.csv', sep=";", header = FALSE)[1:49,3]
byu244 <- read.csv2('z24byu.csv', sep=";", header = FALSE)[1:48,4]
byu245 <- read.csv2('z24byu.csv', sep=";", header = FALSE)[1:25,5]

byu241
byu242
byu243
byu244
byu245

byu241_sum<- count_runs_services_sideout(byu241,1:9) 
byu241_sum$num_runs
byu241_sum$A_points
byu241_sum$B_points
byu241_sum$n_A
byu241_sum$n_B
byu241_sum$A_services
byu241_sum$B_services

byu242_sum <- count_runs_services_sideout(byu242,1:9) 
byu242_sum$num_runs
byu242_sum$A_points
byu242_sum$B_points
byu242_sum$n_A
byu242_sum$n_B
byu242_sum$A_services
byu242_sum$B_services

byu243_sum <- count_runs_services_sideout(byu243,1:9)
byu243_sum$num_runs
byu243_sum$A_points
byu243_sum$B_points
byu243_sum$n_A
byu243_sum$n_B
byu243_sum$A_services
byu243_sum$B_services

byu244_sum <- count_runs_services_sideout(byu244,1:9)
byu244_sum$num_runs
byu244_sum$A_points
byu244_sum$B_points
byu244_sum$n_A
byu244_sum$n_B
byu244_sum$A_services
byu244_sum$B_services

byu245_sum <- count_runs_services_sideout(byu245,1:9)
byu245_sum$num_runs
byu245_sum$A_points
byu245_sum$B_points
byu245_sum$n_A
byu245_sum$n_B
byu245_sum$A_services
byu245_sum$B_services

num_runs <- rbind(rbind(rbind(rbind(byu241_sum$num_runs,byu242_sum$num_runs),byu243_sum$num_runs),byu244_sum$num_runs),byu245_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(byu241_sum$n_A)/(byu241_sum$n_A+(byu241_sum$B_points- byu241_sum$n_B))
(byu241_sum$n_B)/(byu241_sum$n_B+(byu241_sum$A_points- byu241_sum$n_A))

P1 <- matrix(c(0.3076923,1-0.3571429,1-0.3076923,0.3571429), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(byu242_sum$n_A)/(byu242_sum$n_A+(byu242_sum$B_points- byu242_sum$n_B))
(byu242_sum$n_B)/(byu242_sum$n_B+(byu242_sum$A_points- byu242_sum$n_A))

P2 <- matrix(c(0.2857143,1-0.4166667,1-0.2857143,0.4166667), nrow=2, ncol=2)
print(P2)

#Pre treti set
(byu243_sum$n_A)/(byu243_sum$n_A+(byu243_sum$B_points- byu243_sum$n_B))
(byu243_sum$n_B)/(byu243_sum$n_B+(byu243_sum$A_points- byu243_sum$n_A))

P3 <- matrix(c(0.25,1-0.2083333,1-0.25,0.2083333), nrow=2, ncol=2)
print(P3)

#Pre stvrty set
(byu244_sum$n_A)/(byu244_sum$n_A+(byu244_sum$B_points- byu244_sum$n_B))
(byu244_sum$n_B)/(byu244_sum$n_B+(byu244_sum$A_points- byu244_sum$n_A))

P4 <- matrix(c(0.28,1-0.1818182,1-0.28,0.1818182), nrow=2, ncol=2)
print(P4)

#Pre piaty set
(byu245_sum$n_A)/(byu245_sum$n_A+(byu245_sum$B_points- byu245_sum$n_B))
(byu245_sum$n_B)/(byu245_sum$n_B+(byu245_sum$A_points- byu245_sum$n_A))

P5 <- matrix(c(0.4285714,1-0.1,1-0.4285714,0.1), nrow=2, ncol=2)
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
byu241_sim_1000 <- sim_volley_games_proc(P1, max_points=28, num_games=1000, A_points=26, B_points=28)
#Simulacia druhy set
byu242_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=20, B_points=25)
#Simulacia treti set
byu243_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=25, B_points=23)
#Simulacia stvrty set
byu244_sim_1000 <- sim_volley_games_proc(P4, max_points=25, num_games=1000, A_points=25, B_points=22)
#Simulacia piaty set
byu245_sim_1000 <- sim_volley_games_proc(P5, max_points=15, num_games=1000, A_points=15, B_points=9)


num_runs_sim <- rbind(rbind(rbind(rbind(byu241_sim_1000,byu242_sim_1000),byu243_sim_1000),byu244_sim_1000),byu245_sim_1000)
colSums(num_runs_sim/1000)
