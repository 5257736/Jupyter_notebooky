#Prvy zapas
P <- matrix(c(36/73,1-19/58,1-36/73,19/58), nrow=2, ncol=2)
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

# https://uclabruins.com/sports/mens-volleyball/stats/2022/princeton/boxscore/29596
# UCLA vs Princeton 
# 25:12  
# 25:22  
# 25:22

princeton11 <- read.csv2('z1princeton.csv', sep=";", header = FALSE)[1:38,1]
princeton12 <- read.csv2('z1princeton.csv', sep=";", header = FALSE)[,2] 
princeton13 <- read.csv2('z1princeton.csv', sep=";", header = FALSE)[,3]
princeton11
princeton12
princeton13

princeton11_sum<- count_runs_services_sideout(princeton11,1:9) 
princeton11_sum$num_runs
princeton11_sum$A_points
princeton11_sum$B_points
princeton11_sum$n_A
princeton11_sum$n_B
princeton11_sum$A_services
princeton11_sum$B_services

princeton12_sum <- count_runs_services_sideout(princeton12,1:9) 
princeton12_sum$num_runs
princeton12_sum$A_points
princeton12_sum$B_points
princeton12_sum$n_A
princeton12_sum$n_B
princeton12_sum$A_services
princeton12_sum$B_services

princeton13_sum <- count_runs_services_sideout(princeton13,1:9)
princeton13_sum$num_runs
princeton13_sum$A_points
princeton13_sum$B_points
princeton13_sum$n_A
princeton13_sum$n_B
princeton13_sum$A_services
princeton13_sum$B_services

num_runs <- rbind(rbind(princeton11_sum$num_runs,princeton12_sum$num_runs),princeton13_sum$num_runs)
colSums(num_runs)

#Pre prvy set
(princeton11_sum$n_A)/(princeton11_sum$n_A+(princeton11_sum$B_points- princeton11_sum$n_B))
(princeton11_sum$n_B)/(princeton11_sum$n_B+(princeton11_sum$A_points- princeton11_sum$n_A))

P1 <- matrix(c(0.5833333,1-0.1538462,1-0.5833333,0.1538462), nrow=2, ncol=2)
print(P1)

#Pre druhy set
(princeton12_sum$n_A)/(princeton12_sum$n_A+(princeton12_sum$B_points- princeton12_sum$n_B))
(princeton12_sum$n_B)/(princeton12_sum$n_B+(princeton12_sum$A_points- princeton12_sum$n_A))

P2 <- matrix(c(0.52,1-0.4545455,1-0.52,0.4545455), nrow=2, ncol=2)
print(P2)

#Pre treti set
(princeton13_sum$n_A)/(princeton13_sum$n_A+(princeton13_sum$B_points- princeton13_sum$n_B))
(princeton13_sum$n_B)/(princeton13_sum$n_B+(princeton13_sum$A_points- princeton13_sum$n_A))

P3 <- matrix(c(0.375,1-0.3043478,1-0.375,0.3043478), nrow=2, ncol=2)
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
      # print(paste('sum of runsÂ´ lengths: ', t(num_runs[games_counter,])%*%1:9))
    }
  }
  # names of columns 
  colnames(num_runs) <- c('1','2','3','4','5','6','7','8','>=9')
  
  return(num_runs) 
}

#Simulacia prvy set
princeton11_sim_1000 <- sim_volley_games_proc(P1, max_points=25, num_games=1000, A_points=25, B_points=12)
#Simulacia druhy set
princeton12_sim_1000 <- sim_volley_games_proc(P2, max_points=25, num_games=1000, A_points=25, B_points=22)
#Simulacia treti set
princeton13_sim_1000 <- sim_volley_games_proc(P3, max_points=25, num_games=1000, A_points=25, B_points=22)

num_runs_sim <- rbind(rbind(princeton11_sim_1000,princeton12_sim_1000),princeton13_sim_1000)
colSums(num_runs_sim/1000)



#chi-kvadrát
body <- matrix(c(36,39,37,19), nrow=2, ncol=2)
print(body)
per<-matrix(c(42,33,31,25), nrow=2, ncol=2)
print(per)
chisq.test(body,p=per)

tab<-matrix(c(36,39,37,19), nrow=2, ncol=2)
tab
exp_tab<-matrix(c(36/73,1-19/58,1-36/73,19/58),nrow=2, byrow=TRUE)
exp_tab
chisq.test(tab,p=exp_tab)
