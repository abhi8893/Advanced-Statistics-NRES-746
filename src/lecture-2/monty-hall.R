############################################################
#####################** Description **######################
# Monty hall problem
############################################################

monty <- function(strat="stay", N=1000, n_doors=3, print_games=FALSE){

  doors <- 1:n_doors
  win <- 0
  
  for (i in 1:N){
    prize <- sample(doors, 1)
    guess <- sample(doors, 1)
    
    if (prize!=guess)
      reveal <- doors[-c(prize, guess)]
    else
      reveal <- sample(doors[-c(prize, guess)], n_doors - 2)
    
    if (strat == "switch")
      select <- doors[-c(reveal, guess)]
    if (strat == "stay")
      select <- guess
    if (strat == "random")
      select <- sample(doors[-c(reveal)], 1)
    
    if (select == prize){
      win <- win + 1
      outcome <- "Winner!"
    } else {
      outcome <- "Loser!"
    }
    
    if (print_games){
      cat(paste('Guess: ', guess,
                '\nRevealed: ', reveal,
                '\nSelection: ', select,
                '\nPrize Door: ', prize,
                '\n', outcome, '\n\n', sep = ''))
    }
    
    
  }
  cat(paste('Using the ', strat, ' strategy, your win percentage was ', win*100/N, "%\n", sep=''))
  
}
