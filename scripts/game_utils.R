## Game simulator utility file

## Initialize values
boxes <- c("full", "empty")
yum <- 2
results_win <- c()
results_picks <- c()

## Gaming fct
play_game <- function(n_of_plays) {
  #set.seed(42)
  ## Loop number of plays
  for (i in 1:n_of_plays) {
    ## Initial success
    success <- FALSE
    ## Initial number of picks
    n_of_picks <- 1
    while (!success) {
      pick <- sample(boxes, 1, replace = TRUE)
      if (pick == "empty") {
        ## If empty box, double value in non-empty box
        yum <- yum * 2
        ## Shuffle boxes
        boxes <- sample(boxes)
        ## Keep track of number of picks needed
        n_of_picks <- n_of_picks + 1
      } else {
        ## Update success
        success <- TRUE
        ## Take money in non-empty box
        win <- yum
        ## Reset money in non-empty box
        yum <- 2
        ## Append results
        results_win <- c(results_win, win)
        results_picks <- c(results_picks, n_of_picks)
      }
    }
  }
  
  results <- data.frame(results_picks,results_win)
  return(results)
}
