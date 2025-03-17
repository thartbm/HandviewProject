

# function optimization -----

exponentialModel <- function(par, timepoints) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  output = par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints )
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}


exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)) ) {
  
  MSE <- mean((exponentialModel(par, timepoints)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}


exponentialFit <- function(signal, timepoints=length(signal), gridpoints=11, gridfits=10, asymptoteRange=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  if (is.null(asymptoteRange)) {
    # set a wiiiiide range... especially for single participants, the range may or may not work depending on how noisy their data is
    asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  }
  
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints)
  
  
  
  
  lo <- c(0,asymptoteRange[1])
  hi <- c(1,asymptoteRange[2])
  
  
  
  

  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal
                            ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  
  # return the best parameters:
  return(winpar)
  
}


# fit exponential to learning curves -----


learningExponentials <- function() {
  
  for (group in c('control', 'cursorjump', 'handview')[1]) {
    
    
    df <- read.csv(sprintf('data/%s/%s_training_reachdevs.csv', group, group),
                   stringsAsFactors = FALSE)
    
    
    adf <- aggregate(reachdeviation_deg ~ trial_num, data=df, FUN=mean, na.rm=TRUE)
    
    exp_par <- exponentialFit( signal = adf$reachdeviation_deg)
    
    print(exp_par)
    
  }
  
  
}