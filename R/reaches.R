
getParticipantTraining <- function(group, participant) {
  
  AL_file <- sprintf('data/%s/%s/%s_aligned_training.csv', group, participant, participant)
  RO_file <- sprintf('data/%s/%s/%s_rotated_training.csv', group, participant, participant)
  
  AL_df <- read.csv( file = AL_file,
                     stringsAsFactors = F)
  RO_df <- read.csv( file = RO_file,
                     stringsAsFactors = F)
  
  return( list('aligned'=AL_df,
               'rotated'=RO_df) )
  
}


getGroupTraining <- function(group) {
  
  participants <- groupParticipants(group = group)
  
  # print(participants)
  
  all_baselines <- NA
  
  for (participant in participants) {
    
    participant_df <- getParticipantTraining( group       = group,
                                              participant = participant )
    
    # print(participant_df)
    
    baseline <- getBaseline( df = participant_df[['aligned']] )
    
    baseline$reachdeviation_deg[which(abs(baseline$reachdeviation_deg) >= 50)] <- NA
    
    baseline <- aggregate(reachdeviation_deg ~ targetangle_deg, data=baseline, FUN=median, na.rm=TRUE)
    
    baseline$participant <- participant
    
    if (is.data.frame(all_baselines)) {
      all_baselines <- rbind(all_baselines, baseline)
    } else {
      all_baselines <- baseline
    }
    
    # removeOutliers(baseline, rotation=0)
    
    # rotated <- getRotatedLearning( df = participant_df[['rotated']] )
    
    # removeOutliers(rotated, rotation=30)
    
  }
  
  plot(x=all_baselines$trial_num,
       y=all_baselines$reachdeviation_deg)
  
}

getBaseline <- function(df) {
  
  # str(df)
  df <- df[which(df$trial_num %in% c(31:45)),]
  # str(df)
  
  trialnos <- unique(df$trial_num)
  
  outdf <- NA
  
  for (trial in trialnos) {
    
    tdf <- df[which(df$trial_num == trial),]
    
    reachdev <- getReachDeviation(tdf)
    
    reachdev <- data.frame(t(data.frame(reachdev)))
    
    if (is.data.frame(outdf)) {
      outdf <- rbind(outdf, reachdev)
    } else {
      outdf <- reachdev
    }
    
  }
  
  return(outdf)
  
}

getReachDeviation <- function(df) {
  
  
  target <- df$targetangle_deg[1]
  
  X <- df$handx_cm
  Y <- df$handy_cm
  
  # at 1/4 the target distance
  # target distance was 10 cm
  # 2.5 cm
  
  distances <- sqrt(X^2 + Y^2)
  idx <- which(distances > 2.5)[1]
  
  # print(idx)
  x <- X[idx]
  y <- Y[idx]
  
  th <- (-1*target/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # rotate the coordinates, add the origin back in
  norm_sample <- matrix(data=c(x,y),ncol=2) %*% R
  
  # print(norm_sample)
  
  reachdev <- (atan2(norm_sample[2], norm_sample[1]) / pi) * 180
  
  # print(reachdev)
  
  return(c('trial_num' = df$trial_num[1],
           'targetangle_deg'=target,
           'reachdeviation_deg'=reachdev))
  
}

