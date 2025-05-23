
# generic plotting functions -----

setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  if (target == 'pdf') {
    pdf(file   = filename, 
        width  = width, 
        height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    png( filename = filename,
         width = width*dpi,
         height = height*dpi,
         res = dpi
    )
  }
  if (target == 'tiff') {
    tiff( filename = filename,
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
}



getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}

# training plots -----

plotTraining <- function(target='inline',main=NULL) {
  
  
  setupFigureFile(target=target,
                  width = 8,
                  height=6,
                  dpi=300,
                  sprintf('doc/fig2a_training.%s', target))
  
  groups <- c('control', 'cursorjump', 'handview')
  
  colors <- getColors()
  
  plot(NA, NA,
       main='', xlab='trial', ylab='reach deviation [°]',
       xlim=c(0,91), ylim=c(-10,40),
       ax=F, bty='n')
  
  if(!is.null(main)) {
    title(main=main, adj=0, cex.main=2)
  }
  
  lines(x=c(0,91),
        y=c(30,30),
        col='#999999',
        lw=2,
        lty=2)
  lines(x=c(0,91),
        y=c(0,0),
        col='#999999',
        lw=2,
        lty=2)
  
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    
    df <- read.csv(sprintf('data/%s/%s_training_reachdevs.csv', group, group),
                   stringsAsFactors = F)
    
    
    CI <- aggregate(reachdeviation_deg ~ trial_num,
                     data = df,
                     FUN = Reach::getConfidenceInterval)
    
    lo <- CI$reachdeviation_deg[,1]
    hi <- CI$reachdeviation_deg[,2]
    
    polygon( x = c( CI$trial_num, rev(CI$trial_num)),
             y = c( lo, rev(hi)),
             border=NA,
             col = colors$tr[group_no])
    
  }
  
  
  
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    
    df <- read.csv(sprintf('data/%s/%s_training_reachdevs.csv', group, group),
                   stringsAsFactors = F)
    
    
    avg <- aggregate(reachdeviation_deg ~ trial_num,
                     data = df,
                     FUN = mean,
                     na.rm = T)
    
    lines(avg,
          col = colors$op[group_no])
    
  }
  
  legend( x = 70,
          y = 12,
          legend = groups,
          col = colors$op[c(1:length(groups))],
          bty='n',
          lty=1,
          # title='groups:',
          bg='#FFFFFF')
  
  axis(side = 1,
       at = c(1,30,60,90))
  axis(side = 2,
       at = c(0,10,20,30))
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


plotFittedExponentials <- function(target='inline', trials=c(1:90), main=NULL) {
  
  setupFigureFile(target=target,
                  width = 8,
                  height=6,
                  dpi=300,
                  sprintf('doc/fig2b_exponentials.%s', target))
  
  groups <- c('control', 'cursorjump', 'handview')
  
  colors <- getColors()
  
  xrange = c(min(trials)-1,max(trials)+1)
  
  plot(NA, NA,
       main='', xlab='trial', ylab='reach deviation [°]',
       xlim=xrange, ylim=c(-10,40),
       ax=F, bty='n')
  
  if(!is.null(main)) {
    title(main=main, adj=0, cex.main=2)
  }
  lines(x=xrange,
        y=c(30,30),
        col='#999999',
        lw=2,
        lty=2)
  lines(x=xrange,
        y=c(0,0),
        col='#999999',
        lw=2,
        lty=2)
  
  
  timepoints <- seq(min(trials)-1,max(trials)-1,0.1)
  
  groups <- c('control', 'cursorjump', 'handview')
  
  # loop through groups
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    # load the bootstrapped exponential fits:
    expfits <- read.csv(sprintf('data/%s/%s_expfits.csv', group, group),
                        stringsAsFactors = FALSE)
    
    # calculate the 95% CI for lambda and N0
    lambdaCI <- quantile(expfits$lambda, c(0.025, 0.975))
    N0CI     <- quantile(expfits$N0, c(0.025, 0.975))
    
    
    N0 <- unname(N0CI[1])
    lambda <- unname(lambdaCI[1])
    par <- c('N0'=N0,
             'lambda'=lambda)
    fittedcurve <- exponentialModel(par=par,
                                    timepoints=timepoints)
    
    X <- timepoints + 1
    Y <- fittedcurve$output
    
    N0 <- unname(N0CI[2])
    lambda <- unname(lambdaCI[2])
    par <- c('N0'=N0,
             'lambda'=lambda)
    fittedcurve <- exponentialModel(par=par,
                                    timepoints=timepoints)
    
    X <- c(X , rev(timepoints)+1)
    Y <- c(Y , rev(fittedcurve$output))
    
    
    polygon( x = X,
             y = Y,
             border=NA,
             col = colors$tr[group_no])
    
  }
  
  
  # loop through groups
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    # load the bootstrapped exponential fits:
    expfits <- read.csv(sprintf('data/%s/%s_expfits.csv', group, group),
                        stringsAsFactors = FALSE)
    
    # calculate the 95% CI for lambda and N0
    lambdaCI <- quantile(expfits$lambda, c(0.025, 0.50, 0.975))
    N0CI     <- quantile(expfits$N0, c(0.025, 0.50, 0.975))
    
    N0 <- unname(N0CI[2])
    lambda <- unname(lambdaCI[2])
    
    par <- c('N0'=N0,
             'lambda'=lambda)
    
    fittedcurve <- exponentialModel(par=par,
                                    timepoints=timepoints)
    
      
    lines(x=timepoints+1,
          y=fittedcurve$output,
          col = colors$op[group_no])
    
  }
  
  legend( x = 70,
          y = 12,
          legend = groups,
          col = colors$op[c(1:length(groups))],
          bty='n',
          lty=1,
          # title='groups:',
          bg='#FFFFFF')
  
  axis(side = 1,
       at = c(1,seq(30,max(trials),30)))
  axis(side = 2,
       at = c(0,10,20,30))
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


plotAsymptotes <- function(target='inline', main=NULL) {
  
  setupFigureFile(target=target,
                  width = 3,
                  height=3,
                  dpi=300,
                  sprintf('doc/fig2c_asymptotes.%s', target))
  
  groups <- c('control', 'cursorjump', 'handview')
  
  colors <- getColors()
  
  plot(NA, NA,
       main='', xlab='', ylab='asymptote [°]',
       xlim=c(0.5,3.5), ylim=c(-10,40),
       ax=F, bty='n')
  
  if(!is.null(main)) {
    title(main=main, adj=0, cex.main=2)
  }
  
  lines(x=c(0,4),
        y=c(30,30),
        col='#999999',
        lw=2,
        lty=2)
  lines(x=c(0,4),
        y=c(0,0),
        col='#999999',
        lw=2,
        lty=2)
  
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    df <- read.csv(file = sprintf('data/%s/%s_participant_expfits.csv',group,group),
                   stringsAsFactors = FALSE)
    
    colop = colors$op[group_no]
    coltr = colors$tr[group_no]
    
    points(x=rep(group_no, length(df$participant)),
           y=df$N0,
           col=coltr,
           pch=16)
    
    
    CI <- Reach::getConfidenceInterval(data=df$N0)
    avg <- mean(df$N0)
    
    polygon( x = c(-.4,-.2,-.2,-.4)+group_no,
             y = rep(CI, each=2),
             border=NA,
             col=coltr)
    
    lines( x = c(-.4,-.2)+group_no,
           y = rep(avg,2),
           col=colop)
    
    grdens <- density(df$N0,
                      n=100,
                      from=5,
                      to=40)
    
    polygon( x = (3*c(grdens$y,0,0))+group_no+0.2,
             y = c(seq(from=5,to=40,length.out=100),40,5),
             border=NA,
             col=coltr)
    
    lines(x = (3*c(grdens$y))+group_no+0.2,
          y = seq(from=5,to=40,length.out=100),
          col=colop)
    
    
  }
  
  
  axis(side = 1,
       at = c(1,2,3),
       labels = c('control','cursor\njump','hand\nview'),
       las=2)
  axis(side = 2,
       at = c(0,10,20,30))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  
}


plotLearningRates <- function(target='inline', main=NULL) {
  
  setupFigureFile(target=target,
                  width = 3,
                  height=3,
                  dpi=300,
                  sprintf('doc/fig2c_learningrates.%s', target))
  
  groups <- c('control', 'cursorjump', 'handview')
  
  colors <- getColors()
  
  plot(NA, NA,
       main='', xlab='', ylab='learning rate [% asymptote/trial]',
       xlim=c(0.5,3.5), ylim=c(-1/3,4/3),
       ax=F, bty='n')
  
  if(!is.null(main)) {
    title(main=main, adj=0, cex.main=2)
  }
  
  lines(x=c(0.5,3.5),
        y=c(1,1),
        col='#999999',
        lw=2,
        lty=2)
  lines(x=c(0.5,3.5),
        y=c(0,0),
        col='#999999',
        lw=2,
        lty=2)
  
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    df <- read.csv(file = sprintf('data/%s/%s_participant_expfits.csv',group,group),
                   stringsAsFactors = FALSE)
    
    colop = colors$op[group_no]
    coltr = colors$tr[group_no]
    
    points(x=rep(group_no, length(df$participant)),
           y=df$lambda,
           col=coltr,
           pch=16)
    
    
    CI <- Reach::getConfidenceInterval(data=df$lambda)
    avg <- mean(df$lambda)
    
    polygon( x = c(-.4,-.2,-.2,-.4)+group_no,
             y = rep(CI, each=2),
             border=NA,
             col=coltr)
    
    lines( x = c(-.4,-.2)+group_no,
           y = rep(avg,2),
           col=colop)
    
    grdens <- density(df$lambda,
                      n=100,
                      from=-0.1,
                      to=1.1)
    
    polygon( x = (0.3*c(grdens$y,0,0))+group_no+0.2,
             y = c(seq(from=-0.1,to=1.1,length.out=100),1.1,-0.1),
             border=NA,
             col=coltr)
    
    lines(x = (0.3*c(grdens$y))+group_no+0.2,
          y = seq(from=-0.1,to=1.1,length.out=100),
          col=colop)
    
    
  }
  
  
  axis(side = 1,
       at = c(1,2,3),
       labels = c('control','cursor\njump','hand\nview'),
       las=2)
  axis(side = 2,
       at = c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  
}


fig2_learning <- function(target='inline') {
  
  setupFigureFile(target = target,
                  filename = sprintf('doc/fig2_training.%s', target),
                  width=8, height=6, dpi=300
                  )
  
  layout( mat = matrix(data = c(1,1,1,2,3,4),
                       ncol = 3,
                       nrow = 2,
                       byrow = TRUE),
          width=c(0.4,.3,.3))
  
  par(mar=c(4,4,2.3,0.1))
  
  plotTraining(target='inline', main='A')
  
  # par(mar=c(5,4,2.3,0.1))
  
  plotFittedExponentials(target='inline', trials=c(1:30), main='B')
  
  plotAsymptotes(target='inline', main='C')
  
  plotLearningRates(target='inline', main='D')
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}



# nocursor plots -----

plotNoCursors <- function(target='inline') {
  
  setupFigureFile(target=target,
                  width = 8,
                  height=6,
                  dpi=300,
                  sprintf('doc/fig3_nocursors.%s', target))
  
  groups <- c('control', 'cursorjump', 'handview')
  
  colors <- getColors()
  
  df <- getNoCursorData()
  
  
  plot(NA, NA,
       main='no cursors', xlab='strategy', ylab='reach deviation [°]',
       xlim=c(0,3), ylim=c(-10,40),
       ax=F, bty='n')
  
  lines(x=c(0,3),
        y=c(30,30),
        col='#999999',
        lw=2,
        lty=2)
  lines(x=c(0,3),
        y=c(0,0),
        col='#999999',
        lw=2,
        lty=2)
  
  for (group_no in c(1:length(groups))) {
    
    group <- groups[group_no]
    
    gdf <- df[df$group == group,]
    
    CI <- aggregate(reachdeviation_deg ~ strategy,
                     data = gdf,
                     FUN = Reach::getConfidenceInterval)
    
    lo <- CI$reachdeviation_deg[,1]
    hi <- CI$reachdeviation_deg[,2]
    
    polygon( x = c( CI$strategy, rev(CI$strategy))+1,
             y = c( lo, rev(hi)),
             border=NA,
             col = colors$tr[group_no]) 
    
    avg <- aggregate(reachdeviation_deg ~ strategy,
                     data = gdf,
                     FUN = mean,
                     na.rm = T)
    # print(avg)
    lines(x=c(1,2), avg$reachdeviation_deg, 
          col = colors$op[group_no])
    
    
    
    without <- gdf$reachdeviation_deg[which(gdf$strategy == 0)]
    with <- gdf$reachdeviation_deg[which(gdf$strategy == 1)]
    
    points(x = rep(group_no/4, length(without)),
           y = without,
           pch=16,
           cex=1.5,
           col = colors$tr[group_no])
    
    points(x = 2+rep(group_no/4, length(with)),
           y = with,
           pch=16,
           cex=1.5,
           col = colors$tr[group_no])

  }
  
  axis(side = 1,
       at = c(1,2),
       labels=c('without','with'))
  
  axis(side = 2,
       at = c(0,15,30))
  
  
  legend( x = 0,
          y = 45,
          legend = groups,
          col = colors$op[c(1:length(groups))],
          bty='n',
          lty=1,
          # title='groups:',
          bg='#FFFFFF',
          xpd=TRUE)
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}
