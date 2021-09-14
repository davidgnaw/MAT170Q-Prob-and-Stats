library(plotrix)

simulated.sample.proportions = function(x,n,p) {
  return(rbinom(x,n,p)/n)
}

margin.of.error = function(p.hat,n,conf.level) {
  half.alpha = (1-conf.level)/2
   z.half.alpha = abs(qnorm(half.alpha))
   standard.error = sqrt((p.hat*(1-p.hat))/n)
   return(z.half.alpha*standard.error)
}

simulated.confidence.intervals = function(x, n, p, conf.level) {
  p.hats = simulated.sample.proportions(x, n, p)
  margins.of.error = margin.of.error(p.hats, n, conf.level)
  vector = (mean(phat) > (p.hat + margins.of.error) | mean(p.hat) < (p.hat - margins.of.error))
  plotCI(p.hats,
         y = NULL,
         uiw = margins.of.error,
         err = "y",
  main= "Simulated Confidence Intervals",
  xlab= "study #",
  ylab= "proportion",
  col = ifelse(vector,"red","black")
  ) 
  abline(h = p, col="blue", lty=2)
}

simulated.confidence.intervals(20,200,0.25,0.95)