sample.size= 30
population.mean= 50
sample.sd= 5
population.sd= 5/(sqrt(sample.size))
x = rnorm(sample.size,population.mean,population.sd)

rounded.data= round(x,digits=1)
mean(rounded.data)
sd(rounded.data)
z.scores=qnorm((1:4)*.20,0,1)
x.values= (z.scores*sd(rounded.data))+mean(rounded.data)
x.values

band.1 = rounded.data[rounded.data > 0 & rounded.data < min(x.values)]
band.2 = rounded.data[rounded.data > min(x.values) & rounded.data < quantile(x.values, 0.4)]
band.3 = rounded.data[rounded.data > quantile(x.values, 0.4) & rounded.data < quantile(x.values, 0.6)]
band.4 = rounded.data[rounded.data > quantile(x.values, 0.6) & rounded.data < quantile(x.values, 0.8)]
band.5 = rounded.data[rounded.data > quantile(x.values, 0.8) & rounded.data < max(x.values)]


#observed values
num.quantiles = function(n) {
  quantiles = seq(from = 0, to = n/n, by = 1/n)
  cutoff.x.values = quantile(rounded.data, quantiles)
  return(hist(rounded.data, breaks = cutoff.x.values, plot= FALSE)$counts)
}


#expected values
expected.band.counts = length(rounded.data)/num.quantiles 

expected = function(n) {
  expected.band.counts = length(rounded.data)/n
  return(expected.band.counts)
}
  

normality.test = function(data, num.quantiles, alpha) {
  num.quantiles = n
    quantiles = seq(from = 0, to = n/n, by = 1/n)
    cutoff.x.values = quantile(rounded.data, quantiles)
    observed.band.counts = hist(rounded.data, breaks = cutoff.x.values, plot= FALSE)$counts
    expected.band.counts = length(rounded.data)
    
  }





 
