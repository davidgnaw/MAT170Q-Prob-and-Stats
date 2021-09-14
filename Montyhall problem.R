#Yu Fung David Wang
#Monty Hall Problem Simulation

# not switching doors 

original.door = function() {
  doors = c("G","G","C")
  pick.door = sample(doors,size = 1,replace = FALSE)
  return(ifelse(pick.door == "G",FALSE,TRUE))
}

# switch doors after knowing one door that has a goat

switch.door = function() {
  doors = c("G","G","C")
  car = "C"
  car.location= sample(doors,size = 1) == "C"
  original.door = sample(doors,size = 1)
  mc.door = sample((doors="G"),size = 1)
  new.door = ifelse((original.door != car & mc.door != car),TRUE,FALSE)
  return(car.location = new.door)
}

# number of cars won in 3000 trials through not switching doors

cars.won = function(n) {
  return(sum(replicate(n,original.door())))
}

cars.won(3000)

#number of cars won in 3000 trials through switching doors

cars.won = function(n) {
  return(sum(replicate(n,switch.door())))
}
  
cars.won(3000)

#Conclusion- When playing this game of chance, always switch doors when monty hall asks since Marilyn is correct. By switching doors, it produces a 2/3 chance while staying with the initial door only has a 1/3 chance of winning the car.