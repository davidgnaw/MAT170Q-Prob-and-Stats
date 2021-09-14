craps= function() {
  dice= c(1:6)
  first.roll= sum(sample(dice, size= 2, replace= TRUE))
  if(any(c(7,11))==first.roll()) {
    print("win")
  } else if(any(c(2,3,12))== first.roll()) {
    print("lose")
  } else 
    second.roll= sum(sample(dice,size = 2, replace=TRUE))
    while(TRUE)
    n= second.roll()
    x= first.roll()
    if (n=x) {
      return("win")
    } 
    if (n=7) 
    return("lose")
}