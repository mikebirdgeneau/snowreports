get.rad<-function()
{
  print("Woo!")
}

get.compass.heading<-function(degrees)
{
  ifelse(degrees<=22.5,"N",
         ifelse(degrees<67.5,"NE",
                ifelse(degrees<112.5,"E",
                       ifelse(degrees<=157.5,"SE", 
                              ifelse(degrees<=202.5,"S",
                                     ifelse(degrees<247.5,"SW",
                                            ifelse(degrees<=292.5,"W",
                                                   ifelse(degrees<=337.5,"NW","N"))))))))
}