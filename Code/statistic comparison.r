
statistic.comparison = function(statistic.1,statistic.2,transformation){

require(emdist)

if (transformation == 1) statistic.2 <- statistic.2
if (transformation == 2) statistic.2 <- statistic.2[c(7,8,1,2,3,4,5,6)]
if (transformation == 3) statistic.2 <- statistic.2[c(5,6,7,8,1,2,3,4)]
if (transformation == 4) statistic.2 <- statistic.2[c(3,4,5,6,7,8,1,2)]
if (transformation == 5) statistic.2 <- statistic.2[c(1,8,7,6,5,4,3,2)]
if (transformation == 6) statistic.2 <- statistic.2[c(3,2,1,8,7,6,5,4)]
if (transformation == 7) statistic.2 <- statistic.2[c(5,4,3,2,1,8,7,6)]
if (transformation == 8) statistic.2 <- statistic.2[c(7,6,5,4,3,2,1,8)]

angles <- seq(0,7*pi/4,by=pi/4)

horizontal.locations <- cos(angles)
vertical.locations <- sin(angles)

horizontal.locations.1 <- statistic.1*horizontal.locations
vertical.locations.1 <- statistic.1*vertical.locations

horizontal.locations.2 <- statistic.2*horizontal.locations
vertical.locations.2 <- statistic.2*vertical.locations

A <- matrix(c(as.vector(horizontal.locations.1),as.vector(vertical.locations.1)),ncol=2)
wA <- rep(1,8)

B <- matrix(c(as.vector(horizontal.locations.2),as.vector(vertical.locations.2)),ncol=2)
wB <- rep(1,8)

return(emdw(A,wA,B,wB)*8)

}