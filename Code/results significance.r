
results.significance = function(object.location,start.sample,end.sample,start.time,end.time,subregion.width,subregion.height,hypothesis){

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/comparisons/",hypothesis,"/permutation data ",start.sample," ",start.sample+1," ",start.time,".RData",sep=""))

number.of.samples <- end.sample-start.sample+1

sample.matrix.1 <- matrix(c(start.sample:end.sample),nrow=number.of.samples,ncol=number.of.samples)
sample.matrix.2 <- matrix(c(start.sample:end.sample),nrow=number.of.samples,ncol=number.of.samples,byrow=TRUE)
pairings <- matrix(c(as.vector(sample.matrix.1[upper.tri(sample.matrix.1)]),as.vector(sample.matrix.2[upper.tri(sample.matrix.2)])),ncol=2)

number.of.pairings <- dim(pairings)[1]

number.of.permutations <- length(permutation.data$permutation.statistics)

times <- start.time:end.time

number.of.time.points <- end.time-start.time+1

all.statistics <- array(NA,dim=c(number.of.permutations,number.of.pairings,number.of.time.points))
all.significances <- array(NA,dim=c(number.of.pairings,number.of.time.points))

for (i in 1:number.of.pairings){

	for (j in 1:number.of.time.points){
	
		load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/comparisons/",hypothesis,"/permutation data ",pairings[i,1]," ",pairings[i,2]," ",j,".RData",sep=""))

		all.statistics[,i,j] <- permutation.data$permutation.statistics
		
		all.significances[i,j] <- sum(permutation.data$permutation.statistics<=permutation.data$permutation.statistics[1])/number.of.permutations
		
	}
	
}

significant.proportions <- rep(NA,number.of.time.points)
ks.significant <- rep(NA,number.of.time.points)

for (i in 1:number.of.time.points){

	significant.proportions[i] <- sum(all.significances[,i]<=0.05)/number.of.pairings
	
	ks.significant[i] <- ks.test(all.significances[,i],punif)$p.value<0.05
	
}

significance.data <- list(all.statistics=all.statistics,all.significances=all.significances,significant.proportions=significant.proportions,ks.significant=ks.significant)

save(significance.data,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/comparisons/",hypothesis," summary.RData",sep=""))

}