
permutation.calculations = function(object.location,sample.1,sample.2,time.point,subregion.width,subregion.height,hypothesis,number.of.permutations){

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",hypothesis,".RData",sep=""))

number.of.subregions <- dim(allowable.comparisons)[1]

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.1,"/summary statistics ",time.point,".RData",sep=""))
summary.statistics.1 <- summary.statistics

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.2,"/summary statistics ",time.point,".RData",sep=""))
summary.statistics.2 <- summary.statistics

statistic.comparisons <- array(NA,dim=c(number.of.subregions,number.of.subregions,8))
permutation.comparisons <- array(NA,dim=c(number.of.permutations,number.of.subregions,3))
permutation.statistics <- rep(NA,number.of.permutations)

for (i in 1:number.of.permutations){

	if (i == 1){
	
		current.comparisons <- matrix(c(1:number.of.subregions,1:number.of.subregions,rep(1,number.of.subregions)),ncol=3)
		
		for (j in 1:number.of.subregions){
		
			statistic.comparisons[current.comparisons[j,,drop=FALSE]] <- statistic.comparison(summary.statistics.1[current.comparisons[j,1],],summary.statistics.2[current.comparisons[j,2],],current.comparisons[j,3])
		
		}
		
		permutation.comparisons[i,,] <- current.comparisons
		permutation.statistics[i] <- sum(statistic.comparisons[current.comparisons])
		
	} else{
	
		current.comparisons <- matrix(c(sample(1:number.of.subregions,number.of.subregions),rep(NA,number.of.subregions*2)),ncol=3)
		current.allowable.comparisons <- allowable.comparisons
		
		for (j in 1:number.of.subregions){
		
			if (length(which(current.allowable.comparisons[current.comparisons[j,1],,]==1))>1){
		
				matching.pair <- sample(which(current.allowable.comparisons[current.comparisons[j,1],,]==1),1)
				
			} else{
			
				matching.pair <- which(current.allowable.comparisons[current.comparisons[j,1],,]==1)
				
			}
			
			current.comparisons[j,2] <- matching.pair%%number.of.subregions
			if (current.comparisons[j,2]==0) current.comparisons[j,2] <- number.of.subregions
			current.comparisons[j,3] <- ceiling(matching.pair/number.of.subregions)
			
			if (is.na(statistic.comparisons[current.comparisons[j,,drop=FALSE]]) == TRUE) statistic.comparisons[current.comparisons[j,,drop=FALSE]] <- statistic.comparison(summary.statistics.1[current.comparisons[j,1],],summary.statistics.2[current.comparisons[j,2],],current.comparisons[j,3])
			
			current.allowable.comparisons[current.comparisons[j,1],,] <- 0
			current.allowable.comparisons[,current.comparisons[j,2],] <- 0
			
		}
		
		permutation.comparisons[i,,] <- current.comparisons
		permutation.statistics[i] <- sum(statistic.comparisons[current.comparisons])
		
	}
	
}

permutation.data <- list(permutation.statistics=permutation.statistics,permutation.comparisons=permutation.comparisons,statistic.comparisons=statistic.comparisons)

dir.create(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/comparisons/",hypothesis,sep=""),recursive=TRUE,showWarnings=FALSE)

save(permutation.data,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/comparisons/",hypothesis,"/permutation data ",sample.1," ",sample.2," ",time.point,".RData",sep=""))

}