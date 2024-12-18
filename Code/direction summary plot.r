
direction.summary.plot = function(object.location,start.sample,end.sample,start.time,end.time,subregion.width,subregion.height){

load(paste(object.location,"/image data/image data.RData",sep=""))

image.width <- dim(image.data)[3]
image.height <- dim(image.data)[4]

rm(image.data)

subregions.wide <- floor(image.width/subregion.width)
subregions.high <- floor(image.height/subregion.height)

number.of.subregions <- subregions.wide*subregions.high

samples <- start.sample:end.sample
times <- start.time:end.time

collated.statistics <- array(NA,dim=c(length(samples),length(times),number.of.subregions,8))

for (i in 1:length(samples)){

	for (j in 1:length(times)){
	
		load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",samples[i],"/summary statistics ",times[j],".RData",sep=""))
	
		collated.statistics[i,j,,] <- summary.statistics
		
	}
	
}

maximum.statistic <- max(as.vector(collated.statistics))

maximum.density <- 0

for (i in 1:number.of.subregions){

	for (j in 1:8){
	
		maximum.density <- max(maximum.density,density(collated.statistics[,,i,j],bw="SJ")$y)
		
	}
	
}

subregion.locations <- matrix(1:number.of.subregions,nrow=subregions.wide,ncol=subregions.high)
subregion.locations <- subregion.locations[,subregions.high:1]

subregion.ordering <- as.vector(subregion.locations)

pdf(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/direction summary ",start.sample," ",end.sample," ",start.time," ",end.time,".pdf",sep=""),height=15,width=15)

par(mfrow=c(subregions.high,subregions.wide))

for (i in subregion.ordering){

	plot(density(collated.statistics[,,i,1],bw="SJ"),xlim=c(0,maximum.statistic),ylim=c(0,maximum.density),col=1,main="")
	
	if (i == 1)	legend("topright",legend=1:8,col=1:8,lty=1)
	
	for (j in 2:8) lines(density(collated.statistics[,,i,j],bw="SJ"),col=j)
	
}

dev.off()

}