
summary.plot = function(object.location,sample.number,start.time,end.time,subregion.width,subregion.height){

load(paste(object.location,"/image data/image data.RData",sep=""))

image.width <- dim(image.data)[3]
image.height <- dim(image.data)[4]

rm(image.data)

subregions.wide <- floor(image.width/subregion.width)
subregions.high <- floor(image.height/subregion.height)

number.of.subregions <- subregions.wide*subregions.high

horizontal.centres <- rep(1:subregions.wide,subregions.high)
vertical.centres <- rep(1:subregions.high,each=subregions.wide)

maximum.statistic.value <- 0

for (i in start.time:end.time){

	load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/summary statistics ",i,".RData",sep=""))
	
	maximum.statistic.value <- max(maximum.statistic.value,as.vector(summary.statistics))
	
}

angles <- seq(0,7*pi/4,by=pi/4)
horizontal.line.directions <- cos(angles)
vertical.line.directions <- sin(angles)

pdf(file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/statistic plot ",start.time," ",end.time,".pdf",sep=""),width=7,height=7)

for (i in start.time:end.time){

	plot(horizontal.centres,vertical.centres,type="p",xlim=c(0,subregions.wide+1),ylim=c(0,subregions.high+1),pch=".")
	
	load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/summary statistics ",i,".RData",sep=""))
	
	summary.statistics <- summary.statistics/maximum.statistic.value/2
	
	for (j in 1:number.of.subregions){
	
		segments(x0=rep(horizontal.centres[j],8),y0=rep(vertical.centres[j],8),x1=horizontal.centres[j]+summary.statistics[j,]*horizontal.line.directions,y1=vertical.centres[j]+summary.statistics[j,]*vertical.line.directions)
		
	}
	
}

dev.off()

}