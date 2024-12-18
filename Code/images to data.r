
images.to.data = function(object.location){

require(EBImage)

number.of.time.points <- length(list.files(paste(object.location,"/tiffs/1",sep="")))

image.names <- seq(100,l=number.of.time.points,by=1)

number.of.samples <- length(list.files(paste(object.location,"/tiffs",sep="")))

holding.image <- readImage(paste(object.location,"/tiffs/1/",image.names[1],".tif",sep=""))
holding.data <- imageData(holding.image)

image.width <- dim(holding.data)[1]
image.height <- dim(holding.data)[2]

image.data <- array(NA,dim=c(number.of.samples,number.of.time.points,image.width,image.height))

for (i in 1:length(image.names)){

	for (j in 1:number.of.samples){
	
		image.data[j,i,,] <- round(imageData(readImage(paste(object.location,"/tiffs/",j,"/",image.names[i],".tif",sep="")))[,image.height:1]*2^16)
		
	}
	
}

dir.create(paste(object.location,"/image data",sep=""),showWarnings=FALSE)

save(image.data,file=paste(object.location,"/image data/image data.RData",sep=""))

}