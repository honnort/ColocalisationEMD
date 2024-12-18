
subregion.specification = function(object.location,subregion.width,subregion.height){

load(paste(object.location,"/image data/image data.RData",sep=""))

object.dimensions <- dim(image.data)

rm(image.data)

image.width <- object.dimensions[3]
image.height <- object.dimensions[4]

subregions.wide <- floor(image.width/subregion.width)
subregions.high <- floor(image.height/subregion.height)

number.of.subregions <- subregions.wide*subregions.high

left.delimeters <- seq(from=1,by=subregion.width,l=subregions.wide)
right.delimeters <- seq(from=subregion.width,by=subregion.width,l=subregions.wide)

bottom.delimeters <- seq(from=1,by=subregion.height,l=subregions.high)
top.delimeters <- seq(from=subregion.height,by=subregion.height,l=subregions.high)

lefts <- matrix(left.delimeters,nrow=subregions.wide,ncol=subregions.high)
rights <- matrix(right.delimeters,nrow=subregions.wide,ncol=subregions.high)

bottoms <- matrix(bottom.delimeters,byrow=TRUE,nrow=subregions.wide,ncol=subregions.high)
tops <- matrix(top.delimeters,byrow=TRUE,nrow=subregions.wide,ncol=subregions.high)

subregion.limits <- matrix(NA,ncol=4,nrow=number.of.subregions)

subregion.limits[,1] <- as.vector(lefts)
subregion.limits[,2] <- as.vector(rights)
subregion.limits[,3] <- as.vector(bottoms)
subregion.limits[,4] <- as.vector(tops)

dir.create(paste(object.location,"/flow data ",subregion.width," ",subregion.height,sep=""),showWarnings=FALSE)

save(subregion.limits,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/subregion limits.RData",sep=""))

}