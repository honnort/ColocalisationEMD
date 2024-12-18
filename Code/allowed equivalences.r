
allowed.equivalences = function(object.location,subregion.width,subregion.height,hypothesis){

load(paste(object.location,"/image data/image data.RData",sep=""))

image.width <- dim(image.data)[3]
image.height <- dim(image.data)[4]

rm(image.data)

subregions.wide <- floor(image.width/subregion.width)
subregions.high <- floor(image.height/subregion.height)

number.of.subregions <- subregions.wide*subregions.high

#1 rotate 0  ++ do nothing
#2 rotate 90  ++ transpose then rearrange rows
#3 rotate 180  ++ rearrange rows then rearrange columns
#4 rotate 270  ++ transpose then rearrange columns
#5 flip vertically  ++ rearrange columns
#6 flip vertically, rotate 90 == flip across (1,1)  ++ transpose
#7 flip vertically, rotate 180 == flip horizontally  ++ rearrange rows
#8 flip vertically, rotate 270 == flip across (-1,1)  ++ transpose then rearrange rows then rearrange columns

# initial.arrangement <- matrix(1:number.of.subregions,nrow=subregions.wide,ncol=subregions.high)
# arrangement.1 <- as.vector(initial.arrangement)
# arrangement.2 <- as.vector(t(initial.arrangement)[subregions.wide:1,])
# arrangement.3 <- as.vector(initial.arrangement[subregions.wide:1,][,subregions.high:1])
# arrangement.4 <- as.vector(t(initial.arrangement)[,subregions.high:1])
# arrangement.5 <- as.vector(initial.arrangement[,subregions.high:1])
# arrangement.6 <- as.vector(t(initial.arrangement))
# arrangement.7 <- as.vector(initial.arrangement[subregions.wide:1,])
# arrangement.8 <- as.vector(t(initial.arrangement)[subregions.wide:1,][,subregions.high:1])

allowable.comparisons <- array(0,dim=c(number.of.subregions,number.of.subregions,8))
#allowable.comparions[a,b,c]==1 means that a can be compared to b after b has been transformed according to c

if (hypothesis == "isotropic"){

	allowable.comparisons <- array(1,dim=c(number.of.subregions,number.of.subregions,8))

}

if (hypothesis == "homogeneous"){

	allowable.comparisons[,,1] <- 1

}

if (hypothesis == "symmetric"){

	initial.arrangement <- matrix(1:number.of.subregions,nrow=subregions.wide,ncol=subregions.high)
	arrangement.1 <- as.vector(initial.arrangement)
	arrangement.2 <- as.vector(t(initial.arrangement)[subregions.wide:1,])
	arrangement.3 <- as.vector(initial.arrangement[subregions.wide:1,][,subregions.high:1])
	arrangement.4 <- as.vector(t(initial.arrangement)[,subregions.high:1])
	arrangement.5 <- as.vector(initial.arrangement[,subregions.high:1])
	arrangement.6 <- as.vector(t(initial.arrangement))
	arrangement.7 <- as.vector(initial.arrangement[subregions.wide:1,])
	arrangement.8 <- as.vector(t(initial.arrangement)[subregions.wide:1,][,subregions.high:1])

	allowable.comparisons[matrix(c(arrangement.1,arrangement.1,rep(1,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.2,rep(2,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.3,rep(3,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.4,rep(4,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.5,rep(5,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.6,rep(6,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.7,rep(7,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.8,rep(8,number.of.subregions)),ncol=3)] <- 1

}

if (hypothesis == "vertical"){

	initial.arrangement <- matrix(1:number.of.subregions,nrow=subregions.wide,ncol=subregions.high)
	arrangement.1 <- as.vector(initial.arrangement)
	arrangement.5 <- as.vector(initial.arrangement[,subregions.high:1])

	allowable.comparisons[matrix(c(arrangement.1,arrangement.1,rep(1,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.5,rep(5,number.of.subregions)),ncol=3)] <- 1

}

if (hypothesis == "horizontal"){

	initial.arrangement <- matrix(1:number.of.subregions,nrow=subregions.wide,ncol=subregions.high)
	arrangement.1 <- as.vector(initial.arrangement)
	arrangement.7 <- as.vector(initial.arrangement[subregions.wide:1,])

	allowable.comparisons[matrix(c(arrangement.1,arrangement.1,rep(1,number.of.subregions)),ncol=3)] <- 1
	allowable.comparisons[matrix(c(arrangement.1,arrangement.7,rep(7,number.of.subregions)),ncol=3)] <- 1

}

save(allowable.comparisons,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",hypothesis,".RData",sep=""))

}