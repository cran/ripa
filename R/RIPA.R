##################################################################################################################
####################################### Aviris functions #########################################################
##################################################################################################################

		       ##############
		       #            #
         		 #            #
	     #########    ####    ############
	     #       #    #   #   #          #
	################  ####    #   2006   #
	#              #  #  #    #          #
	#    AVIRIS    #  #   #   ############
	#              # .#   #   #
	################          #
	             #            #
	             ##############

########################################################################
###                                                                  ###
###       Marcelo G. Almiron		        Adrian E. Muract     ###
###                                                                  ###
###   <almiron.marcelo@gmail.com>     <amuract@dc.exa.unrc.edu.ar>   ###
###                                                                  ###
########################################################################






	#########################
######### Class aviris_band ############################################################
	#########################



setClass("aviris_band",representation(scene="character",band="numeric",type="character",
	numberOfLines="numeric",samples="numeric",data="matrix",min="numeric",
	max="numeric",mean="numeric",sd="numeric"))



setMethod("initialize",
	"aviris_band",
	function(.Object,scene=character(),band=numeric(),type=character(),
	numberOfLines="numeric()",samples=numeric(),data=matrix()){
		.Object@scene=scene
		.Object@band=band
		.Object@type=type
		.Object@numberOfLines=numberOfLines
		.Object@samples=samples
		.Object@data=data
		.Object@min=min(data)
		.Object@max=max(data)
		.Object@mean=mean(data)
		.Object@sd=sd(as.vector(data))
		.Object
	})



print_information.aviris_band <- function(Object){
	cat("  Scene: ",Object@scene,"\n")
	cat("  Band : ",Object@band,"\n")
	cat("  Number of lines: ",Object@numberOfLines,"\n")
	cat("  Number of samples: ",Object@samples,"\n")		
	cat("  Minimum: ",Object@min,"\n")
	cat("  Maximum: ",Object@max,"\n")
	cat("  Mean: ",Object@mean,"\n")
	cat("  Standard desviation:",Object@sd,"\n")
}



# Interfaz para la Carga de una banda #
#######################################

lband <- function(scene,b){
	path <- paste(scene@path,scene@name,sep="/")
	Object <- new("aviris_band",scene@name,b,scene@type,scene@numberOfLines,
		scene@samples,loadBand(path,X=b,C=scene@samples,F=scene@numberOfLines,
		B=scene@bands))
}

lbandsample <- function(scene,b){
	path <- paste(scene@path,scene@name,sep="/")
	Object <- new("aviris_band",scene@name,b,scene@type,scene@numberOfLines,
		scene@samples,loadBandSample(path,X=b,C=scene@samples,F=30,
		B=scene@bands))
}


# Interfaz para Escribir en una banda #
#######################################

wband <- function(scene,band){
	path <- paste(scene@path,scene@name,sep="/")
	writeBand(path,band@data,band@band,band@samples,band@numberOfLines,scene@bands)
}




# Algor�mo de Carga de una banda #
###################################

loadBand <- function(I,X=5,C=614,F=512,B=224){
    f <- 0
    conexionParaLeer <- file(I,open="r+b")   
    Z <- matrix(nrow=F,ncol=C)
    while (f!=F){
        c <- 0
        while (c!=C){
            seek(conexionParaLeer,where=(((B*(f*C+c))+(X-1))*2),rw="read")
	    Z[f+1,c+1] <- readBin(conexionParaLeer,"integer",size=2,endian="swap")
            c <- c+1
        }
        f <- f+1
    }
    close(conexionParaLeer,rw="read")
    Z
}

loadBandSample <- function(I,X=5,C=614,F=30,B=224){
    f <- 0
    conexionParaLeer <- file(I,open="r+b")   
    Z <- matrix(nrow=F,ncol=30)
    while (f!=F){
        c <- 0
        while (c!=30){
            seek(conexionParaLeer,where=(((B*(f*C+c))+(X-1))*2),rw="read")
	    Z[f+1,c+1] <- readBin(conexionParaLeer,"integer",size=2,endian="swap")
            c <- c+1
        }
	f <- f+1
    }
    close(conexionParaLeer,rw="read")
    Z
}

# Algor�mo de Escritura en una banda #
#######################################

writeBand <- function(I,Z,X=NA,C=614,F=512,B=224){
    f <- 0
    conexionParaEscribir <- file(I,open="r+b")
    while (f!=F){
        c <- 0
        while (c!=C){
            seek(conexionParaEscribir,where=(((B*(f*C+c))+(X-1))*2),rw="write") 
            writeBin(as.integer(Z[f+1,c+1]),conexionParaEscribir,size=2,endian="swap")
            c <- c+1
        }
        f <- f+1
    }
    close(conexionParaEscribir,rw="write")
}




# Interfaz de Contrastes #
##########################

contrast <- function(band,type=c("gauss","lineal"),...){
	if(type=="gauss"){
		c <- new("aviris_band",band@scene,band@band,band@type,
			band@numberOfLines,band@samples,cgauss(band@data))	
	}else{
		c <- new("aviris_band",band@scene,band@band,band@type,
			band@numberOfLines,band@samples,clineal(band@data,...))
	}
}




# Algor�mo de Expansion Lineal de Contraste #
##############################################

clineal <- function(Z,A,B){
	if (max(Z)==0) return(Z)
	minZ <- min(Z)
	rango <- (B - A)
	rangoZ <- (max(Z)-minZ)
	pendiente <- (rango/rangoZ)
	pendiente*Z+(A-(pendiente*minZ))
}




# Algor�mo de Expansion Gausseano de Contraste #
#################################################

cgauss <- function(Z){
	s <- sd(as.vector(Z))
	x <- mean(Z)
	dnorm(Z,mean=x,sd=s)
}




# Interfaz de Visualizacion #
#############################

plot_band.aviris_band <- function (R=NULL,G=NULL,B=NULL,type=NULL,x0=1,y0=1,...){
	if (is.null(R) && is.null(G) && is.null(B)){ 
      	stop("Missing objects")
	}else if (class(R)[1]=="aviris_band" && is.null(G) && is.null(B)){ 
      	type <- "grey"
	}else if (class(R)[1]=="aviris_band" && class(G)[1]=="aviris_band" && class(B)[1]=="aviris_band"){
      	type <- "rgb"
	}else{ 
		stop("Incorrect parameters")
	}
	if (type == "rgb"){
		RGB(R,G,B,x0,y0,...)
	}else{
		Grey(R,x0,y0,...)	
	}
}




# Interfaz de Visualizacion en escala de Grises #
#################################################

Grey <- function(band,x0,y0,...){
	w <- 6.39
	h <- 5.33
	sw <- band@samples/96
	sh <- band@numberOfLines/96
	if((w-sw)<(h-sh)){
		c <- w/sw
	}else{
		c <- h/sh
	}
	x11(width=sw*c,height=sh*c)
	opar <- par(no.readonly=TRUE)
	opar$usr <- c(x0,x0+band@samples-1,y0+band@numberOfLines-1,y0)
	image(1:band@samples,1:band@numberOfLines,t(band@data[band@numberOfLines:1,])     
		,col=gray(0:255/256),xlab="Samples",ylab="Lines",axes=FALSE,...)
	par(cex=0.7)
	title(paste(paste("Scene",band@scene,sep=": "),paste("Band",band@band,sep=": "),sep=" - "))
	par(opar)
}




# Interfaz de Visualizacion en RGB #
####################################

RGB <- function(red,green,blue,x0,y0,...){
	rc <- clineal(red@data,0,1)
	gc <- clineal(green@data,0,1)
	bc <- clineal(blue@data,0,1)
	colvec <- rgb(round(rc,7),round(gc,7),round(bc,7))
    	colors <- unique(colvec)
    	colmat <- array(match(colvec,colors), dim = dim(red@data)[1:2])
	x11(width=6.39,height=5.33)
	opar <- par(no.readonly=TRUE)
	opar$usr <- c(x0,x0+dim(colmat)[2],y0+dim(colmat)[1],y0)
	image(1:dim(colmat)[2],1:dim(colmat)[1],t(colmat[nrow(colmat):1,])     
		,col=colors,xlab="Samples",ylab="Lines",axes=FALSE,...)
	r <- paste("R: Band",red@band,sep=" ")
	g <- paste("G: Band",green@band,sep=" ")
	b <- paste("B: Band",blue@band,sep=" ")
	subt <- paste(r,g,sep=" , ")
	t <- paste(subt,b,sep=" , ")
	par(cex=0.7)
	title(paste(paste("Scene",red@scene,sep=" "),t,sep=" - "))
	par(opar)
}



# ROIs #
########

zoom <- function (R=NULL,G=NULL,B=NULL){
	if (is.null(R) && is.null(G) && is.null(B)){ 
      	stop("Missing objects")
	}else if (class(R)[1]=="aviris_band" && is.null(G) && is.null(B)){ 
      	type <- "grey"
	}else if (class(R)[1]=="aviris_band" && class(G)[1]=="aviris_band" && class(B)[1]=="aviris_band"){
      	type <- "rgb"
	}else{ 
		stop("Incorrect parameters")
	}
	if (type == "rgb"){
		zoomRGB(R,G,B)
	}else{
		zoomGrey(R)	
	}
}



zoomGrey <- function(band){
	pos <- locator(2)
	if(pos$x[1]>pos$x[2]){
		aux <- pos$x[1]
		pos$x[1] <- pos$x[2]
		pos$x[2] <- aux  
	}
	if(pos$y[1]>pos$y[2]){
		auy <- pos$y[1]
		pos$y[1] <- pos$y[2]
		pos$y[2] <- auy  
	}
	pos$x <- round(pos$x)
	pos$y <- round(pos$y)
	rect(pos$x[1],pos$y[1],pos$x[2],pos$y[2],col="red",density=0.1)
	newBand <- band
	newBand@data <- band@data[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2]]
	newBand@samples <- ncol(newBand@data)
	newBand@numberOfLines <- nrow(newBand@data)
	plot(newBand,x0=pos$x[1],y0=pos$y[1])
}

zoomRGB <- function(Red, Green, Blue){
	pos <- locator(2)
	if(pos$x[1]>pos$x[2]){
		aux <- pos$x[1]
		pos$x[1] <- pos$x[2]
		pos$x[2] <- aux  
	}
	if(pos$y[1]>pos$y[2]){
		auy <- pos$y[1]
		pos$y[1] <- pos$y[2]
		pos$y[2] <- auy  
	}
	pos$x <- round(pos$x)
	pos$y <- round(pos$y)
	rect(pos$x[1],pos$y[1],pos$x[2],pos$y[2],col="red",density=0.1)
	newBandR <- Red
	newBandG <- Green
	newBandB <- Blue
	newBandR@data <- Red@data[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2]]
	newBandG@data <- Green@data[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2]]
	newBandB@data <- Blue@data[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2]]
	newBandR@samples <- ncol(newBandR@data)
	newBandG@samples <- ncol(newBandG@data)
	newBandB@samples <- ncol(newBandB@data)
	newBandR@numberOfLines <- nrow(newBandR@data)
	newBandG@numberOfLines <- nrow(newBandG@data)
	newBandB@numberOfLines <- nrow(newBandB@data)
	plot(newBandR, newBandG, newBandB,x0=pos$x[1],y0=pos$y[1])
}





	##########################
######### Class aviris_scene ############################################################
	##########################



setClass("aviris_scene",representation(name="character",numberOfLines="numeric",
	samples="numeric",bands="numeric",imageName="character",type="character",path="character"))



setMethod("initialize",
	"aviris_scene",
	function(.Object,name=character(),numberOfLines=numeric(),samples=numeric(),
		bands=numeric(),imageName=character(),type=character(),path=character()){
		.Object@name=name
		.Object@numberOfLines=numberOfLines
		.Object@samples=samples
		.Object@bands=bands
		.Object@imageName=imageName
		.Object@type=type
		.Object@path=path
		.Object
	})


	

print_information.aviris_scene <- function(Object){
	cat("  Name: ",Object@name,"\n")
	cat("  Number of lines: ",Object@numberOfLines,"\n")
	cat("  Number of samples :",Object@samples,"\n")		
	cat("  Number of bands: ",Object@bands,"\n")
}




# Interfaz para cargar una escena de la imagen #
################################################

lscene <- function(image,n){
	if (n == image@numberOfScenes){
		numberOfLines <- image@linesInLastScene
	}else if (n < image@numberOfScenes && n > 0){
		numberOfLines <- 512
	}else{
		stop("La escena solicitada no existe")
	}
	sc <- paste("_sc0",as.character(n),sep="")
	if (image@type=="reflectance"){
		name <- sub(".a",paste(sc,".a.rfl",sep=""), image@name)
	}else{
		name <- sub(".c",paste(sc,".c.img",sep=""), image@name)
	}
	Object <- new("aviris_scene",name,numberOfLines,614,224,image@name,image@type,image@path)
}



	##########################
######### Class aviris_image ############################################################
	##########################



setClass("aviris_image",representation(name="character",numberOfScenes="numeric",
	linesInLastScene="numeric",type="character",path="character"))



setMethod("initialize",
	"aviris_image",
	function(.Object,name=character(),numberOfScenes=numeric(),linesInLastScene=numeric(),
	type=character(),path=character()){
		.Object@name=name
		.Object@numberOfScenes=numberOfScenes
		.Object@linesInLastScene=linesInLastScene
		.Object@type=type
		.Object@path=path
		.Object
	})




print_information.aviris_image <- function(Object){
	cat("  Name: ",Object@name,"\n")
	cat("  Number of scenes: ",Object@numberOfScenes,"\n")
	cat("  Lines in last scene: ",Object@linesInLastScene,"\n")		
	cat("  Image type: ",Object@type,"\n")
}




# Interfaz para cargar el archivo cabecera de una imagen #
##########################################################

limage <- function(H,type){
	if (type!="reflectance" & type!="radiance"){ 
		stop("Debe especificar el tipo de imagen: reflectance o radiance")
	}
	con <- file(paste(H,".log",sep=""),"r+b")
	txt <- readLines(con)
	close(con,rw="read")
	lpath <- unlist(strsplit(H, "\\/"))
	nameImg <- lpath[length(lpath)] 
	path <- sub(lpath[length(lpath)],"",H)
	if (length(lpath)==1) path <- getwd()
	e <- txt[5]
	u <- txt[6]
	e <- as.numeric(sub("number of scenes","",e))#,fixed=TRUE))  
	u <- as.numeric(sub("lines in last scene","",u))#,fixed=TRUE))
	avirisImage <- new("aviris_image",nameImg,as.numeric(e),as.numeric(u),type,path)     	
	}


	###############################
######### Class aviris_training ############################################################
	###############################

	
setClass("aviris_training",representation(category="character",color="character"
			,scene="aviris_scene",bands="list",posX="numeric",posY="numeric"))


setMethod("initialize",
	"aviris_training",
	function(.Object, category=character(),color=character(),scene=aviris_scene,
			band=list(),posX=vector(),posY=vector()){
		.Object@category <- category
		.Object@color <- color
		.Object@scene <- scene
#### tratamiento por si las bandas pertenecen a diferentes escenas ##################
		i <- 1																		#
		bool<-TRUE																	#
		while (i!=(length(band))){													#
			bool <- all.equal(band[[i]]@scene, band[[i+1]]@scene)					#
####		cat(band[[i]]@scene," = ", band[[i+1]]@scene," es ", bool,"\n")			#
			i <- i+1																#
		}																			#
		if (!bool) {																#
			cat("OJO QUE LAS BANDAS PERTENECEN A DIFERENTES ESCENAS \n")			#
		}else {																		#
			cat("TODO BIEN LOCO \n")												#
		}																			#
###	la decision que tome es solo advertirle al usuario que las bandas pertenecen a 	#
###	distintas escenas. 																#
#####################################################################################
		.Object@bands <- band
		.Object
	}
)

print_information.aviris_training <- function(Object){
	cat("  Category: ",Object@category,"\n")
	cat("  Color: ",Object@color,"\n")
	cat("  Scene: ",Object@scene@name,"\n")
	i <- 0
	cat("BANDAS INVOLUCRADAS \n")
	while (i!=(length(Object@bands))){
		i <- i+1
		cat("  Band : ",Object@bands[[i]]@band,"\n")
	}
}



#################################################################################
# falta hacer analisis por casos porque falta cubrir algunos casos de parametros 
# incorrectos. por ejemplo "takeSamples(clase,5, 56)->clase"
#################################################################################
takeSamples <- function(t, n=NULL, Sample=NULL, Line=NULL){
	if (is.null(Sample) && is.null(Line) && is.null(n)){ 
		p <- locator(1,type="p",pch=20, col=t@color)
		t@posY <- c(t@posY, round(p$y))
		t@posX <- c(t@posX, round(p$x))
	}else if (is.null(Sample) && is.null(Line) && !is.null(n)){ 
		p <- locator(n,type="p",pch=20, col=t@color)
		t@posY <- c(t@posY, round(p$y))
		t@posX <- c(t@posX, round(p$x))
	}else if ((is.null(Sample) && !is.null(Line)) || ((is.null(n) && !is.null(Sample) && is.null(Line))) ){ 
      	stop("Incorrect parameters")
	}else{
		t@posY <- c(t@posY, Line)
		t@posX <- c(t@posX, Sample)
		points(Sample, Line, type="p",pch=20, col="green")
	} 
	cat("  X value: ",t@posX,"\n")
	cat("  Y value: ",t@posY,"\n")
	t
}

#############################################################################################


Zprofile <- function(scene, X=NULL, Y=NULL){
	if (is.null(X) && is.null(Y)){ 
		p <- locator(1)
		Y <- round(p$y)
		X <- round(p$x)
	}else if ((is.null(X) && !is.null(Y)) || ((!is.null(X) && is.null(Y))) ){ 
      	stop("Incorrect parameters")
	} 
	I <- paste(scene@path,scene@name,sep="/")
	conexionParaLeer <- file(as.character(I),open="r+b")   
	seek(conexionParaLeer,where=(224*((Y-1)*614+(X-1))*2),rw="read")
	Z <- vector()
	i <- 1
	while (i!=225) {
		Z <- c(Z,readBin(conexionParaLeer,"integer",size=2,endian="swap"))
		i <- i+1
	}
	close(conexionParaLeer,rw="read")
	x11()
	plot(Z, type="l", col="red")	
    Z
}

##################################################################################################################

##################################################################################################################
####################################### Functions ################################################################
##################################################################################################################

# Function to read LAN images
read.lan <- function(arquivo){
	LANfile <<- arquivo
	res <- .C("getImgData",file=as.character(arquivo),numbands=as.integer(0),row=as.integer(0),col=as.integer(0),PACKAGE="ripa")
	row <- res$row
	col <- res$col
	nbands <- res$numbands
	imgMatrix <- array(0,dim=c(row,col,nbands))
	n <- row*col
	res <- .C("readLAN",file = as.character(arquivo),out = as.integer(rep(0,n*nbands)),PACKAGE="ripa")
	mat <- matrix(res$out,ncol=nbands,byrow=T)
	for (i in 1:nbands){
		band <- matrix(mat[,i]/255,nrow=row,byrow=T)
		imgMatrix[,,i] <- band
	}
	return(imgMatrix)
}
#

# Function ro write LAN images
write.lan <- function(arquivo,img){
	mat <- matrix(0,ncol=dim(img)[3],nrow=(nrow(img)*ncol(img)))
	
	for (i in 1:dim(img)[3]){
		mat[,i] = t(img[,,i]*255)
	}
	
	res <- .C("writeLAN",file1 = as.character(LANfile),file2 = as.character(arquivo), as.integer(as.vector(t(mat))),PACKAGE="ripa")
}
#

# Function to apply the median filter to an image
medianImg <- function(img,mask){
	if (is.na(dim(img)[3])) img = array(img,dim=c(nrow(img),ncol(img),1))
	else img = array(img,dim=dim(img))
	n <- nrow(img)*ncol(img)
	nrow <- as.integer(nrow(img))
	ncol <- as.integer(ncol(img))
	mat <- img
	for (i in 1:dim(img)[3]){
		image <- as.double(as.vector(t(matrix(img[,,i],nrow=nrow(img)))))
		out <- .C("median",image,nrow, ncol,mask,outImg=as.double(rep(0.0,n)),PACKAGE="ripa")
		mat[,,i] <- matrix(out$outImg,ncol=ncol(img),byrow=T)
	}
	return(mat)
}
#

# Linear contrast stretch function
stretchImg <- function(img){
	if (is.na(dim(img)[3])) img <- array(img,dim=c(nrow(img),ncol(img),1))
	else img <- array(img,dim=dim(img))
	
	img2 <- array(0,dim=dim(img))
	n <- nrow(img)*ncol(img)
	a <- as.double(0)
	b <- as.double(1)
	for (i in 1:dim(img)[3]){
		x <- quantile(img[,,i],seq(0,1,by=0.05))
		c <- as.double(x[[2]])
		d <- as.double(x[[20]])
		res <- .C("stretch",img[,,i],a,b,c,d,as.integer(nrow(img)),as.integer(ncol(img)),out=as.double(rep(0,n)),PACKAGE="ripa")
		mat <- matrix(res$out,ncol=ncol(img))
		img2[,,i] <- imagematrix(mat)
		print(i)
	}
	return(img2)
}

# Function to apply the new brightness and contrast
contBriImg <- function(img,cont,bri){

	if (is.na(dim(img)[3])) img <- array(img,dim=c(nrow(img),ncol(img),1))
	else img <- array(img,dim=dim(img))
	
	if (is.na(cont)) cont <- 1
	if (is.na(bri)) bri <- 0
	img5<-img
	for (i in 1:dim(img)[3]){
		img2 <-as.matrix(img[,,i])
		
		img2 <- cont*img2+bri
		
		nrow <- as.integer(nrow(img2))
		ncol <- as.integer(ncol(img2))
		
		out <- .C("normalize",image<-as.vector(as.double(img2)),nrow,ncol,as.double(bri),PACKAGE="ripa")
		
		img2 <- matrix(image,nrow=nrow)
		
		img5[,,i]<-img2
	}
	return(img5)
}

# Function to read AVIRIS images
read.aviris <- function(fileName){
	aux <- tclvalue(tcl(tn,"select"))
 	
 	if (aux==".1.1.1") bandsIndexes <- AVIRISbands1
	else bandsIndexes <- AVIRISbands2

	img <- limage(as.character(fileName),"reflectance")
	imgtmpScene2 <- lscene(img,2)
	#print(Sys.time())
	#names(bands) <- bandsIndexes
	mat <- array(0,dim=c(512,614,length(bandsIndexes)))
	bands <<- vector(length=length(bandsIndexes),mode="list")
	
	pb <- tkProgressBar(title = "Reading bands...", min = 0, max = length(bandsIndexes), width = 300)
	for (i in 1:length(bandsIndexes)){
		bands[[i]] <<- lband(imgtmpScene2,bandsIndexes[i])
		mat[,,i] <- clineal(bands[[i]]@data,0,1)
		if (is.nan(mat[,,i][1])){
			for (j in 1:length(mat[,,i])) mat[,,i][j] <- 0
		}
		#print(paste("Band ",bandsIndexes[i],sep="")
		setTkProgressBar(pb, i, label=paste(round(i/length(bandsIndexes)*100, 0),"% done"))
	}
	#print(Sys.time())
	close(pb)
	return(mat)
}

# Function to build a modal dialog
modalDialog <- function(title,question,entryInit,entryWidth=20,returnValOnCancel="ID_CANCEL"){
	dlg <- tktoplevel()
	tkwm.deiconify(dlg)
	#tkgrab.set(dlg)
	tkfocus(dlg)
	tkwm.title(dlg,title)
	tkwm.geometry(dlg,"+350+300")
	textEntryVarTcl <- tclVar(paste(entryInit))
	textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
	tkgrid(tklabel(dlg,text="       "))
	tkgrid(tklabel(dlg,text=question),textEntryWidget)
	tkgrid(tklabel(dlg,text="       "))
	ReturnVal <- returnValOnCancel
	onOK <- function(){
		ReturnVal <<- tclvalue(textEntryVarTcl)
		tkgrab.release(dlg)
		tkdestroy(dlg)
		}
	onCancel <- function(){
		ReturnVal <<- returnValOnCancel
		tkgrab.release(dlg)
		tkdestroy(dlg)
		#tkfocus(ttMain)
	}
	OK.but <-tkbutton(dlg,text="   OK   ",command=onOK)
	Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
	tkgrid(OK.but,Cancel.but)
	tkgrid(tklabel(dlg,text="    "))
	tkfocus(dlg)
	tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg)})
	tkbind(textEntryWidget, "<Return>", onOK)
	tkwait.window(dlg)
	return(ReturnVal)
}

##################################################################################################################

##################################################################################################################
################################################## GUI ###########################################################
##################################################################################################################

RIPAgui <- function(){

	##################################################################################################################
	############################# Load shared library with C funcions ################################################
	##################################################################################################################
	dyn.load(system.file("libs/ripa.so",package="ripa"))
	##################################################################################################################


	##################################################################################################################
	#################################### Required packages ###########################################################
	##################################################################################################################
	tclRequire("BWidget")
	tclRequire("Tktable")
	tclRequire("Img")
	##################################################################################################################

	ans <- tkmessageBox(title="Resolution",message="We recomend to use this package with resolution of 1024x768. Do you want to continue?",icon="question",type="yesno")
	ans <- as.character(ans)
	if (ans=="no") return()
	
	##################################################################################################################
	############################### Initialization of some variables #################################################
	##################################################################################################################
	img1 <<- NULL
	img2 <<- NULL
	img3 <<- NULL
	img4 <<- NULL
	visualBands1 <<- c(1,2,3)
	visualBands2 <<- c(1,2,3)
	AVIRISbands1 <<- NULL
	AVIRISbands2 <<- NULL
	imageType1 <<- NULL
	imageType2 <<- NULL
	numbands1 <<- 0
	numbands2 <<- 0
	regionsList <<- list()
	regionType <<- NULL
	regionPointsX <<- NULL
	regionPointsY <<- NULL
	LANfile <<- NULL
	##################################################################################################################
	
	##################################################################################################################
	###################################### Main window ###############################################################
	##################################################################################################################
	tt <- tktoplevel()
	tkwm.resizable(tt,0,0)
	tktitle(tt)<-"Image Processing and Analysis in R"
	tn <<- ttknotebook(tt)
	tkpack(tn)
	tkconfigure(tn,width=1015,height=670)
	##################################################################################################################
	
	##################################################################################################################
	#################################### Operations Tab ##############################################################
	##################################################################################################################
	
	#################################### Auxiliar Funcitions #########################################################
	
	# Function to build histograms of the region bands
	regionBands <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if (aux==".1.1.1"){
			tkmessageBox(title="Error",message="Please, use this function only with the second tab!",icon="error",type="ok")
			return()
		}
		
		if (length(regionsList)==0){
			tkmessageBox(title="Error",message="Please, select at least one region in order to use it!",icon="error",type="ok")
			return()
		}
		build1 <- function(){
			ttregionsbands1<<-tktoplevel()
			tkwm.geometry(ttregionsbands1,"+400+700")
			tkwm.resizable(ttregionsbands1,0,0)
			tktitle(ttregionsbands1)<<-"Regions"
			
			scr1 <- tkscrollbar(ttregionsbands1, repeatinterval=5, command=function(...)tkyview(tl1,...))
			tl1<-tklistbox(ttregionsbands1,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr1,...),background="white")
			tkgrid(tklabel(ttregionsbands1,text="Choose the region"))
			tkgrid(tl1,scr1)
			tkgrid.configure(scr1,rowspan=6,sticky="nsw")
			
			regions <- names(regionsList)
			
			for (i in (1:length(regionsList))){
				tkinsert(tl1,"end",regions[i])
			}
			tkselection.set(tl1,0)
			
			OnOK1 <- function(){
				regionChoice <<- as.numeric(tkcurselection(tl1))+1
				tkdestroy(ttregionsbands1)
				build2()
			}
			
			OK1.but <-tkbutton(ttregionsbands1,text="   OK   ",command=OnOK1)
			tkgrid(OK1.but)
			tkfocus(ttregionsbands1)
		}
		
		build2 <- function(){
			ttregionsbands2<<-tktoplevel()
			tkwm.geometry(ttregionsbands2,"+400+700")
			tkwm.resizable(ttregionsbands2,0,0)
			tktitle(ttregionsbands2)<<-"Bands"
			
			scr2 <- tkscrollbar(ttregionsbands2, repeatinterval=5, command=function(...)tkyview(tl2,...))
			tl2<-tklistbox(ttregionsbands2,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr2,...),background="white")
			tkgrid(tklabel(ttregionsbands2,text="Choose the band"))
			tkgrid(tl2,scr2)
			tkgrid.configure(scr2,rowspan=6,sticky="nsw")
			
			bs <- NULL
			for (i in 1:numbands2){
				if (is.null(AVIRISbands2)) bs <- c(bs,paste("Band ",i,sep=""))
				else bs <- c(bs,paste("Band ",AVIRISbands[i],sep=""))
			}
			
			for (i in (1:numbands2)){
				tkinsert(tl2,"end",bs[i])
			}
			tkselection.set(tl2,0)
			
			OnOK2 <- function(){
				bandChoice <- as.numeric(tkcurselection(tl2))+1
				tkdestroy(ttregionsbands2)
				
				ttregionhist <- tktoplevel()
				tkwm.geometry(ttregionhist,"+700+0")
				tkwm.resizable(ttregionhist,0,0)
				if (is.null(AVIRISbands2)){
					tktitle(ttregionhist)<-paste("Region ",regionChoice," Band ",bandChoice,sep="")
					histimage <-tkrplot(ttregionhist, function() hist(regionsList[[regionChoice]][[2]][[bandChoice]],main=paste("Band",bandChoice),100,xlab="Values"),vscale=1.04,hscale=0.98)
				}
				else{
					tktitle(ttregionhist)<-paste("Region ",regionChoice," Band ",AVIRISbands2[bandChoice],sep="")
					histimage <-tkrplot(ttregionhist, function() hist(regionsList[[regionChoice]][[2]][[bandChoice]],main=paste("Band",AVIRISbands2[bandChoice]),100,xlab="Values"),vscale=1.04,hscale=0.98)
				}
				
				tkpack(histimage)
				build1()
			}
			OK2.but <-tkbutton(ttregionsbands2,text="   OK   ",command=OnOK2)
			tkgrid(OK2.but)
			tkfocus(ttregionsbands2)
		}
		build1()
	}
	#
	
	# Function to build regions bands brushplots
	regionBrush <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if (aux==".1.1.1"){
			tkmessageBox(title="Error",message="Please, use this function only with the second tab!",icon="error",type="ok")
			return()
		}
		
		if (length(regionsList)==0){
			tkmessageBox(title="Error",message="Please, select at least one region in order to use it!",icon="error",type="ok")
			return()
		}
		
		if (imageType2=="jpgGrey"){
			tkmessageBox(title="Error",message="Please, use this function with multiple bands images!",icon="error",type="ok")
			return()
		}
		
		build1 <- function(){
			ttregionsbands1<<-tktoplevel()
			tkwm.geometry(ttregionsbands1,"+400+700")
			tkwm.resizable(ttregionsbands1,0,0)
			tktitle(ttregionsbands1)<-"Regions"
			
			scr1 <- tkscrollbar(ttregionsbands1, repeatinterval=5, command=function(...)tkyview(tl1,...))
			tl1<-tklistbox(ttregionsbands1,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr1,...),background="white")
			tkgrid(tklabel(ttregionsbands1,text="Choose the region"))
			tkgrid(tl1,scr1)
			tkgrid.configure(scr1,rowspan=6,sticky="nsw")
			
			regions <- names(regionsList)
			
			for (i in (1:length(regionsList))){
				tkinsert(tl1,"end",regions[i])
			}
			tkselection.set(tl1,0)
			
			OnOK1 <- function(){
				regionChoice <<- as.numeric(tkcurselection(tl1))+1
				tkdestroy(ttregionsbands1)
				build2()
			}
			
			OK1.but <-tkbutton(ttregionsbands1,text="   OK   ",command=OnOK1)
			tkgrid(OK1.but)
			tkfocus(ttregionsbands1)
		}
		
		build2 <- function(){
			ttregionsbands2<<-tktoplevel()
			tkwm.geometry(ttregionsbands2,"+400+700")
			tkwm.resizable(ttregionsbands2,0,0)
			tktitle(ttregionsbands2)<-"Bands"
			
			scr2 <- tkscrollbar(ttregionsbands2, repeatinterval=5, command=function(...)tkyview(tl2,...))
			tl2<-tklistbox(ttregionsbands2,height=6,width=30,selectmode="multiple",yscrollcommand=function(...)tkset(scr2,...),background="white")
			tkgrid(tklabel(ttregionsbands2,text="Choose the bands"))
			tkgrid(tl2,scr2)
			tkgrid.configure(scr2,rowspan=6,sticky="nsw")
			
			bs <- NULL
			for (i in 1:numbands2){
				if (is.null(AVIRISbands2)) bs <- c(bs,paste("Band ",i,sep=""))
				else bs <- c(bs,paste("Band ",AVIRISbands2[i],sep=""))
			}
			
			for (i in (1:numbands2)){
				tkinsert(tl2,"end",bs[i])
			}
			tkselection.set(tl2,0)
			
			OnOK2 <- function(){
				bandChoice <- as.vector(as.numeric(tkcurselection(tl2)))+1
				tkdestroy(ttregionsbands2)
				if (length(bandChoice)==1){
					tkmessageBox(title="Error",message="Please, choose at least two bands!",icon="error",type="ok")
					return()
				}
				bs <- NULL
				for (i in 1:length(bandChoice)){
					if (is.null(AVIRISbands2)) bs <- c(bs,paste("Band ",bandChoice[i],sep=""))
					else bs <- c(bs,paste("Band ",AVIRISbands2[bandChoice[i]],sep=""))
				}
				bandsValues <<- matrix(nrow=length(regionsList[[regionChoice]][[2]][[1]]),ncol=length(bandChoice))
				for (i in 1:length(bandChoice)){
					bandsValues[,i]<<-regionsList[[regionChoice]][[2]][[bandChoice[i]]]
				}
				bandsValues <<- as.data.frame(bandsValues)
				names(bandsValues)<<-bs
				pairs(bandsValues,pch=".")
				
				build1()
			}
			OK2.but <-tkbutton(ttregionsbands2,text="   OK   ",command=OnOK2)
			tkgrid(OK2.but)
			tkfocus(ttregionsbands2)
		}
		build1()
	}
	#
	
	# Function to build bands brushplots
	brush <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if ((aux==".1.1.1" && imageType1=="jpgGrey") || (aux==".1.1.2" && imageType2=="jpgGrey")){
			tkmessageBox(title="Error",message="Please, use this function with multiple bands images!",icon="error",type="ok")
			return()
		}
		
		#build1 <- function(){
		#	ttregionsbands1<<-tktoplevel()
		#	tkwm.geometry(ttregionsbands1,"+400+700")
		#	tkwm.resizable(ttregionsbands1,0,0)
		#	tktitle(ttregionsbands1)<-"Regions"
		#	
		#	scr1 <- tkscrollbar(ttregionsbands1, repeatinterval=5, command=function(...)tkyview(tl1,...))
		#	tl1<-tklistbox(ttregionsbands1,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr1,...),background="white")
		#	tkgrid(tklabel(ttregionsbands1,text="Choose the region"))
		#	tkgrid(tl1,scr1)
		#	tkgrid.configure(scr1,rowspan=6,sticky="nsw")
		#	
		#	regions <- names(regionsList)
		#	
		#	for (i in (1:length(regionsList))){
		#		tkinsert(tl1,"end",regions[i])
		#	}
		#	tkselection.set(tl1,0)
		#	
		#	OnOK1 <- function(){
		#		regionChoice <<- as.numeric(tkcurselection(tl1))+1
		#		tkdestroy(ttregionsbands1)
# 				build2()
# 			}
# 			
# 			OK1.but <-tkbutton(ttregionsbands1,text="   OK   ",command=OnOK1)
# 			tkgrid(OK1.but)
# 			tkfocus(ttregionsbands1)
# 		}
		
		build2 <- function(){
			ttbands<<-tktoplevel()
			tkwm.geometry(ttbands,"+400+700")
			tkwm.resizable(ttbands,0,0)
			tktitle(ttbands)<-"Bands"
			
			scr <- tkscrollbar(ttbands, repeatinterval=5, command=function(...)tkyview(tl,...))
			tl<-tklistbox(ttbands,height=6,width=30,selectmode="multiple",yscrollcommand=function(...)tkset(scr,...),background="white")
			tkgrid(tklabel(ttbands,text="Choose the bands"))
			tkgrid(tl,scr)
			tkgrid.configure(scr,rowspan=6,sticky="nsw")
			
			bs <- NULL
			if (aux==".1.1.1"){
				for (i in 1:numbands1){
					if (is.null(AVIRISbands1)) bs <- c(bs,paste("Band ",i,sep=""))
					else bs <- c(bs,paste("Band ",AVIRISbands1[i],sep=""))
				}
			}
			else{
				for (i in 1:numbands2){
					if (is.null(AVIRISbands2)) bs <- c(bs,paste("Band ",i,sep=""))
					else bs <- c(bs,paste("Band ",AVIRISbands2[i],sep=""))
				}
			}
			if (aux==".1.1.1") N=numbands1
			else N=numbands2
			for (i in (1:N)){
				tkinsert(tl,"end",bs[i])
			}
			tkselection.set(tl,0)
			
			OnOK <- function(){
				bandChoice <- as.vector(as.numeric(tkcurselection(tl)))+1
				tkdestroy(ttbands)
				if (length(bandChoice)==1){
					tkmessageBox(title="Error",message="Please, choose at least two bands!",icon="error",type="ok")
					return()
				}
				bs <- NULL
				if (aux==".1.1.1"){
					for (i in 1:length(bandChoice)){
						if (is.null(AVIRISbands1)) bs <- c(bs,paste("Band ",bandChoice[i],sep=""))
						else bs <- c(bs,paste("Band ",AVIRISbands1[bandChoice[i]],sep=""))
					}
				}
				else{
					for (i in 1:length(bandChoice)){
						if (is.null(AVIRISbands2)) bs <- c(bs,paste("Band ",bandChoice[i],sep=""))
						else bs <- c(bs,paste("Band ",AVIRISbands2[bandChoice[i]],sep=""))
					}
				}
# 				if (aux==".1.1.1"){
# 					if (!is.null(AVIRISbands1)) bandsValues <<- matrix(nrow=nrow(bands[[1]]@data),ncol=ncol(bands[[1]]@data))
# 					else bandsValues <<- matrix(nrow=nrow(img1),ncol=ncol(img1))
# 				}
# 				else{
# 					if (!is.null(AVIRISbands2)) bandsValues <<- matrix(nrow=nrow(bands[[1]]@data),ncol=ncol(bands[[1]]@data))
# 					else bandsValues <<- matrix(nrow=nrow(img3),ncol=ncol(img3))
# 				}
				if (aux==".1.1.1"){
					if (is.null(AVIRISbands1)) bandsValues <<- data.frame(a=as.vector(img1[,,bandChoice[1]]))
					else bandsValues <<- data.frame(a = as.vector(bands[[bandChoice[1]]]@data))
				}
				else{
					if (is.null(AVIRISbands2)) bandsValues <<- data.frame(a=as.vector(img3[,,bandChoice[1]]))
					else bandsValues <<- data.frame(a = as.vector(bands[[bandChoice[1]]]@data))
				}				
				for (i in 2:length(bandChoice)){
					if (aux==".1.1.1"){
						if (is.null(AVIRISbands1)) bandsValues[,i]<<-as.vector(img1[,,bandChoice[i]])
						else bandsValues[,i] <<- as.vector(bands[[bandChoice[i]]]@data)
					}
					else{
						if (is.null(AVIRISbands2)) bandsValues[,i]<<-as.vector(img3[,,bandChoice[i]])
						else bandsValues[,i] <<- as.vector(bands[[bandChoice[i]]]@data)
					}
				}
				#bandsValues <<- as.data.frame(bandsValues)
				names(bandsValues)<<-bs
				pairs(bandsValues,pch=".")
				
				#build1()
			}
			OK.but <-tkbutton(ttbands,text="   OK   ",command=OnOK)
			tkgrid(OK.but)
			tkfocus(ttbands)
		}
		build2()
	}
	#

	# Function to shows bands histogram and statistics
	imageBands <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if ((aux==".1.1.1" && is.null(img1)) || (aux==".1.1.2" && is.null(img3)) ){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		ttbands<-tktoplevel()
		tkwm.geometry(ttbands,"+600+700")
		tkwm.resizable(ttbands,0,0)
		tktitle(ttbands)<-"List of bands"
		scr <- tkscrollbar(ttbands, repeatinterval=5, command=function(...)tkyview(tl,...))
		tl<-tklistbox(ttbands,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		tkgrid(tklabel(ttbands,text="Choose the band"))
		tkgrid(tl,scr)
		tkgrid.configure(scr,rowspan=6,sticky="nsw")
		bands <- NULL
		
		if (aux==".1.1.1"){
			for (i in 1:numbands1){
				if (is.null(AVIRISbands1)) bands <- c(bands,paste("Band ",i,sep=""))
				else bands <- c(bands,paste("Band ",AVIRISbands1[i],sep=""))
			}
			for (i in (1:numbands1)){
				tkinsert(tl,"end",bands[i])
			}
		}
		
		if (aux==".1.1.2"){
			for (i in 1:numbands2){
				if (is.null(AVIRISbands2)) bands <- c(bands,paste("Band ",i,sep=""))
				else bands <- c(bands,paste("Band ",AVIRISbands2[i],sep=""))
			}
			for (i in (1:numbands2)){
				tkinsert(tl,"end",bands[i])
			}
		}
		
		tkselection.set(tl,0)
		
		OnOK <- function(){
			bandChoice <- as.numeric(tkcurselection(tl))+1
			
			aux <- tclvalue(tcl(tn,"select"))
			
			ttstatistics<-tktoplevel()
			tkwm.geometry(ttstatistics,"+0+700")
			tkwm.resizable(ttstatistics,0,0)
			if (aux==".1.1.1"){
				if (is.null(AVIRISbands1)) tktitle(ttstatistics)<-paste("Band ",bandChoice," statistics",sep="")
				else tktitle(ttstatistics)<-paste("Band ",AVIRISbands1[bandChoice]," statistics",sep="")
			}
			if (aux==".1.1.2"){
				if (is.null(AVIRISbands2)) tktitle(ttstatistics)<-paste("Band ",bandChoice," statistics",sep="")
				else tktitle(ttstatistics)<-paste("Band ",AVIRISbands2[bandChoice]," statistics",sep="")
			}
			scr <- tkscrollbar(ttstatistics, repeatinterval=5,command=function(...)tkyview(txt,...))
			txt <- tktext(ttstatistics,bg="white",font="courier",width=40,height=10,yscrollcommand=function(...)tkset(scr,...))
			tkgrid(txt,scr)
			tkgrid.configure(scr,sticky="ns")
			
			if (aux==".1.1.1"){
				ttband <- tktoplevel()
				tkwm.geometry(ttband,"+0+0")
				tkwm.resizable(ttband,0,0)
				if (is.null(AVIRISbands1)) tktitle(ttband)<-paste("Band ",bandChoice,sep="")
				else tktitle(ttband)<-paste("Band ",AVIRISbands1[bandChoice],sep="")
				bandimage <-tkrplot(ttband, function() plot(imagematrix(img1[,,bandChoice])),vscale=1.04,hscale=0.98)
				tkpack(bandimage)
				
				nR <- length(img1[,,bandChoice])
				minR <- min(img1[,,bandChoice])
				maxR <- max(img1[,,bandChoice])
				meanR <- mean(img1[,,bandChoice])
				medianR <- median(img1[,,bandChoice])
				deviationR <- sd(as.vector(img1[,,bandChoice]))
				mDeviationR <- mad(img1[,,bandChoice])
				kurtosisR <- kurtosis(as.vector(img1[,,bandChoice]))
				skewnessR <- skewness(as.vector(img1[,,bandChoice]))
				
				ttbandhist <- tktoplevel()
				tkwm.geometry(ttbandhist,"+500+0")
				tkwm.resizable(ttbandhist,0,0)
				if (is.null(AVIRISbands1)){
					tktitle(ttbandhist)<-paste("Band ",bandChoice,sep="")
					histimage <-tkrplot(ttbandhist, function() hist(img1[,,bandChoice],main=paste("Band",bandChoice),100,xlab="Values"),vscale=1.04,hscale=0.98)
				}
				else{
					tktitle(ttbandhist)<-paste("Band ",AVIRISbands1[bandChoice],sep="")
					histimage <-tkrplot(ttbandhist, function() hist(img1[,,bandChoice],main=paste("Band",AVIRISbands1[bandChoice]),100,xlab="Values"),vscale=1.04,hscale=0.98)
				}
				tkpack(histimage)
			}
			if (aux==".1.1.2"){
				ttband <- tktoplevel()
				tkwm.geometry(ttband,"+0+0")
				tkwm.resizable(ttband,0,0)
				if (is.null(AVIRISbands2)) tktitle(ttband)<-paste("Band ",bandChoice,sep="")
				else tktitle(ttband)<-paste("Band ",AVIRISbands2[bandChoice],sep="")
				bandimage <-tkrplot(ttband, function() plot(imagematrix(img3[,,bandChoice])),vscale=1.04,hscale=0.98)
				tkpack(bandimage)
				
				nR <- length(img3[,,bandChoice])
				minR <- min(img3[,,bandChoice])
				maxR <- max(img3[,,bandChoice])
				meanR <- mean(img3[,,bandChoice])
				medianR <- median(img3[,,bandChoice])
				deviationR <- sd(as.vector(img3[,,bandChoice]))
				mDeviationR <- mad(img3[,,bandChoice])
				kurtosisR <- kurtosis(as.vector(img3[,,bandChoice]))
				skewnessR <- skewness(as.vector(img3[,,bandChoice]))
				
				ttbandhist <- tktoplevel()
				tkwm.geometry(ttbandhist,"+500+0")
				tkwm.resizable(ttbandhist,0,0)
				if (is.null(AVIRISbands2)){
					tktitle(ttbandhist)<-paste("Band ",bandChoice,sep="")
					histimage <-tkrplot(ttbandhist, function() hist(img3[,,bandChoice],main=paste("Band",bandChoice),100,xlab="Values"),vscale=1.04,hscale=0.98)
				} else{
					tktitle(ttbandhist)<-paste("Band ",AVIRISbands2[bandChoice],sep="")
					histimage <-tkrplot(ttbandhist, function() hist(img3[,,bandChoice],main=paste("Band",AVIRISbands2[bandChoice]),100,xlab="Values"),vscale=1.04,hscale=0.98)
				}
				tkpack(histimage)
			}

			tkconfigure(txt,state="normal")
			tkinsert(txt,"end",paste("N = ",nR,"\n",sep=""))
			tkinsert(txt,"end",paste("Min = ",minR,"\n",sep=""))
			tkinsert(txt,"end",paste("Max = ",maxR,"\n",sep=""))
			tkinsert(txt,"end",paste("Mean = ",meanR,"\n",sep=""))
			tkinsert(txt,"end",paste("Median = ",medianR,"\n",sep=""))
			tkinsert(txt,"end",paste("Deviation = ",deviationR,"\n",sep=""))
			tkinsert(txt,"end",paste("Median Deviation = ",mDeviationR,"\n",sep=""))
			tkinsert(txt,"end",paste("Kurtosis = ",kurtosisR,"\n",sep=""))
			tkinsert(txt,"end",paste("Skewness = ",skewnessR,"\n\n",sep=""))
			
			tkconfigure(txt,state="disable")
			
		}
		OK.but <-tkbutton(ttbands,text=   " OK ",command=OnOK)
		tkgrid(OK.but)
		tkfocus(ttbands)
	}
	#
	
	# Function to calculate the covariance matrix of a region
	covMatrix <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if (aux==".1.1.1"){
			tkmessageBox(title="Error",message="Please, use this function only with the second tab!",icon="error",type="ok")
			return()
		}
		
		if (is.null(imageType2)){
			tkmessageBox(title="Error",message="Please, open an image in oder to use it!",icon="error",type="ok")
			return()
		}
		
		if (length(regionsList)==0){
			tkmessageBox(title="Error",message="Please, select at least one region in order to use it!",icon="error",type="ok")
			return()
		}
		
		if (imageType2=="jpgGrey"){
			tkmessageBox(title="Error",message="Please, use this function only with multiple bands images!",icon="error",type="ok")
			return()
		}
		
		ttregions<-tktoplevel()
		tkwm.geometry(ttregions,"+400+700")
		tkwm.resizable(ttregions,0,0)
		tktitle(ttregions)<-"Regions"
		
		scr <- tkscrollbar(ttregions, repeatinterval=5, command=function(...)tkyview(tl,...))
		tl<-tklistbox(ttregions,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		tkgrid(tklabel(ttregions,text="Choose the region"))
		tkgrid(tl,scr)
		tkgrid.configure(scr,rowspan=6,sticky="nsw")
		
		regions <- as.vector(names(regionsList))
		
		for (i in (1:length(regionsList))){
			tkinsert(tl,"end",regions[i])
		}
		tkselection.set(tl,0)
		
		OnOK <- function(){
			regionChoice <- as.numeric(tkcurselection(tl))+1
			tkdestroy(ttregions)
			bands <- NULL
			for (i in 1:numbands2){
				if (is.null(AVIRISbands2)) bands <- c(bands,paste("Band ",i,sep=""))
				else bands <- c(bands,paste("Band ",AVIRISbands2[i],sep=""))
			}
			
			Matrix <- matrix(nrow=length(regionsList[[regionChoice]][[2]][[1]]),ncol=numbands2)
			for (i in 1:numbands2){
				Matrix[,i]<-regionsList[[regionChoice]][[2]][[i]]
			}
			Matrix <- as.data.frame(Matrix)
			names(Matrix)<-bands
			covMat <- cov(Matrix)
			displayInTable <- function(tclarray,title="",height=-1,width=-1,nrow=-1,ncol=-1){
				tttable <- tktoplevel()
				tkwm.title(tttable,title)
				tkwm.geometry(tttable,"+0+70")
				tkwm.resizable(tttable,0,0)
				table <- tkwidget(tttable,"table",rows=nrow,cols=ncol,titlerows=1,titlecols=1,colwidth=20,
					height=height+1,width=width+1,
					xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
				xscr <-tkscrollbar(tttable,orient="horizontal", command=function(...)tkxview(table,...))
				yscr <- tkscrollbar(tttable,command=function(...)tkyview(table,...))
				tkgrid(table,yscr)
				tkgrid.configure(yscr,sticky="nsw")
				tkgrid(xscr,sticky="new")
				tkconfigure(table,variable=tclarray,background="white",selectmode="extended")
				return (table)
			}
			tclArray <- tclArray()
			#col <- ncol(regionsList[[regionChoice]][[3]])
			for (i in 1:numbands2) tclArray[[0,i]] <- names(Matrix)[i]
			for (i in 1:numbands2) tclArray[[i,0]] <- names(Matrix)[i]
			for (i in 1:numbands2){
				for (j in 1:numbands2){
					tclArray[[i,j]] <- covMat[i,j]
				}
			}
			table <- displayInTable(tclArray,title=paste("Covariance Matrix ","(Region ",regionChoice,")"),nrow=numbands2+1,ncol=numbands2+1)
			
			tkconfigure(table, state="disabled")
		}
		
		OK.but <-tkbutton(ttregions,text="   OK   ",command=OnOK)
		tkgrid(OK.but)
		tkfocus(ttregions)
	}
	#
	
	# Function to change the actual visual bands
	changeBands <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if ((aux==".1.1.1" && is.null(imageType1)) || (aux==".1.1.2" && is.null(imageType2))){
			tkmessageBox(title="Error",message="Please, open an image in oder to use it!",icon="error",type="ok")
			return()
		}
		
		if ((aux==".1.1.1" && imageType1=="jpgGrey") || (aux==".1.1.2" && imageType2=="jpgGrey") || (aux==".1.1.1" && numbands1==3) || (aux==".1.1.2" && numbands2==3)){
			tkmessageBox(title="Error",message="Please, use this function with more than three bands images!",icon="error",type="ok")
			return()
		}
		
		if (aux==".1.1.1") visualBands1<<-NULL
		if (aux==".1.1.2") visualBands2<<-NULL
		
		build <- function(count){
			ttregions<-tktoplevel()
			tkwm.geometry(ttregions,"+400+300")
			tkwm.resizable(ttregions,0,0)
			tktitle(ttregions)<-paste("Bands ",count,sep="")
			
			scr <- tkscrollbar(ttregions, repeatinterval=5, command=function(...)tkyview(tl,...))
			tl<-tklistbox(ttregions,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
			tkgrid(tklabel(ttregions,text="Choose the band"))
			tkgrid(tl,scr)
			tkgrid.configure(scr,rowspan=6,sticky="nsw")
			
			bands <- NULL
			if (aux==".1.1.1"){
				for (i in 1:numbands1){
					if (is.null(AVIRISbands1)) bands <- c(bands,paste("Band ",i,sep=""))
					else bands <- c(bands,paste("Band ",AVIRISbands1[i],sep=""))
				}
				for (i in 1:numbands1)
					tkinsert(tl,"end",bands[i])
			}
			if (aux==".1.1.2"){
				for (i in 1:numbands2){
					if (is.null(AVIRISbands2)) bands <- c(bands,paste("Band ",i,sep=""))
					else bands <- c(bands,paste("Band ",AVIRISbands2[i],sep=""))
				}
				for (i in 1:numbands2)
					tkinsert(tl,"end",bands[i])
				
			}
			
			tkselection.set(tl,0)
			
			OnOK <- function(){
				
				choice <- as.numeric(tkcurselection(tl))+1
				
				if (count==2){
					if ((aux==".1.1.1" && choice==visualBands1[1]) || (aux==".1.1.2" && choice==visualBands2[1])){
						tkmessageBox(title="Error",message="This band was already chosen! Please, select another band",icon="error",type="ok")
						return()
					}
				}
				
				if (count==3){
					if ((aux==".1.1.1" && choice==visualBands1[1]) || (aux==".1.1.2" && choice==visualBands2[1]) || (aux==".1.1.1" && choice==visualBands1[2]) || (aux==".1.1.2" && choice==visualBands2[2])){
						tkmessageBox(title="Error",message="This band was already chosen! Please, select another band",icon="error",type="ok")
						return()
					}
				}
				if (aux==".1.1.1") visualBands1 <<- c(visualBands1,choice)
				if (aux==".1.1.2") visualBands2 <<- c(visualBands2,choice)
				tkdestroy(ttregions)
				if (count!=3) build(count+1)
				else{
					if (aux==".1.1.1"){
						tkdestroy(Frame3)
						Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
						tkconfigure(Frame3,width=485,height=510)
						tkplace(Frame3,x=510,y=30)
						
						Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
						tkplace(Frame4,x=1,y=1)
						
						tkdestroy(Frame1)
						Frame1 <- tkframe(lb1,relief="groove",borderwidth=2)
						tkconfigure(Frame1,width=485,height=510)
						tkplace(Frame1,x=10,y=30)
						
						Frame2 <- tkframe(Frame1,relief="groove",borderwidth=0)
						tkplace(Frame2,x=1,y=1)
						
						auximg <- array(c(img1[,,visualBands1[1]],img1[,,visualBands1[2]],img1[,,visualBands1[3]]),c(nrow(img1),ncol(img1),numbands1))
						
						image <-tkrplot(Frame2, function() plot.imagematrix(imagematrix(auximg,type="rgb")),vscale=1.04,hscale=0.98)
						tkpack(image)
						auximg <- array(c(img2[,,visualBands1[1]],img2[,,visualBands1[2]],img2[,,visualBands1[3]]),c(nrow(img2),ncol(img2),numbands1))
						image <-tkrplot(Frame4, function() plot.imagematrix(imagematrix(auximg,type="rgb")),vscale=1.04,hscale=0.98)
						tkpack(image)
					}
					if (aux==".1.1.2"){
						regionsList<<-list()
						regionsListAux<<-list()
						
						tkconfigure(statisticsTxt,state="normal")
						tkdelete(statisticsTxt,"1.0","end")
						tkconfigure(statisticsTxt,state="disable")
						
						tkdestroy(Frame5)
					
						Frame5 <- tkframe(lb2,relief="groove",borderwidth=2)
						tkconfigure(Frame5,width=485,height=510)
						tkplace(Frame5,x=10,y=30)
				
						Frame6 <- tkframe(Frame5,relief="groove",borderwidth=0)
						tkplace(Frame6,x=1,y=1)
						
						auximg <- array(c(img3[,,visualBands2[1]],img3[,,visualBands2[2]],img3[,,visualBands2[3]]),c(nrow(img3),ncol(img3),numbands2))
						
						image <-tkrplot(Frame6, function() plot.imagematrix(imagematrix(auximg,type="rgb",noclipping=TRUE)),vscale=1.04,hscale=0.98)
						tkpack(image)
					}
				}
			}
			
			OK.but <-tkbutton(ttregions,text="   OK   ",command=OnOK)
			tkgrid(OK.but)
			tkfocus(ttregions)
		}
		build(1)
	}
	#
	
	# Function for the brightness and contrast slider
	sliderFunction <- function(){
	
		aux <- tclvalue(tcl(tn,"select"))

		if ((is.null(img1) && aux==".1.1.1") || (is.null(img3) && aux==".1.1.2")){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
	
		cont <- (as.numeric(tclvalue(SliderValue2)))
		bri <- as.numeric(tclvalue(SliderValue))
		
		img2 <<- contBriImg(img1,cont,bri)
		
		tkdestroy(Frame3)	
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
		
		if (numbands1>3){
			auximg <- array(c(img2[,,visualBands1[1]],img2[,,visualBands1[2]],img2[,,visualBands1[3]]),c(nrow(img2),ncol(img2),numbands1))
			image <-tkrplot(Frame4, function() plot(imagematrix(auximg)),vscale=1.04,hscale=0.98)
		}else{
			image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
		}
		tkpack(image)
	}
	#
	
	# Function to quit the interface
	quit <- function(){
		#rm(img,img2)
		tkdestroy(tt)
	}
	#
	
	# Function to show the actual pixel value of the mouse
	pixelsValues <- function(){
		plotFunction1 <- function(){
			params <- par(bg="white")
			
			aux <- tclvalue(tcl(tn,"select"))
			
			if (aux==".1.1.1" && numbands1>3){
				auximg <- array(c(img1[,,visualBands1[1]],img1[,,visualBands1[2]],img1[,,visualBands1[3]]),c(nrow(img1),ncol(img1),numbands1))
				plot(imagematrix(auximg))
			}
			
			if (aux==".1.1.2" && numbands2>3){
				auximg <- array(c(img3[,,visualBands2[1]],img3[,,visualBands2[2]],img3[,,visualBands2[3]]),c(nrow(img3),ncol(img3),numbands2))
				plot(imagematrix(auximg))
			}
			if (aux==".1.1.1" && numbands1<=3)
				plot(imagematrix(img1))
			if (aux==".1.1.2" && numbands2<=3)
				plot(imagematrix(img3))
			
			parPlotSize1 <<- par("plt")
			usrCoords1   <<- par("usr")
			par(params)
		}
		
		OnLeftClick <- function(x,y){
			xClick <- x
			yClick <- y
			width  <- as.numeric(tclvalue(tkwinfo("reqwidth",imgtmp)))
			height <- as.numeric(tclvalue(tkwinfo("reqheight",imgtmp)))
			
			xMin <- parPlotSize1[1] * width
			xMax <- parPlotSize1[2] * width
			yMin <- parPlotSize1[3] * height
			yMax <- parPlotSize1[4] * height
			
			rangeX <- usrCoords1[2] - usrCoords1[1]
			rangeY <- usrCoords1[4] - usrCoords1[3]
			
			imgXcoords <- (xCoords1-usrCoords1[1])*(xMax-xMin)/rangeX + xMin
			imgYcoords <- (yCoords1-usrCoords1[3])*(yMax-yMin)/rangeY + yMin
			
			xClick <- as.numeric(xClick)+0.5
			yClick <- as.numeric(yClick)+0.5
			yClick <- height - yClick
			
			xPlotCoord <- usrCoords1[1]+(xClick-xMin)*rangeX/(xMax-xMin)
			yPlotCoord <- usrCoords1[3]+(yClick-yMin)*rangeY/(yMax-yMin)
			
			a <- round(xPlotCoord)
			b <- round(nrow(img1) - yPlotCoord)
			
			aux <- tclvalue(tcl(tn,"select"))
			
			if (aux==".1.1.1"){
				if (a<0 || b<0 || a>ncol(img1) || b>nrow(img1)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
				else{
					aux2 <- NULL
					for (i in 1:numbands1){
						if (is.null(AVIRISbands1)) aux2 <- paste(aux2,"Band ",i,": ",img1[b,a,i],"\n",sep="")
						else aux2 <- paste(aux2,"Band ",AVIRISbands1[i],": ",img1[b,a,i],"\n",sep="")
					}
					tkmessageBox(title="Value",message=aux2,icon="info",type="ok")
				}
			}
			if (aux==".1.1.2"){
				if (a<0 || b<0 || a>ncol(img3) || b>nrow(img3)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
				else{
					aux2 <- NULL
					for (i in 1:numbands2){
						if (is.null(AVIRISbands2)) aux2 <- paste(aux2,"Band",i,": ",img3[b,a,i],"\n",sep="")
						else aux2 <- paste(aux2,"Band",AVIRISbands2[i],": ",img3[b,a,i],"\n",sep="")
					}
					tkmessageBox(title="Value",message=aux2,icon="info",type="ok")
				}
			}
		}
		
		aux <- tclvalue(tcl(tn,"select"))
		
		if ((aux==".1.1.1" && is.null(img1)) || (aux==".1.1.2" && is.null(img3))){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		ttpixels <- tktoplevel()
		tkwm.geometry(ttpixels,"+150+0")
		tkwm.title(ttpixels,"Click on a point to get the pixel value")
		
		if (aux==".1.1.1"){
			xCoords1<<-(1:ncol(img1))
			yCoords1<<-(1:nrow(img1))
		}
		if (aux==".1.1.2"){
			xCoords1<<-(1:ncol(img3))
			yCoords1<<-(1:nrow(img3))
		}
		
		parPlotSize1 <- c()
		usrCoords1 <- c()
	
		imgtmp <<- tkrplot(ttpixels,fun=plotFunction1,hscale=1.5,vscale=1.5)
		tkgrid(imgtmp)
	
		tkbind(imgtmp, "<Button-1>",OnLeftClick)
		tkconfigure(imgtmp,cursor="hand2")
	}
	#
	
	# Function to edit pixels values
	editPixels <- function(){
		aux <- tclvalue(tcl(tn,"select"))
	
		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this function only with the first tab!",icon="error",type="ok")
			return()
		}
		
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, select an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		ttbands<-tktoplevel()
		tkwm.geometry(ttbands,"+400+700")
		tkwm.resizable(ttbands,0,0)
		tktitle(ttbands)<-"Bands"
		
		scr <- tkscrollbar(ttbands, repeatinterval=5, command=function(...)tkyview(tl,...))
		tl<-tklistbox(ttbands,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		tkgrid(tklabel(ttbands,text="Choose the band"))
		tkgrid(tl,scr)
		tkgrid.configure(scr,rowspan=6,sticky="nsw")
		
		bands<-NULL
		
		for (i in 1:numbands1){
			if (is.null(AVIRISbands1)) bands <- c(bands,paste("Band ",i,sep=""))
			else bands <- c(bands,paste("Band ",AVIRISbands1[i],sep=""))
		}
		
		for (i in (1:numbands1)){
			tkinsert(tl,"end",bands[i])
		}
		tkselection.set(tl,0)
		
		OnOK <- function(){
			
			onok <- function(){
				for (i in 1:nrow(img1)){
					for (j in 1:ncol(img1)){
						img2[i,j,bandChoice] <- as.numeric(tclvalue(myarray[[i,j]]))
					}
				}
				
				tkdestroy(Frame3)	
				Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
				tkconfigure(Frame3,width=485,height=510)
				tkplace(Frame3,x=510,y=30)
				Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
				tkplace(Frame4,x=1,y=1)
				
				if (numbands1>3){
					auximg <- array(c(img2[,,visualBands1[1]],img2[,,visualBands1[2]],img2[,,visualBands1[3]]),c(nrow(img2),ncol(img2),numbands1))
					image <-tkrplot(Frame4, function() plot(imagematrix(auximg)),vscale=1.04,hscale=0.98)
				}else{
					image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
				}
				tkpack(image)
			}
			
			bandChoice <- as.numeric(tkcurselection(tl))+1
			tkdestroy(ttbands)
			
			myarray <- tclArray()
			
			for (i in 1:ncol(img1)) myarray[[0,i]] <- i
			for (i in 1:nrow(img1)) myarray[[i,0]] <- i
			for (i in 1:nrow(img1)){
				for (j in 1:ncol(img1)){
					myarray[[i,j]] <- img1[i,j,bandChoice]
				}
			}
			
			tttable <- tktoplevel()
			tkwm.title(tttable,paste("Pixels Values (Band ",bandChoice,")",sep=""))
			tkwm.geometry(tttable,"+80+50")
			tkwm.resizable(tttable,0,0)
			table <- tkwidget(tttable,"table",rows=nrow(img1)+1,cols=ncol(img1)+1,titlerows=1,titlecols=1,colwidth=20,
				height=0,width=0,
				xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
			xscr <-tkscrollbar(tttable,orient="horizontal", command=function(...)tkxview(table,...))
			yscr <- tkscrollbar(tttable,command=function(...)tkyview(table,...))
			tkgrid(table,yscr)
			tkgrid.configure(yscr,sticky="nsw")
			tkgrid(xscr,sticky="new")
			tkconfigure(table,variable=myarray,background="white",selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="none")
			
			button <-tkbutton(tttable,text="   OK   ",command=onok)
			tkgrid(button)
			
			#tkconfigure(table, state="disabled")
		}
		
		OK.but <-tkbutton(ttbands,text="   OK   ",command=OnOK)
		tkgrid(OK.but)
		tkfocus(ttbands)
# 		plotFunction1 <- function(){
# 			params <- par(bg="white")
# 			
# 			aux <- .Tcl(paste(tn, "view"))
# 			aux <- as.integer(aux)
# 			
# 			if (aux==0 && numbands1>3){
# 				auximg <- array(c(img1[,,visualBands1[1]],img1[,,visualBands1[2]],img1[,,visualBands1[3]]),c(nrow(img1),ncol(img1),numbands1))
# 				plot(imagematrix(auximg))
# 			}
# 			
# 			if (aux==1 && numbands2>3){
# 				auximg <- array(c(img3[,,visualBands2[1]],img3[,,visualBands2[2]],img3[,,visualBands2[3]]),c(nrow(img3),ncol(img3),numbands2))
# 				plot(imagematrix(auximg))
# 			}
# 			if (aux==0 && numbands1<=3)
# 				plot(imagematrix(img1))
# 			if (aux==1 && numbands2<=3)
# 				plot(imagematrix(img3))
# 			
# 			parPlotSize1 <<- par("plt")
# 			usrCoords1   <<- par("usr")
# 			par(params)
# 		}
# 		
# 		OnLeftClick <- function(x,y){
# 			xClick <- x
# 			yClick <- y
# 			width  <- as.numeric(tclvalue(tkwinfo("reqwidth",imgtmp)))
# 			height <- as.numeric(tclvalue(tkwinfo("reqheight",imgtmp)))
# 			
# 			xMin <- parPlotSize1[1] * width
# 			xMax <- parPlotSize1[2] * width
# 			yMin <- parPlotSize1[3] * height
# 			yMax <- parPlotSize1[4] * height
# 			
# 			rangeX <- usrCoords1[2] - usrCoords1[1]
# 			rangeY <- usrCoords1[4] - usrCoords1[3]
# 			
# 			imgXcoords <- (xCoords1-usrCoords1[1])*(xMax-xMin)/rangeX + xMin
# 			imgYcoords <- (yCoords1-usrCoords1[3])*(yMax-yMin)/rangeY + yMin
# 			
# 			xClick <- as.numeric(xClick)+0.5
# 			yClick <- as.numeric(yClick)+0.5
# 			yClick <- height - yClick
# 			
# 			xPlotCoord <- usrCoords1[1]+(xClick-xMin)*rangeX/(xMax-xMin)
# 			yPlotCoord <- usrCoords1[3]+(yClick-yMin)*rangeY/(yMax-yMin)
# 			
# 			a <- round(xPlotCoord)
# 			b <- round(nrow(img1) - yPlotCoord)
# 			
# 			aux <- .Tcl(paste(tn, "view"))
# 			aux <- as.integer(aux)
# 			
# 			if (aux==0){
# 				if (a<0 || b<0 || a>ncol(img1) || b>nrow(img1)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
# 				else{
# 					for (i in 1:numbands1){
# 						returnVal <- modalDialog(paste("Pixel value (Band ",i,")",sep=""),"Enter the new value",img1[b,a,i])
# 						if (returnVal=="ID_CANCEL")
# 							return()
# 						val <- as.double(returnVal)
# 						img1[b,a,i] <<- val
# 					}
# 				}
# 			}
# 			if (aux==1){
# 				if (a<0 || b<0 || a>ncol(img3) || b>nrow(img3)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
# 				else{
# 					for (i in 1:numbands2){
# 						returnVal <- modalDialog(paste("Pixel value (Band ",i,")",sep=""),"Enter the new value",img3[b,a,i])
# 						if (returnVal=="ID_CANCEL")
# 							return()
# 						val <- as.double(returnVal)
# 						img3[b,a,i] <<- val
# 					}
# 				}
# 			}
# 			tkdestroy(imgtmp)
# 			imgtmp <<- tkrplot(ttpixels,fun=plotFunction1,hscale=1.5,vscale=1.5)
# 			tkgrid(imgtmp)
# 	
# 			tkbind(imgtmp, "<Button-1>",OnLeftClick)
# 			tkconfigure(imgtmp,cursor="hand2")
# 		}
# 		
# 		aux <- .Tcl(paste(tn, "view"))
# 		aux <- as.integer(aux)
# 		
# 		if ((aux==0 && is.null(img1)) || (aux==1 && is.null(img3))){
# 			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
# 			return()
# 		}
# 		
# 		ttpixels <- tktoplevel()
# 		tkwm.geometry(ttpixels,"+150+0")
# 		tkwm.title(ttpixels,"Click on a point to edit the pixel value")
# 		
# 		if (aux==0){
# 			xCoords1<<-(1:ncol(img1))
# 			yCoords1<<-(1:nrow(img1))
# 		}
# 		if (aux==1){
# 			xCoords1<<-(1:ncol(img3))
# 			yCoords1<<-(1:nrow(img3))
# 		}
# 		
# 		parPlotSize1 <- c()
# 		usrCoords1 <- c()
# 	
# 		imgtmp <<- tkrplot(ttpixels,fun=plotFunction1,hscale=1.5,vscale=1.5)
# 		tkgrid(imgtmp)
# 	
# 		tkbind(imgtmp, "<Button-1>",OnLeftClick)
# 		tkconfigure(imgtmp,cursor="hand2")
	}
	#
	
	# Function to buils dynamic graphics from GGOBI
	dynFunc <- function(func){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.1"){
			tkmessageBox(title="Error",message="Please, use this menu only with the second tab!",icon="error",type="ok")
			return()
		}
		
		if (is.null(img3)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		if (numbands2==1){
			tkmessageBox(title="Error",message="Graphics available only for images with more than one band!",icon="error",type="ok")
			return()
		}
		
		if (length(regionsList)==0){
			tkmessageBox(title="Error",message="There are not regions!",icon="error",type="ok")
			return()
		}
		
		names <- vector()
		vars <- vector()
		dataf <- vector()
		if (func!=9){
			for (i in 1:numbands2){
				auxVector <- vector()
				for (j in 1:length(regionsList)){
					if (func==1) auxVector<-c(auxVector,min(regionsList[[j]][[2]][[i]]))
					if (func==2) auxVector<-c(auxVector,max(regionsList[[j]][[2]][[i]]))
					if (func==3) auxVector<-c(auxVector,mean(regionsList[[j]][[2]][[i]]))
					if (func==4) auxVector<-c(auxVector,median(regionsList[[j]][[2]][[i]]))
					if (func==5) auxVector<-c(auxVector,sd(regionsList[[j]][[2]][[i]]))
					if (func==6) auxVector<-c(auxVector,mad(regionsList[[j]][[2]][[i]]))
					if (func==7) auxVector<-c(auxVector,kurtosis(regionsList[[j]][[2]][[i]]))
					if (func==8) auxVector<-c(auxVector,skewness(regionsList[[j]][[2]][[i]]))
				}
				if (is.null(AVIRISbands2)){
					assign(paste("Band",i,sep=""),auxVector)
					names <- c(names,paste("Band",i,sep=""))
				} else{
					assign(paste("Band",AVIRISbands2[i],sep=""),auxVector)
					names <- c(names,paste("Band",AVIRISbands2[i],sep=""))
				}
				vars <- c(vars,i)
			}
		}else{
			for (i in 1:numbands2){
				auxVector<-regionsList[[1]][[2]][[i]]
				if (is.null(AVIRISbands2)){
					assign(paste("Band",i,sep=""),auxVector)
					names <- c(names,paste("Band",i,sep=""))
				} else{
					assign(paste("Band",AVIRISbands2[i],sep=""),auxVector)
					names <- c(names,paste("Band",AVIRISbands2[i],sep=""))
				}
				vars <- c(vars,i)
			}
		}
		print(auxVector)
		for (i in 1:numbands2){
			if (i==1){
				if (is.null(AVIRISbands2)) dataf <- data.frame(get(paste("Band",i,sep="")))
				else dataf <- data.frame(get(paste("Band",AVIRISbands2[i],sep="")))
			}
			else{
				if (is.null(AVIRISbands2)) dataf[,i] <- get(paste("Band",i,sep=""))
				else dataf[,i] <- get(paste("Band",AVIRISbands2[i],sep=""))
			}
		}
		
		names(dataf) <- names
		g <- ggobi(dataf)		
		d <- display(g[1], "Parallel Coordinates Display")
	}
	#
	
	# Function to show all regions
	showAllRegions <- function(){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.1"){
			tkmessageBox(title="Error",message="Please, use this menu only with the second tab!",icon="error",type="ok")
			return()
		}
	
		if (length(regionsList)==0){
			tkmessageBox(title="Error",message="There are not regions",icon="error",type="ok")
			return()
		}
		for (i in 1:length(regionsList)){
			ttregion <- tktoplevel()
			tkwm.resizable(ttregion,0,0)
			tktitle(ttregion)<-paste("Region",i)
			
			if (numbands2>3){
				auximg <- array(c(regionsList[[i]][[3]][,,visualBands2[1]],regionsList[[i]][[3]][,,visualBands2[2]],regionsList[[i]][[3]][,,visualBands2[3]]),c(nrow(regionsList[[i]][[3]]),ncol(regionsList[[i]][[3]]),numbands2))
				cRegion <-tkrplot(ttregion, function() plot(imagematrix(auximg)),vscale=0.8,hscale=0.8)
			}
			if (numbands2<=3){
				cRegion <-tkrplot(ttregion, function() plot(imagematrix(regionsList[[i]][[3]])),vscale=0.8,hscale=0.8)
			}
			tkpack(cRegion)
		}
	}
	#
	
	pca <- function(){
		aux <- tclvalue(tcl(tn,"select"))
		
		if ((aux==".1.1.1" && is.null(img1)) || (aux==".1.1.2" && is.null(img3))){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		data <- matrix(nrow=nrow(img1[,,1])*ncol(img1[,,1]),ncol=numbands1)
		if (aux==".1.1.1"){
			for (i in 1:numbands1){
				data[,i] <- as.vector(t(img1[,,i]))
			}
		}
		result <- princomp(data)
		for (i in 1:numbands1){
			assign(paste("pca",i,sep=""),matrix(result[[6]][,i],nrow=nrow(img1[,,1]),ncol=ncol(img1[,,1]),byrow=T),env = .GlobalEnv)
		}
		for (i in 1:numbands1){
			assign(paste("ttpca",i,sep=""),tktoplevel())
			tkwm.title(get(paste("ttpca",i,sep="")),paste("PCA ",i,sep=""))
			image <-tkrplot(get(paste("ttpca",i,sep="")), function() plot(imagematrix(stretchImg(get(paste("pca",i,sep=""))))),vscale=0.7,hscale=0.7)
			tkpack(image)
		}
		#plot(imagematrix(pca1))
	}

	# Function to apply a zoom to an image
	zoomImg <- function(){
		aux <- tclvalue(tcl(tn,"select"))
		
		if ((aux==".1.1.1" && is.null(img1)) || (aux==".1.1.2" && is.null(img3))){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}

		if (aux==".1.1.1"){
			if (imageType1=="lan"){
				auximg <- array(c(img1[,,visualBands1[1]],img1[,,visualBands1[2]],img1[,,visualBands1[3]]),c(nrow(img1),ncol(img1),numbands1))
				plot(imagematrix(auximg))
				zoom_jpgRGB(auximg)
			}

			if (imageType1=="jpgRGB"){
				plot(imagematrix(img1))
				zoom_jpgRGB(img1)
			}
			
			if (imageType1=="jpgGrey"){
				plot(imagematrix(img1))
				zoom_jpgGrey(img1)
			}
				
			if (!is.null(AVIRISbands1)){
				plot(bands[[1]],bands[[2]],bands[[3]])
				zoom(bands[[1]],bands[[2]],bands[[3]])
			}
		}

		if (aux==".1.1.2"){
			if (imageType2=="lan"){
				auximg <- array(c(img3[,,visualBands2[1]],img3[,,visualBands2[2]],img3[,,visualBands2[3]]),c(nrow(img3),ncol(img3),numbands2))
				plot(imagematrix(auximg))
				zoom_jpgRGB(auximg)
			}

			if (imageType2=="jpgRGB"){
				plot(imagematrix(img3))
				zoom_jpgRGB(img3)
			}
			
			if (imageType2=="jpgGrey"){
				plot(imagematrix(img3))
				zoom_jpgGrey(img3)
			}
				
			if (!is.null(AVIRISbands2)){
				plot(bands[[1]],bands[[2]],bands[[3]])
				zoom(bands[[1]],bands[[2]],bands[[3]])
			}
		}

	}

	zoom_jpgRGB <- function(image){
		pos <- locator(2)
		pos$x <- round(pos$x)
		pos$y <- round(pos$y)
		pos$y[1] = dim(image)[1] - pos$y[1]
		pos$y[2] = dim(image)[1] - pos$y[2]

		#rect(pos$x[1],pos$y[1],pos$x[2],pos$y[2],col="red",density=0.1)
		plot(imagematrix(image[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2],]))
	}

	zoom_jpgGrey <- function(image){
		pos <- locator(2)
		pos$x <- round(pos$x)
		pos$y <- round(pos$y)
		pos$y[1] = dim(image)[1] - pos$y[1]
		pos$y[2] = dim(image)[1] - pos$y[2]

		#rect(pos$x[1],pos$y[1],pos$x[2],pos$y[2],col="red",density=0.1)
		plot(imagematrix(image[pos$y[1]:pos$y[2],pos$x[1]:pos$x[2],]))
	}

	# Function to equalize the image histogram
	equalizeImg <- function(){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
	
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		tkdestroy(Frame3)
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
		img2<<-img1
		for (i in 1:numbands1){
			img2[,,i] <<- equalize(img1[,,i])
		}
		image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
		tkpack(image)
	}
	#
	
	# Function to show the chosen region
	region <- function(){
		chooseRegion <- function(){
			plotFunction2 <- function(){
				params <- par(bg="white")
				if (numbands2>3){
					auximg <- array(c(img3[,,visualBands2[1]],img3[,,visualBands2[2]],img3[,,visualBands2[3]]),c(nrow(img3),ncol(img3),numbands2))
					plot(imagematrix(auximg))
				}else{
					plot(imagematrix(img3))
				}
				parPlotSize2 <<- par("plt")
				usrCoords2   <<- par("usr")
				par(params)
			}
		
			OnLeftClick2 <- function(x,y){
			
				xClick <- x
				yClick <- y
				width  <- as.numeric(tclvalue(tkwinfo("reqwidth",imgtmp2)))
				height <- as.numeric(tclvalue(tkwinfo("reqheight",imgtmp2)))
				
				xMin <- parPlotSize2[1] * width
				xMax <- parPlotSize2[2] * width
				yMin <- parPlotSize2[3] * height
				yMax <- parPlotSize2[4] * height
			
				rangeX <- usrCoords2[2] - usrCoords2[1]
				rangeY <- usrCoords2[4] - usrCoords2[3]
			
				imgXcoords <- (xCoords2-usrCoords2[1])*(xMax-xMin)/rangeX + xMin
				imgYcoords <- (yCoords2-usrCoords2[3])*(yMax-yMin)/rangeY + yMin
				
				xClick <- as.numeric(xClick)+0.5
				yClick <- as.numeric(yClick)+0.5
				yClick <- height - yClick
				
				xPlotCoord <- usrCoords2[1]+(xClick-xMin)*rangeX/(xMax-xMin)
				yPlotCoord <- usrCoords2[3]+(yClick-yMin)*rangeY/(yMax-yMin)
				
				a <- round(xPlotCoord)
				b <- round(nrow(img3) - yPlotCoord)
				
				if (a<0 || b<0 || a>ncol(img3) || b>nrow(img3)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
				else{
				regionPointsX <<- c(regionPointsX,a)
					regionPointsY <<- c(regionPointsY,b)
				}
			}
			
			findRegion <- function(){
				regionValues <- list()
				ttregion <- tktoplevel()
				tkwm.geometry(ttregion,"+0+0")
				tkwm.resizable(ttregion,0,0)
				tktitle(ttregion)<-paste("Region",length(regionsList)+1)
				n = as.integer(length(regionPointsX))
				out = .C("grahamMain",n,as.integer(regionPointsX),as.integer(regionPointsY),xvectorOut = as.integer(rep(0,n)),yvectorOut = as.integer(rep(0,n)),PACKAGE="ripa")
				aux=NULL
				enter=F
				x = out$xvectorOut
				y = out$yvectorOut
				for (i in 1:length(x)){
					if (x[i]==0){
						enter=T
						for (j in 1:(i-1)){
							aux = c(aux,x[j])
						}
					}
					if (enter==T) break
				}
				if (enter==T){
					x = aux
					aux=NULL
					enter=F
				}
				for (i in 1:length(y)){
					if (y[i]==0){
						enter=T
						for (j in 1:(i-1)){
							aux = c(aux,y[j])
						}
					}
					if (enter==T) break
				}
				if (enter==T) y = aux
				
				chosenRegion <- img3
				for (i in 1:numbands2){
					out <- .C("inpolyMain",as.integer(length(x)),as.integer(x),as.integer(y),outregion=as.double(as.vector(t(as.matrix(img3[,,i])))),as.integer(nrow(img3)),as.integer(ncol(img3)),rValues = as.double(rep(0,nrow(img3)*ncol(img3))),regionLength=as.integer(0),PACKAGE="ripa")
					chosenRegion[,,i] <- matrix(out$outregion,ncol=ncol(img3),byrow=T)
					regionValues[[i]] <- out$rValues[1:(out$regionLength-1)]
				}
				
				regionsList[[length(regionsList)+1]] <<- list(Type=regionType,Values=regionValues,Visual=chosenRegion)
					
				names(regionsList)[[length(regionsList)]] <<-paste("Region",length(regionsList),sep=" ")
				length(names(regionsList)) <<- length(regionsList)
				
				if (numbands2>3){
					auximg <- array(c(chosenRegion[,,visualBands2[1]],chosenRegion[,,visualBands2[2]],chosenRegion[,,visualBands2[3]]),c(nrow(chosenRegion),ncol(chosenRegion),numbands2))
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(auximg,noclipping=TRUE)),vscale=0.8,hscale=0.8)
					tkpack(cRegion)
				}else{
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(chosenRegion)),vscale=0.8,hscale=0.8)
					tkpack(cRegion)
				}
				
				showRegionStatistics(regionValues)
				if (exists("ttregionsbands1")) tkdestroy(ttregionsbands1)
				if (exists("ttregionsbands2")) tkdestroy(ttregionsbands2)
				#regionBand()
			}
			
			# Auxiliar function to print the statistical summary of the regions
			showRegionStatistics <- function(region){
				# Function to print the statistical summary of the regions
				printStatistcs <- function(nR,minR,maxR,meanR,medianR,deviationR,mDeviationR,kurtosisR,skewnessR){
					tkconfigure(statisticsTxt,state="normal")
					tkinsert(statisticsTxt,"end",paste("N = ",nR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Min = ",minR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Max = ",maxR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Mean = ",meanR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Median = ",medianR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Deviation = ",deviationR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Median Deviation = ",mDeviationR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Kurtosis = ",kurtosisR,"\n",sep=""))
					tkinsert(statisticsTxt,"end",paste("Skewness = ",skewnessR,"\n\n",sep=""))
					tkconfigure(statisticsTxt, state="disabled")
				}
				#
				
				for (i in 1:numbands2){
					nR <- length(region[[i]])
					minR <- min(region[[i]])
					maxR <- max(region[[i]])
					meanR <- mean(region[[i]])
					medianR <- median(region[[i]])
					deviationR <- sd(as.vector(region[[i]]))
					mDeviationR <- mad(region[[i]])
					kurtosisR <- kurtosis(as.vector(region[[i]]))
					skewnessR <- skewness(as.vector(region[[i]]))
					tkconfigure(statisticsTxt,state="normal")
					if (i==1) tkinsert(statisticsTxt,"end",paste(names(regionsList)[length(regionsList)],"\n"))
					if (is.null(AVIRISbands2)) tkinsert(statisticsTxt,"end",paste("Band",i,"\n"))
					else tkinsert(statisticsTxt,"end",paste("Band",AVIRISbands2[i],"\n"))
					printStatistcs(nR,minR,maxR,meanR,medianR,deviationR,mDeviationR,kurtosisR,skewnessR)
				}
			}
			#
			
			OnRightClick <- function(){
				tkdestroy(ttpoints)
				findRegion()
			}
			
			rectangleRegion <- function(){
				regionValues <- list()
				tkdestroy(ttpoints)
				
				ttregion <- tktoplevel()
				tkwm.geometry(ttregion,"+0+0")
				tkwm.resizable(ttregion,0,0)
				tktitle(ttregion)<-paste("Region",length(regionsList)+1)
				
				a<-1
				b<-regionPointsY[1]
				c<-regionPointsY[2]
				d<-nrow(img3)
				e<-1
				f<-regionPointsX[1]
				g<-regionPointsX[2]
				h<-ncol(img3)
				
				chosenRegion<-img3[-c(a:b,c:d),-c(e:f,g:h),]
				chosenRegion <- array(chosenRegion,dim=c(nrow(chosenRegion),ncol(chosenRegion),dim(img3)[3]))
				for (i in 1:numbands2){
					regionValues[[i]] <- as.vector(chosenRegion[,,i])
				}
				regionsList[[length(regionsList)+1]] <<- list(Type=regionType,Values=regionValues,Visual=chosenRegion)
				
				names(regionsList)[[length(regionsList)]] <<-paste("Region",length(regionsList),sep=" ")
				length(names(regionsList)) <<- length(regionsList)
				
				if (numbands2>3){
					auximg <- array(c(chosenRegion[,,visualBands2[1]],chosenRegion[,,visualBands2[2]],chosenRegion[,,visualBands2[3]]),c(nrow(chosenRegion),ncol(chosenRegion),numbands2))
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(auximg,noclipping=TRUE)),vscale=0.8,hscale=0.8)
					tkpack(cRegion)
				}else{
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(chosenRegion)),vscale=0.8,hscale=0.8)
					tkpack(cRegion)
				}
				
				showRegionStatistics(regionValues)
				
				if (exists("ttregionsbands1")) tkdestroy(ttregionsbands1)
				if (exists("ttregionsbands2")) tkdestroy(ttregionsbands2)
			}
			
			OnLeftClick3 <- function(x,y){
				xClick <- x
				yClick <- y
				width  <- as.numeric(tclvalue(tkwinfo("reqwidth",imgtmp2)))
				height <- as.numeric(tclvalue(tkwinfo("reqheight",imgtmp2)))
				
				xMin <- parPlotSize2[1] * width
				xMax <- parPlotSize2[2] * width
				yMin <- parPlotSize2[3] * height
				yMax <- parPlotSize2[4] * height
				
				rangeX <- usrCoords2[2] - usrCoords2[1]
				rangeY <- usrCoords2[4] - usrCoords2[3]
				
				imgXcoords <- (xCoords2-usrCoords2[1])*(xMax-xMin)/rangeX + xMin
				imgYcoords <- (yCoords2-usrCoords2[3])*(yMax-yMin)/rangeY + yMin
				
				xClick <- as.numeric(xClick)+0.5
				yClick <- as.numeric(yClick)+0.5
				yClick <- height - yClick
				
				xPlotCoord <- usrCoords2[1]+(xClick-xMin)*rangeX/(xMax-xMin)
				yPlotCoord <- usrCoords2[3]+(yClick-yMin)*rangeY/(yMax-yMin)
				
				a <- round(xPlotCoord)
				b <- round(nrow(img3) - yPlotCoord)
				
				if (a<0 || b<0 || a>ncol(img3) || b>nrow(img3)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
				else{
					regionPointsX <<- c(regionPointsX,a)
					regionPointsY <<- c(regionPointsY,b)
				}
				if (length(regionPointsX)==2) rectangleRegion()
			}
			
			circleRegion <- function(){
				tkdestroy(ttpoints)
				
				ttregion <- tktoplevel()
				tkwm.geometry(ttregion,"+0+0")
				tkwm.resizable(ttregion,0,0)
				tktitle(ttregion)<-paste("Region",length(regionsList)+1)
				
				x1<-as.integer(regionPointsX[1])
				x2<-as.integer(regionPointsX[2])
				y1<-as.integer(regionPointsY[1])
				y2<-as.integer(regionPointsY[2])
				
				regionValues <- list()
				chosenRegion<-img3
				for (i in 1:numbands2){
					out <- .C("inCircle",x1,x2,y1,y2,outregion=as.double(as.vector(t(as.matrix(img3[,,i])))),as.integer(nrow(img3)),as.integer(ncol(img3)),rValues = as.double(rep(0,nrow(img3)*ncol(img3))),regionLength=as.integer(0),PACKAGE="ripa")
					regionValues[[i]]<-out$rValues[1:(out$regionLength-1)]
					chosenRegion[,,i]<-matrix(out$outregion,nrow=nrow(img3),byrow=T)
				}

				regionsList[[length(regionsList)+1]] <<- list(Type=regionType,Values=regionValues,Visual=chosenRegion)
						
				names(regionsList)[[length(regionsList)]] <<-paste("Region",length(regionsList),sep=" ")
				length(names(regionsList)) <<- length(regionsList)
				
				if (numbands2>3){
					auximg <- array(c(chosenRegion[,,visualBands2[1]],chosenRegion[,,visualBands2[2]],chosenRegion[,,visualBands2[3]]),c(nrow(chosenRegion),ncol(chosenRegion),numbands2))
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(auximg,noclipping=TRUE)),vscale=0.8,hscale=0.81)
					tkpack(cRegion)
				}else{
					cRegion <-tkrplot(ttregion, function() plot(imagematrix(chosenRegion)),vscale=0.8,hscale=0.8)
					tkpack(cRegion)
				}
				
				showRegionStatistics(regionValues)
				
				if (exists("ttregionsbands1")) tkdestroy(ttregionsbands1)
				if (exists("ttregionsbands2")) tkdestroy(ttregionsbands2)
			}
			
			OnLeftClick4 <- function(x,y){
				xClick <- x
				yClick <- y
				width  <- as.numeric(tclvalue(tkwinfo("reqwidth",imgtmp2)))
				height <- as.numeric(tclvalue(tkwinfo("reqheight",imgtmp2)))
				
				xMin <- parPlotSize2[1] * width
				xMax <- parPlotSize2[2] * width
				yMin <- parPlotSize2[3] * height
				yMax <- parPlotSize2[4] * height
				
				rangeX <- usrCoords2[2] - usrCoords2[1]
				rangeY <- usrCoords2[4] - usrCoords2[3]
				
				imgXcoords <- (xCoords2-usrCoords2[1])*(xMax-xMin)/rangeX + xMin
				imgYcoords <- (yCoords2-usrCoords2[3])*(yMax-yMin)/rangeY + yMin
				
				xClick <- as.numeric(xClick)+0.5
				yClick <- as.numeric(yClick)+0.5
				yClick <- height - yClick
				
				xPlotCoord <- usrCoords2[1]+(xClick-xMin)*rangeX/(xMax-xMin)
				yPlotCoord <- usrCoords2[3]+(yClick-yMin)*rangeY/(yMax-yMin)
				
				a <- round(xPlotCoord)
				b <- round(nrow(img3) - yPlotCoord)
				
				if (a<0 || b<0 || a>ncol(img3) || b>nrow(img3)) tkmessageBox(title="Error",message="Please, click inside the image!",icon="error",type="ok")
				else{
					regionPointsX <<- c(regionPointsX,a)
					regionPointsY <<- c(regionPointsY,b)
				}
				if (length(regionPointsX)==2) circleRegion()
			}
			
			regionPointsX <<- NULL
			regionPointsY <<- NULL
			ttpoints <<- tktoplevel()
				
			xCoords2<<-(1:ncol(img3))
			yCoords2<<-(1:nrow(img3))
			parPlotSize2 <- c()
			usrCoords2 <- c()
			
			imgtmp2 <<- tkrplot(ttpoints,fun=plotFunction2,hscale=1.5,vscale=1.5)
			tkgrid(imgtmp2)
		
			if (regionType == "Free"){
				tkbind(imgtmp2, "<Button-1>",OnLeftClick2)
				tkbind(imgtmp2, "<Button-3>",OnRightClick)
				tkwm.title(ttpoints,"Choose the points clicking on the left button of the mouse")
			}
			if (regionType == "Rectangle"){
				tkbind(imgtmp2, "<Button-1>",OnLeftClick3)
				tkwm.title(ttpoints,"Choose the top left and the bottom right points")
			}
			if (regionType == "Circle"){
				tkbind(imgtmp2, "<Button-1>",OnLeftClick4)
				tkwm.title(ttpoints,"Choose the center and the radius of the circle")
			}
			tkconfigure(imgtmp2,cursor="hand2")
			
		}
		
		ttkindregions<-tktoplevel()
		tkwm.geometry(ttkindregions,"+400+250")
		tkwm.resizable(ttkindregions,0,0)
		tktitle(ttkindregions)<-"Regions"
		scr <- tkscrollbar(ttkindregions, repeatinterval=5, command=function(...)tkyview(tl,...))
		tl<-tklistbox(ttkindregions,height=6,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		tkgrid(tklabel(ttkindregions,text="Choose the kind of region"))
		tkgrid(tl,scr)
		tkgrid.configure(scr,rowspan=6,sticky="nsw")
		
		tkinsert(tl,"end","Rectangle")
		tkinsert(tl,"end","Circle")
		tkinsert(tl,"end","Free")
		tkselection.set(tl,0)
		
		OnOK <- function(){
			kindChoice <- as.numeric(tkcurselection(tl))+1
			if (kindChoice==1){
				tkdestroy(ttkindregions)
				regionType <<- "Rectangle"
				chooseRegion()
			}
			
			if (kindChoice==2){
				tkdestroy(ttkindregions)
				regionType <<- "Circle"
				chooseRegion()
			}
			
			if (kindChoice==3){
				tkdestroy(ttkindregions)
				regionType <<- "Free"
				chooseRegion()
			}
		}
		
		OK.but <-tkbutton(ttkindregions,text=   " OK ",command=OnOK)
		tkgrid(OK.but)
		tkfocus(ttkindregions)
	}

	# Linear contrast stretch function
	stretch <- function(){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
		
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		tkdestroy(Frame3)
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
		
		img2 <<- stretchImg(img1)
		
		if (numbands1==1){
			image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
			tkpack(image)
		}else{
			auximg <- array(c(img2[,,visualBands1[1]],img2[,,visualBands1[2]],img2[,,visualBands1[3]]),c(nrow(img2),ncol(img2),numbands1))
			image <-tkrplot(Frame4, function() plot(imagematrix(auximg)),vscale=1.04,hscale=0.98)
			tkpack(image)
		}
	}
	
	# Function to choose filters
	filters <- function(opt){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
	
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		if (opt==1){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(sobel(img1[,,i]))
			}
		}
		else if (opt==2){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(laplacian(img1[,,i]))
			}
		}
		else if (opt==3){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(meanImg(matrix(img1[,,1],nrow=nrow(img1))))
			}
		}
		else if (opt==4){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(highpass(img1[,,i]))
			}
		}
		else if (opt==5){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(lowpass(img1[,,i]))
			}
		}
		else if (opt==6){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(minImg(matrix(img1[,,1],nrow=nrow(img1))))
			}
		}
		else if (opt==7){
			img2 <<- img1
			for (i in 1:numbands1){
				img2[,,i] <<- normalize(maxImg(matrix(img1[,,1],nrow=nrow(img1))))
			}
		}
		else if (opt==8){
			ReturnVal <- modalDialog("Mask","Enter the mask length (3, 5, 7...)","")
			if (ReturnVal=="ID_CANCEL")
				return()
			lenMask <- as.integer(ReturnVal)
			img2<<-img1
			img2 <<- medianImg(img1,lenMask)
		}
		
		tkdestroy(Frame3)
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
		
		image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
		tkpack(image)
	}
	#
	
	# Function to choose segmentation techniques
	segmentation <- function(opt){
		# Function to apply thresholding to an image
		thresholdingImg <- function(img,value){
			return(thresholding(imagematrix(img),th=threshValue))
		}
		#
	
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
	
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		if (opt==1){       #thresholding
			ReturnVal <- modalDialog("Threshold","Enter the threshold value (0.0 - 1.0)","")
			if (ReturnVal=="ID_CANCEL")
				return()
			threshValue <- as.double(ReturnVal)
			img2<<-img1
			for (i in 1:numbands1){
				img2[,,i] <<- thresholdingImg(img1[,,i],threshValue)
			}
			img2 <<- imagematrix(img2)
		}
		
		tkdestroy(Frame3)
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
		
		image <-tkrplot(Frame4, function() plot(img2),vscale=1.04,hscale=0.98)
		tkpack(image)
	}
	#
	
	# Function to calculate the image index
	qualityIndex <- function(){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
	
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
	
		image_x <- as.double(as.vector(t(as.matrix(img1))))
		image_y <- as.double(as.vector(t(as.matrix(img2))))
		ncol <- as.integer(ncol(img1))
		nrow <- as.integer(nrow(img1))
		ReturnVal <- modalDialog("Window","Enter the window length (common value: 8)","")
		if (ReturnVal=="ID_CANCEL")
			return()
		tamWin <- as.integer(ReturnVal)
		out <- as.double(0.0)
		res <- .C("quality",image_x,image_y,ncol,nrow,tamWin,out,PACKAGE="ripa")
		index <- res[[6]]
		message <- paste("The quality index is:",index)
		tkmessageBox(title="Quality Index Result",message=message)
	}
	#
	
	# Function to save the transformed image
	saveImg <- function(){
		aux <- tclvalue(tcl(tn,"select"))
		
		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
		if (imageType1!="lan"){
			tkmessageBox(title="Error",message="This operation works only for LAN images.",icon="error",type="ok")
			return()
		}

		if (aux==".1.1.1"){
			if (imageType1=="lan"){
				saveLan()
			}
		}
		
		#name <- tkgetSaveFile(filetypes="{{JPEG Files} {.jpg .jpeg}}")
		#if (!nchar(tclvalue(name))){
		#	tkmessageBox(title="Error",message="No file was selected!")
		#	return()
		#}
		#name <- tclvalue(name)
		#jpeg(name)
		
		#if (aux==0) plot.imagematrix(img2)
		#if (aux==1) plot.imagematrix(img4)
		#dev.off()
	}
	#
	
	# Function to read AVIRIS images
	openAVIRIS <- function(){
		aux <- tclvalue(tcl(tn,"select"))

		if (aux==".1.1.1"){
			AVIRISbands1 <<- NULL
			.Tcl(paste(slider, "set",0))
			.Tcl(paste(slider2, "set",1))
	
			tkdestroy(Frame3)
			Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame3,width=485,height=510)
			tkplace(Frame3,x=510,y=30)
			
			Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
			tkplace(Frame4,x=1,y=1)
			
			tkdestroy(Frame1)
			Frame1 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame1,width=485,height=510)
			tkplace(Frame1,x=10,y=30)
			
			Frame2 <- tkframe(Frame1,relief="groove",borderwidth=0)
			tkplace(Frame2,x=1,y=1)
		}
		
		if (aux==".1.1.2"){
			AVIRISbands2 <<- NULL
			regionsList<<-list()
			regionsListAux<<-list()
			
			tkconfigure(statisticsTxt,state="normal")
			tkdelete(statisticsTxt,"1.0","end")
			tkconfigure(statisticsTxt,state="disable")
			
			tkdestroy(Frame5)
		
			Frame5 <- tkframe(lb2,relief="groove",borderwidth=2)
			tkconfigure(Frame5,width=485,height=510)
			tkplace(Frame5,x=10,y=30)
	
			Frame6 <- tkframe(Frame5,relief="groove",borderwidth=0)
			tkplace(Frame6,x=1,y=1)	
		}
		
		## Aqui come� parte com amostras das bandas
		bandsSet <<- c(rep(FALSE,224))
 		fileName <- tkgetOpenFile(filetypes="{{AVIRIS Files} {.a.log}}")
 		if (!nchar(tclvalue(fileName))){
 			tkmessageBox(message="No file was selected!")
 			return()
 		}
 		fileName <- tclvalue(fileName)
		fileName <- strsplit(as.character(fileName),".log")

		ttsamples <- tktoplevel()
		tktitle(ttsamples) <- "Bands Samples"
		sw <- tkwidget(ttsamples,"ScrolledWindow",relief="sunken",borderwidth=2)
		sf <- tkwidget(sw,"ScrollableFrame")
		tcl(sw,"setwidget",sf)
		subfID <- tclvalue(tcl(sf,"getframe"))
		lab <- tcl("label",paste(subfID,".lab",sep=""),text="Select the bands you want:")
		tkpack(sw,fill="both",expand="yes")
		tkgrid(lab,row=1,column=1)
		sampleList <<- list()
		OnClick <- function(W){
			band <- as.integer(strsplit(W,paste(subfID,".lab",sep=""))[[1]][2])
			if (bandsSet[band]==TRUE){
				tkmessageBox(title="Band number",message=paste("Band ",band," deselected!",sep=""),type="ok")
				bandsSet[band] <<- FALSE
			}else{
				tkmessageBox(title="Band number",message=paste("Band ",band," selected!",sep=""),type="ok")
				bandsSet[band] <<- TRUE
			}
		}
		r <- 2
		imagetmp <- limage(as.character(fileName),"reflectance")
		imagetmpScene2 <- lscene(imagetmp,2)
		pb <- tkProgressBar(title = "Generating samples...", min = 0, max = 224, width = 300)		
		for (i in (1:224))
		{
			## Aqui tem que pegar a amostra da banda i e colocar em img
			img <- clineal(lbandsample(imagetmpScene2,i)@data,0,1)
			if (is.nan(img[1])){
				for (j in 1:length(img)) img[j] <- 0
			}
			jpeg("imgtmp.jpg",width=200,height=200)
			plot(imagematrix(img))
			dev.off()
			image1 <- tclVar()
			tcl("image","create","photo",image1,file="imgtmp.jpg")
			sampleList[[i]] <- tcl("label",paste(subfID,".lab",i,sep=""),image=image1)
			if (i%%5 == 0){
				tkgrid(sampleList[[i-4]],row=r,column=1)
				tkgrid(sampleList[[i-3]],row=r,column=2)
				tkgrid(sampleList[[i-2]],row=r,column=3)
				tkgrid(sampleList[[i-1]],row=r,column=4)
				tkgrid(sampleList[[i]],row=r,column=5)
				r <- r+1
			}
			if (i==224){
				tkgrid(sampleList[[i-3]],row=r,column=1)
				tkgrid(sampleList[[i-2]],row=r,column=2)
				tkgrid(sampleList[[i-1]],row=r,column=3)
				tkgrid(sampleList[[i]],row=r,column=4)
			}
			tkbind(sampleList[[i]],"<FocusIn>",function() tcl(sf,"see",sampleList[[i]]))
			tkbind(sampleList[[i]], "<Button-1>",OnClick)
			setTkProgressBar(pb, i, label=paste(round(i/224*100, 0),"% done"))
		}
		## Aqui termina parte com amostras
		close(pb)

		onOK <- function(){
			tkdestroy(ttsamples)
			countbands <- 0
			
			for (i in 1:224){
				if (bandsSet[i]==TRUE){
					countbands <- countbands + 1
					if (aux==".1.1.1") AVIRISbands1 <<- c(AVIRISbands1,i)
					else AVIRISbands2 <<- c(AVIRISbands2,i)
				}
			}
			
			if (aux==".1.1.1"){
				img1 <<- read.aviris(as.character(fileName))
				image <-tkrplot(Frame2, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
 				tkpack(image)
 				image <-tkrplot(Frame4, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
 				tkpack(image)
 				img2 <<- img1
 				numbands1 <<- countbands
 				imageType1 <<- "jpgRGB"
			}
			if (aux==".1.1.2"){
				img3 <<- read.aviris(as.character(fileName))
				image <-tkrplot(Frame6, function() plot(imagematrix(img3)),vscale=1.04,hscale=0.98)
 				tkpack(image)
 				numbands2 <<- countbands
 				imageType2 <<- "jpgRGB"
			}
		}
		
		OK.but <- tkbutton(ttsamples,text="   OK   ",command=onOK)
		tkpack(OK.but)

	}
	
	# Function to read LAN images
	openLan <- function(){
		aux <- tclvalue(tcl(tn,"select"))
				
		if (aux==".1.1.1"){
			AVIRISbands1 <<- NULL
			.Tcl(paste(slider, "set",0))
			.Tcl(paste(slider2, "set",1))
	
			tkdestroy(Frame3)
			Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame3,width=485,height=510)
			tkplace(Frame3,x=510,y=30)
			
			Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
			tkplace(Frame4,x=1,y=1)
			
			tkdestroy(Frame1)
			Frame1 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame1,width=485,height=510)
			tkplace(Frame1,x=10,y=30)
			
			Frame2 <- tkframe(Frame1,relief="groove",borderwidth=0)
			tkplace(Frame2,x=1,y=1)
		}
		
		if (aux==".1.1.2"){
			AVIRISbands2 <<- NULL
			regionsList<<-list()
			regionsListAux<<-list()
			
			tkconfigure(statisticsTxt,state="normal")
			tkdelete(statisticsTxt,"1.0","end")
			tkconfigure(statisticsTxt,state="disable")
			
			tkdestroy(Frame5)
		
			Frame5 <- tkframe(lb2,relief="groove",borderwidth=2)
			tkconfigure(Frame5,width=485,height=510)
			tkplace(Frame5,x=10,y=30)
	
			Frame6 <- tkframe(Frame5,relief="groove",borderwidth=0)
			tkplace(Frame6,x=1,y=1)
			
			
		}
		
		arquivo <- tkgetOpenFile(filetypes="{{LAN Files} {.lan}}")
		if (!nchar(tclvalue(arquivo))){
			tkmessageBox(message="No file was selected!")
			return()
		}
		arquivo<-tclvalue(arquivo)
		
		if (aux==".1.1.1") img1 <<- read.lan(arquivo)
		if (aux==".1.1.2") img3 <<- read.lan(arquivo)
		
		if (aux==".1.1.1"){
			image <-tkrplot(Frame2, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
			tkpack(image)
			image <-tkrplot(Frame4, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
			tkpack(image)
			img2 <<- img1
			numbands1 <<- 7
			imageType1 <<- "lan"
		}
		
		if (aux==".1.1.2"){
			image <-tkrplot(Frame6, function() plot(imagematrix(img3)),vscale=1.04,hscale=0.98)
			tkpack(image)
			numbands2 <<- 7
			imageType2 <<- "lan"
		}
	}
	#
	
	# Function to write LAN images
	saveLan <- function(){
		aux <- tclvalue(tcl(tn,"select"))
		
		arquivo <- tkgetSaveFile(filetypes="{{LAN Files} {.lan}}")
		if (!nchar(tclvalue(arquivo))){
			tkmessageBox(message="No file was selected!")
			return()
		}
		arquivo<-tclvalue(arquivo)
		
		if (aux==".1.1.1") write.lan(arquivo,img2)
		#if (aux==".1.1.2") img3 <<- read.lan(arquivo)
	}
	#
	
	# Funciton to check tab and call openJpg function
	checkTab <- function(){
		
		# Function to read JPG images Tab 0
		openJpgTabOne <- function(){
			
			.Tcl(paste(slider, "set",0))
			.Tcl(paste(slider2, "set",1))
		
			tkdestroy(Frame3)
			
			Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame3,width=485,height=510)
			tkplace(Frame3,x=510,y=30)
			
			Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
			tkplace(Frame4,x=1,y=1)
			
			tkdestroy(Frame1)
			
			Frame1 <- tkframe(lb1,relief="groove",borderwidth=2)
			tkconfigure(Frame1,width=485,height=510)
			tkplace(Frame1,x=10,y=30)
		
			Frame2 <- tkframe(Frame1,relief="groove",borderwidth=0)
			tkplace(Frame2,x=1,y=1)
			
			arquivo <- tkgetOpenFile(filetypes="{{JPEG Files} {.jpg .jpeg}}")
			if (!nchar(tclvalue(arquivo))){
				tkmessageBox(title="Error",message="No file was selected!")
				return()
			}
			
			img1 <<- read.jpeg(arquivo)
			image <-tkrplot(Frame2, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
			tkpack(image)
			image <-tkrplot(Frame4, function() plot(imagematrix(img1)),vscale=1.04,hscale=0.98)
			tkpack(image)
			
			if (is.na(dim(img1)[3])){
				imageType1 <<- "jpgGrey"
				numbands1 <<- 1
				img1 <<- array(img1,dim=c(nrow(img1),ncol(img1),1))
				img2 <<- img1
			}
			else{
				imageType1 <<- "jpgRGB"
				numbands1 <<- 3
				img1 <<- array(img1,dim=dim(img1))
				img2 <<- img1
			}
		}
		#
		
		# Function to read jpeg image Tab 1
		openJpgTabTwo <- function(){
			
			regionsList<<-list()
			regionsListAux<<-list()
			
			tkconfigure(statisticsTxt,state="normal")
			tkdelete(statisticsTxt,"1.0","end")
			tkconfigure(statisticsTxt,state="disable")
			
			tkdestroy(Frame5)
			
			Frame5 <- tkframe(lb2,relief="groove",borderwidth=2)
			tkconfigure(Frame5,width=485,height=510)
			tkplace(Frame5,x=10,y=30)
		
			Frame6 <- tkframe(Frame5,relief="groove",borderwidth=0)
			tkplace(Frame6,x=1,y=1)
			
			arquivo <- tkgetOpenFile(filetypes="{{JPEG Files} {.jpg .jpeg}}")
			if (!nchar(tclvalue(arquivo))){
				tkmessageBox(message="No file was selected!")
				return()
			}
			
			img3 <<- read.jpeg(arquivo)
			image <-tkrplot(Frame6, function() plot.imagematrix(img3),vscale=1.04,hscale=0.98)
			tkpack(image)
			if (is.na(dim(img3)[3])){
				imageType2 <<- "jpgGrey"
				numbands2 <<- 1
				img3 <<- array(img3,dim=c(nrow(img3),ncol(img3),1))
			}
			else{
				imageType2 <<- "jpgRGB"
				numbands2 <<- 3
				img3 <<- array(img3,dim=dim(img3))
			}
		}
		#
		
		aux <- tclvalue(tcl(tn,"select"))
				
		if (aux==".1.1.1"){
			openJpgTabOne()
			AVIRISbands1 <<- NULL
		}
		if (aux==".1.1.2"){
			openJpgTabTwo()
			AVIRISbands2 <<- NULL
		}
	}
	#
	
	# Function to apply the negative function to an image
	negative <- function(){
		aux <- tclvalue(tcl(tn,"select"))
		
		if (aux==".1.1.2"){
			tkmessageBox(title="Error",message="Please, use this menu only with the first tab!",icon="error",type="ok")
			return()
		}
	
		if (is.null(img1)){
			tkmessageBox(title="Error",message="Please, open an image in order to use it!",icon="error",type="ok")
			return()
		}
		
		tkdestroy(Frame3)
		Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
		tkconfigure(Frame3,width=485,height=510)
		tkplace(Frame3,x=510,y=30)
		Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
		tkplace(Frame4,x=1,y=1)
	
		img2 <<- 1-img1
		
		if (numbands1>3){
			auximg <- array(c(img2[,,visualBands1[1]],img2[,,visualBands1[2]],img2[,,visualBands1[3]]),c(nrow(img2),ncol(img2),numbands1))
			image <-tkrplot(Frame4, function() plot(imagematrix(auximg)),vscale=1.04,hscale=0.98)
		}else{
			image <-tkrplot(Frame4, function() plot(imagematrix(img2)),vscale=1.04,hscale=0.98)
		}
		tkpack(image)
	}
	#

	showAbout <-function(){
		tkmessageBox(title="About",message="R Image Processing and Analysis\n Version 1.0\n\n Talita Perciano - IME/USP \n Alejandro C. Frery - IC/UFAL",icon="info",type="ok")
	}
	
	##################################################################################################################
	

	fontText <- tkfont.create(family="book",size=10)
	fr1 <- tkframe(tn)
	tkadd(tn,fr1,text="Operations")
	lb1 <- ttklabelframe(fr1)
	tkpack(lb1)
	tkconfigure(lb1,labelanchor="n",text="Operations Tab",width=1010,height=665)

	#OperationsTbn <<- tclvalue(tkadd(tn, label="Operations"))
	
	# Left frame
	Frame1 <- tkframe(lb1,relief="groove",borderwidth=2)
	tkconfigure(Frame1,width=485,height=510)
	tkplace(Frame1,x=10,y=30)
	#
	
	# Frame inside left frame
	Frame2 <- tkframe(Frame1,relief="groove",borderwidth=0)
	tkplace(Frame2,x=1,y=1)
	#
	
	# Right frame
	Frame3 <- tkframe(lb1,relief="groove",borderwidth=2)
	tkconfigure(Frame3,width=485,height=510)
	tkplace(Frame3,x=510,y=30)
	#
	
	# Frame inside right frame
	Frame4 <- tkframe(Frame3,relief="groove",borderwidth=0)
	tkplace(Frame4,x=1,y=1)
	#
	
	Label1 <- tklabel(lb1,text="Original image:")
	tkplace(Label1,x=10,y=10)
	
	Label2 <- tklabel(lb1,text="Changed image:")
	tkplace(Label2,x=510,y=10)
	
	SliderValue <- tclVar("0")
	SliderValueLabel <- tklabel(lb1,text=as.character(tclvalue(SliderValue)))
	Label3 <- tklabel(lb1,text="Brightness Value: ")
	tkplace(Label3,x=10,y=545)
	tkplace(SliderValueLabel,x=940,y=545)
	tkconfigure(SliderValueLabel,textvariable=SliderValue)
	slider <- tkscale(lb1, from=-1, to=1,
                   showvalue=F, variable=SliderValue,
                   resolution=0.01, orient="horizontal",length=810)
	tkplace(slider,x=120,y=545)
	
	tkbind(slider,"<ButtonRelease-1>",function() sliderFunction())
	
	SliderValue2 <- tclVar("0")
	SliderValueLabel2 <- tklabel(lb1,text=as.character(tclvalue(SliderValue2)))
	Label4 <- tklabel(lb1,text="Contrast Value: ")
	tkplace(Label4,x=10,y=575)
	tkplace(SliderValueLabel2,x=940,y=575)
	tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
	
	slider2 <-.Tk.subwin(lb1)
	slider2 <- tkscale(lb1, from=0, to=1,
                   showvalue=F, variable=SliderValue2,
                   resolution=0.01, orient="horizontal",length=810)
	tkplace(slider2,x=120,y=575)
	
	tkbind(slider2,"<ButtonRelease-1>",function() sliderFunction())
	
	##################################################################################################################
	
	##################################################################################################################
	####################################### ROI Tab ##################################################################
	##################################################################################################################
	fr2 <- tkframe(tn)
	tkadd(tn,fr2,text="Regions of Interest")
	lb2 <- ttklabelframe(fr2)
	tkpack(lb2)
	tkconfigure(lb2,labelanchor="n",text="Operations Tab",width=1010,height=665)
	
	# Left frame
	Frame5 <- tkframe(lb2,relief="groove",borderwidth=2)
	tkconfigure(Frame5,width=485,height=510)
	tkplace(Frame5,x=10,y=30)
	#
	
	# Frame inside left frame
	Frame6 <- tkframe(Frame5,relief="groove",borderwidth=0)
	tkplace(Frame6,x=1,y=1)
	#
	
	# Right frame
	Frame7 <- tkframe(lb2,relief="groove",borderwidth=2)
	tkconfigure(Frame7,width=485,height=510)
	tkplace(Frame7,x=510,y=30)
	#
	
	# Configure text space inside right frame
	scr <- tkscrollbar(Frame7, repeatinterval=5,command=function(...)tkyview(statisticsTxt,...))
	statisticsTxt <- tktext(Frame7,bg="white",font="courier",yscrollcommand=function(...)tkset(scr,...))
	tkplace(statisticsTxt,x=1,y=1,width=465,height=502)
	tkconfigure(statisticsTxt, state="disabled")
	tkplace(scr,x=468,y=1)
	tkplace.configure(scr,height=502)
	#
	
	Label3 <- tklabel(lb2,text="Original image:")
	tkplace(Label3,x=10,y=10)
	
	Label4 <- tklabel(lb2,text="Actual Region Statistics:")
	tkplace(Label4,x=510,y=10)
	
	Button1 <- tkbutton(lb2,text="Choose Region",command=region)
	tkplace(Button1,x=10,y=560)

	##################################################################################################################
	
	##################################################################################################################
	######################################### Menu ###################################################################
	topMenu<-tkmenu(tt)
	tkconfigure(tt,menu=topMenu)
	
	# File menu
	fileMenu<-tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Operations menu
	operationsMenu<-tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Regions Menu
	regionsMenu<-tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Bands Menu
	bandsMenu <- tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Help Menu
	helpMenu <- tkmenu(topMenu,tearoff=FALSE)
	#

	# Open menu inside File menu
	openMenu <- tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Filters menu inside Operations Menu
	filtersMenu <- tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Segmentation menu inside Operations Menu
	segmentationMenu <- tkmenu(topMenu,tearoff=FALSE)
	#
	
	# Configure Regions Menu
	tkadd(regionsMenu,"command",label="Show All Regions",command=showAllRegions,underline=0,accelerator="Ctrl+S")
	tkbind(tt,"<Control-s>",function() showAllRegions())
	tkadd(regionsMenu,"command",label="Covariance Matrix",command=covMatrix)
	tkadd(regionsMenu,"command",label="Regions Brushplots",command=regionBrush)
	#
	
	# Configure Bands Menu
	tkadd(bandsMenu,"command",label="Image Bands",command=imageBands,underline=0,accelerator="Ctrl+M")
	tkbind(tt,"<Control-m>",function() imageBands())
	tkadd(bandsMenu,"command",label="Regions Bands",command=regionBands,underline=0,accelerator="Ctrl+G")
	tkbind(tt,"<Control-g>",function() regionBands())
	tkadd(bandsMenu,"command",label="Bands Brushplots",command=brush)
	#
	
	# Configure Dynamic Graphics Menu
	dynGraph<-tkmenu(topMenu,tearoff=FALSE)
	tkadd(regionsMenu,"cascade",label="Dynamic Graphics",menu=dynGraph)
	tkadd(dynGraph,"command",label="Min",command=function() dynFunc(1),underline=1,accelerator="Ctrl+I")
	tkbind(tt,"<Control-i>",function() dynFunc(1))
	tkadd(dynGraph,"command",label="Max",command=function() dynFunc(2),underline=1,accelerator="Ctrl+A")
	tkbind(tt,"<Control-a>",function() dynFunc(2))
	tkadd(dynGraph,"command",label="Mean",command=function() dynFunc(3),underline=1,accelerator="Ctrl+E")
	tkbind(tt,"<Control-e>",function() dynFunc(3))
	tkadd(dynGraph,"command",label="Median",command=function() dynFunc(4),underline=2,accelerator="Ctrl+D")
	tkbind(tt,"<Control-d>",function() dynFunc(4))
	tkadd(dynGraph,"command",label="Standard Deviation",command=function() dynFunc(5),underline=1,accelerator="Ctrl+T")
	tkbind(tt,"<Control-t>",function() dynFunc(5))
	tkadd(dynGraph,"command",label="Median Deviation",command=function() dynFunc(6),underline=9,accelerator="Ctrl+V")
	tkbind(tt,"<Control-v>",function() dynFunc(6))
	tkadd(dynGraph,"command",label="Kurtosis",command=function() dynFunc(7),underline=0,accelerator="Ctrl+K")
	tkbind(tt,"<Control-k>",function() dynFunc(7))
	tkadd(dynGraph,"command",label="Skewness",command=function() dynFunc(8),underline=3,accelerator="Ctrl+W")
	tkbind(tt,"<Control-w>",function() dynFunc(8))
	tkadd(dynGraph,"command",label="All values",command=function() dynFunc(9))
	#
	
	# Configure Graphics Menu
	#graph<<-tkmenu(topMenu,tearoff=FALSE)
	#tkadd(regionsMenu,"cascade",label="Graphics",menu=graph)
	#tkadd(graph,"command",label="Histograms",command=function() histograms(),underline=0,accelerator="Ctrl+H")
	#tkbind(tt,"<Control-h>",function() histograms())
	#
	
	# Configure File menu
	tkadd(fileMenu,"cascade",label="Open Image",menu=openMenu)
	tkadd(fileMenu,"command",label="Change Visual Bands",command=function() changeBands(),underline=0,accelerator="Ctrl+C")
	tkbind(tt,"<Control-c>",function() changeBands())
	tkadd(fileMenu,"command",label="Save Image",command=saveImg,underline=0,accelerator="Ctrl+S")
	tkbind(tt,"<Control-s>",function() saveImg())
	tkadd(fileMenu,"command",label="Quit",command=function() quit(),underline=0,accelerator="Ctrl+Q")
	tkbind(tt,"<Control-q>",function() quit())
	#
	
	# Configure Operations menu
	tkadd(operationsMenu,"command",label="Stretch Image",command=stretch,underline=2,accelerator="Ctrl+R")
	tkbind(tt,"<Control-r>",function() stretch())
	tkadd(operationsMenu,"command",label="Negative", command=negative,underline=0,accelerator="Ctrl+N")
	tkbind(tt,"<Control-n>",function() negative())
	tkadd(operationsMenu,"command",label="Quality Index", command=qualityIndex,underline=8,accelerator="Ctrl+I")
	tkbind(tt,"<Control-i>",function() qualityIndex())
	tkadd(operationsMenu,"command",label="Equalize", command=equalizeImg,underline=0,accelerator="Ctrl+E")
	tkbind(tt,"<Control-e>",function() equalizeImg())
	#tkadd(operationsMenu,"command",label="Histograms",command=imgHistograms,underline=2,accelerator="Ctrl+G")
	#tkbind(tt,"<Control-g>",function() imgHistograms())
	tkadd(operationsMenu,"command",label="Pixels Values",command=pixelsValues,underline=2,accelerator="Ctrl+X")
	tkbind(tt,"<Control-x>",function() pixelsValues())
	tkadd(operationsMenu,"command",label="Edit Pixels Values",command=editPixels)
	tkadd(operationsMenu,"command",label="Zoom",command=zoomImg)
	tkadd(operationsMenu,"command",label="PCA",command=pca)
	#
	
	# To put Filters menu inside Operations menu
	tkadd(operationsMenu,"cascade",label="Filters",menu=filtersMenu)
	#
	
	# To put Segmentation menu inside Operations menu
	tkadd(operationsMenu,"cascade",label="Segmentation",menu=segmentationMenu)
	#
	
	# Image extensions inside Open menu
	tkadd(openMenu,"command",label="JPG",command=checkTab,underline=0,accelerator="Ctrl+J")
	tkbind(tt,"<Control-j>",function() checkTab())
	tkadd(openMenu,"command",label="LAN",command=openLan,underline=0,accelerator="Ctrl+L")
	tkbind(tt,"<Control-l>",function() openLan())
	tkadd(openMenu,"command",label="AVIRIS",command=openAVIRIS)
	#
	
	# Available filters inside Filters menu
	tkadd(filtersMenu,"command",label="Sobel Filter",command=function() filters(1),underline=2,accelerator="Ctrl+B")
	tkbind(tt,"<Control-b>",function() filters(1))
	tkadd(filtersMenu,"command",label="Laplacian Filter",command=function() filters(2),underline=2,accelerator="Ctrl+P")
	tkbind(tt,"<Control-p>",function() filters(2))
	tkadd(filtersMenu,"command",label="Mean Filter",command=function() filters(3),underline=0,accelerator="Ctrl+M")
	tkbind(tt,"<Control-m>",function() filters(3))
	tkadd(filtersMenu,"command",label="High-pass Filter",command=function() filters(4),underline=0,accelerator="Ctrl+H")
	tkbind(tt,"<Control-h>",function() filters(4))
	tkadd(filtersMenu,"command",label="Low-pass Filter",command=function() filters(5),underline=2,accelerator="Ctrl+W")
	tkbind(tt,"<Control-w>",function() filters(5))
	tkadd(filtersMenu,"command",label="Minimum Filter",command=function() filters(6))
	tkadd(filtersMenu,"command",label="Maximum Filter",command=function() filters(7))
	tkadd(filtersMenu,"command",label="Median Filter",command=function() filters(8))
	#
	
	# Available segmentation techniques inside Segmentation Menu
	tkadd(segmentationMenu, "command", label="Thresholding",command=function() segmentation(1))
	#
	
	# Configure Help menu
	tkadd(helpMenu,"command",label="About",command=function() showAbout(),underline=0)
	#

	# Insert Menus
	tkadd(topMenu,"cascade",label="File",menu=fileMenu,underline=0)
	tkadd(topMenu,"cascade",label="Operations",menu=operationsMenu,underline=0)
	tkadd(topMenu,"cascade",label="Regions",menu=regionsMenu,underline=0)
	tkadd(topMenu,"cascade",label="Bands",menu=bandsMenu,underline=0)
	tkadd(topMenu,"cascade",label="Help",menu=helpMenu,underline=0)
	#
	
	tkselect(tn,0)
	tkfocus(tt)
}
##################################################################################################################
