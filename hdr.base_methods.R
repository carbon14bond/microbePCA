# TODO: Add comment
# 
# Author: mhorto
###############################################################################

h <- function(){
	print(system("ls -lt | head"));
}

################################################################################
## Feb. 7, 2017
## my version of stack, which avoids factor generation
################################################################################
mstack <- function(arg, newHeaders, setRowNames=T, sorted=TRUE){
	values <- data.frame(names=I(names(arg)), values=as.numeric(arg));
	
	if( setRowNames ){
		rownames(values) <- values[,"names"];
	}
	
	if( sorted ) {
		values <- values[order(values[,"values"]),];	
	}
	
	colnames(values) <- newHeaders;
	return(values);
}

## round to the nearest by-argument, adopted from code found on R email list.
roundBy <- function(x, by=5){ 
	return( by * round(x / by)); 
}

trim <- function(str){
	return(gsub("^\\s+|\\s+$", "", str));
}

simpleCap <- function(x) {
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
}

###############################################################################
## helper function for chooseFile: this determines how many matches will be listed
###############################################################################
getNumberOfChoicesForChooseFile <- function(pattern){
	
	if( is.null(pattern) | pattern[1] == "" ){
		files <- list.files(path=".", pattern="txt$|Robj$");
		
	} else {
		files <- list.files(path=".", pattern=pattern[1]);
		
		if( length(pattern) > 1 ){
			for( k in 2:length(pattern)){
				files <- files[grep(pattern[k], files)];
			}
		}
	}

	return(c(number_of_pattern_matches=length(files)));
}

###############################################################################
## open a text file or Robject. ;)
###############################################################################
chooseFile <- function(pattern){

	if( is.null(pattern) | pattern[1] == "" ){
		files <- list.files(path=".", pattern="txt$|Robj$");

	} else {
		files <- list.files(path=".", pattern=pattern[1]);

		if( length(pattern) > 1 ){
			for( k in 2:length(pattern)){
				files <- files[grep(pattern[k], files)];
			}
		}
	}

	fileChoices <- paste(1:length(files), files, sep=": ");

	while( TRUE ){
		cat("Available files:\n");
		print(fileChoices);

		fileNumber <- readline("Please choose a file ");
		if( fileNumber == "" ){
			cat("You didn't choose anything.\n");

		} else {

			fileNumber <- as.numeric(fileNumber);
			if( fileNumber >= 1 | fileNumber <= length(files)){
				cat("You chose file:", files[fileNumber], "\n");
				break;

			} else {
				cat("Please choose a real file.\n");
			}
		}
	}

	fileName <- files[fileNumber];
	if( length(grep(".txt$", fileName)) > 0 ){
		fileType <- "text";

	} else if( length(grep( ".Robj$", fileName)) > 0 ){
		fileType <- "object";
		
	} else {
		fileType <- NA;
	}

	return(list("base_dir"=getwd(), "filename"=files[fileNumber], "filetype"=fileType));
};

chooseFiles <- function(pattern, numberOfChoices=1){
	
	if( pattern[1] != "" ){
		files <- list.files(path=".", pattern=pattern[1]);
		if( length(pattern) > 1 ){
			for( k in 2:length(pattern)){
				files <- files[grep(pattern[k], files)];
			}
		}
		
	} else {
		files <- list.files(path=".", pattern="txt$");
	}

	cat(getwd(), "\n");
	fileChoices <- paste(1:length(files), files, sep=": ");
	indices <- c();
	
	while( TRUE ){
		cat("Available files:\n");
		print(fileChoices);
		
		fileNumber <- readline("Please choose a file ");
		if( fileNumber == "" ){
			cat("You didn't choose anything.\n");
			
		} else {
			
			fileNumber <- as.numeric(fileNumber);
			if( fileNumber >= 1 | fileNumber <= length(files)){
				cat("You chose file:", files[fileNumber], "\n");
				indices <- c(indices, fileNumber);
				if( length(indices) == numberOfChoices ){
					break;	
				}
				
			} else {
				cat("Please choose a real file.\n");
			}
		}
	}
	
	return(list("base_dir"=dirname(getwd()), "filename"=files[indices]));
	
};

simpleThreeD <- function( dataset, eigenvectors=c("PC1", "PC2", "PC3"), userPhi, userTheta, in3d=TRUE){
	
	require(plot3D); ## if necessary, load it.
	dataset1 <- data.frame(id=as.numeric(rownames(dataset)), dataset, 
			jit_x=jitter(dataset[,eigenvectors[1]]), 
			jit_y=jitter(dataset[,eigenvectors[2]]), 
			jit_z=jitter(dataset[,eigenvectors[3]]), stringsAsFactors=FALSE);
	
	xRange <- range(dataset1[,"jit_x"]);
	yRange <- range(dataset1[,"jit_y"]); #, range(dataset[,"jit2_y"])));
	zRange <- range(dataset1[,"jit_z"]); #, range(dataset[,"jit2_z"])));
	
	if( in3d ){
		points3D(dataset1[,"jit_x"], dataset1[,"jit_y"], dataset1[,"jit_z"], bg="cadetblue1", pch=21, col="cadetblue", phi=userPhi, theta=userTheta, cex=0.75, xlab=eigenvectors[1], ylab=eigenvectors[2], zlab=eigenvectors[3]);
		
	} else {
		points2D(dataset1[,"jit_x"], dataset1[,"jit_y"], bg="cadetblue1", pch=21, col="cadetblue", cex=0.75, xlab=eigenvectors[1], ylab=eigenvectors[2]); 
	}
}

plotEigOverlap <- function( dataset1, dataset2, eigenvectors=c("PC1", "PC2", "PC3"), legendLabels=c("test1", "test2"), legendLocation=c(0.05, -0.05), in3d=TRUE){
	
	require(plot3D); ## if necessary, load it.
	dataset1 <- data.frame(id=as.numeric(rownames(dataset1)), dataset1, 
			jit1_x=jitter(dataset1[,eigenvectors[1]]), 
			jit1_y=jitter(dataset1[,eigenvectors[2]]), stringsAsFactors=FALSE);
	dataset2 <- data.frame(id=as.numeric(rownames(dataset2)), dataset2, 
			jit2_x=jitter(dataset2[,eigenvectors[1]]), 
			jit2_y=jitter(dataset2[,eigenvectors[2]]), stringsAsFactors=FALSE);
	
	if( in3d ){
		
		dataset1 <- cbind(dataset1, jit1_z=jitter(dataset1[,eigenvectors[3]]), stringsAsFactors=FALSE);
		dataset2 <- cbind(dataset2, jit2_z=jitter(dataset2[,eigenvectors[3]]), stringsAsFactors=FALSE);
		
		dataset <- merge(dataset1[,c("id", "jit1_x", "jit1_y", "jit1_z")], dataset2[,c("id", "jit2_x", "jit2_y", "jit2_z")], by="id");
		xRange <- range(c(range(dataset[,"jit1_x"]), range(dataset[,"jit2_x"])));
		yRange <- range(c(range(dataset[,"jit1_y"]), range(dataset[,"jit2_y"])));
		zRange <- range(c(range(dataset[,"jit1_z"]), range(dataset[,"jit2_z"])));
		
		segments3D(x0=dataset[,"jit1_x"], x1=dataset[,"jit2_x"], y0=dataset[,"jit1_y"], y1=dataset[,"jit2_y"], z0=dataset[,"jit1_z"], z1=dataset[,"jit2_z"], col="lightsteelblue", phi=20, theta=25, xlab=eigenvectors[1], ylab=eigenvectors[2], zlab=eigenvectors[3], colkey=TRUE);
		points3D(dataset[,"jit2_x"], dataset[,"jit2_y"], dataset[,"jit2_z"], bg="brown1", pch=21, col="brown", add=TRUE, phi=20, theta=25, cex=0.75);
		points3D(dataset[,"jit1_x"], dataset[,"jit1_y"], dataset[,"jit1_z"], bg="cadetblue1", pch=21, col="cadetblue", add=TRUE, phi=20, theta=25, cex=0.75);
#		legend(legendLocation[1], legendLocation[2], pch=21, pt.bg=c("cadetblue1", "brown1"), legend=c(legendLabels[1], legendLabels[2]));
		
	} else {
		
		dataset <- merge(dataset1[,c("id", "jit1_x", "jit1_y")], dataset2[,c("id", "jit2_x", "jit2_y")], by="id");
		xRange <- range(c(range(dataset[,"jit1_x"]), range(dataset[,"jit2_x"])));
		yRange <- range(c(range(dataset[,"jit1_y"]), range(dataset[,"jit2_y"])));
		
		segments2D(x0=dataset[,"jit1_x"], x1=dataset[,"jit2_x"], y0=dataset[,"jit1_y"], y1=dataset[,"jit2_y"], col="lightsteelblue", xlab=eigenvectors[1], ylab=eigenvectors[2]);
		points2D(dataset[,"jit2_x"], dataset[,"jit2_y"], bg="brown1", pch=21, col="brown", add=TRUE, cex=0.75);
		points2D(dataset[,"jit1_x"], dataset[,"jit1_y"], bg="cadetblue1", pch=21, col="cadetblue", add=TRUE, cex=0.75);
		legend(legendLocation[1], legendLocation[2], pch=21, pt.bg=c("cadetblue1", "brown1"), legend=c(legendLabels[1], legendLabels[2]));
	}
}
