# this function grabs stuff that is in between code chunks, and removes any header lines (i.e. comments)
# out of the original indices
inBetweenCode <- function(codeLoc, headerLoc){
	keepHead <- rep(TRUE, length(headerLoc))
	
	begCount <- 1
	endCount <- 2
	
	while (endCount < max(codeLoc)){
		greatCode <- headerLoc > codeLoc[begCount]
		lessCode <- headerLoc < codeLoc[endCount]
		keepHead[(greatCode & lessCode)] <- FALSE
		
		begCount <- begCount + 2
		endCount <- begCount + 1
	}
	headerLoc <- headerLoc[keepHead]
	return(headerLoc)
}


# takes a markdown file, and creates a TOC up to the level specified
# because we are working with markdown, we can use the advantage that things live on separate lines for headings.
# Right now we only support # style heading definition.
mdInpageTOC <- function(mdFile, maxLevel=Inf, tocString="%TOC%", newFile=F){
	# read in the file
	mdText <- scan(file=mdFile, what="character", sep="\n", blank.lines.skip=F)
	
	if (newFile){
		dotLoc <- regexpr("\\.", mdFile)
		if (dotLoc[1] != -1){
			mdRoot <- substring(mdFile, 1, max(dotLoc)-1)
			mdExt <- substring(mdFile, max(dotLoc)+1, nchar(mdFile))
		} else {
			mdRoot <- mdFile
			mdExt <- "md"
		}
		mdFile <- paste(mdRoot, "_toc.", mdExt, sep="")
	}
	
	# look for lines starting with "#" followed by anything else
	headerExp <- regexpr('^[#]+', mdText)
	headerLev <- attr(headerExp, "match.length")
	
	# also look for those lines with code block definitions
	codeExp <- regexpr('^`{3}[[:alnum:]]*', mdText)
	codeLoc <- which(codeExp != -1)
	
	# which ones are valid, and what is their length or level
	headerLoc <- which(headerExp != -1)
	headerLoc <- inBetweenCode(codeLoc, headerLoc)
	if (length(headerLoc) == 0){
		stop("No headers found. Please use 'atx' style headers!", call.=F)
	}
	
	headerLev <- headerLev[headerLoc]
	useLev <- headerLev <= maxLevel
	headerLoc <- headerLoc[useLev]
	headerLev <- headerLev[useLev]
	
	tocLoc <- grep(tocString, mdText)
	
	if (length(tocLoc) == 0){
		stop("Table of Contents header string not found, exiting!", call.=F)
	}
	else { tocLoc <- tocLoc[1] }
  
	# find anything that was already set
	headerAnch <- regexpr('<a', mdText)
	headerAnch <- headerAnch[headerLoc]
	nullAnch <- which(headerAnch == -1)
	headerAnch[nullAnch] <- nchar(mdText[headerLoc[nullAnch]])+1 # need to add 1 for when there is no anchor present
	
	# loop through the header lines, getting all the data we need
	tocText <- sapply(seq(1, length(headerLoc)), function(hI){
		tmpTxt <- mdText[headerLoc[hI]]
		headTxt <- substring(tmpTxt, 1, headerAnch[hI]-1)
		headName <- substring(headTxt, headerLev[hI]+2) # assumes there is one space between the # and the string
		anchTxt <- tolower(headName)
		
		# remove any blanks
		anchTxt <- gsub('[[:punct:]]|[[:space:]]', "", anchTxt)
		
		mdText[headerLoc[hI]] <<- paste(headTxt, '<a name="', anchTxt, '"></a>', sep="")
		nBlank <- paste(rep(" ", (headerLev[hI] - 1) * 4 ), collapse="")
		headName <- paste(nBlank, "* [", headName, "](#", anchTxt, ")", sep="")
		return(headName)
		
	})
  	
	# and replace the TOC string that was supplied previously
	mdText <- c(mdText[1:tocLoc-1], tocText, mdText[tocLoc:length(mdText)])
	# mdText <- c(topTxt,"", tocText, "", botTxt)
	cat(mdText, file=mdFile, sep="\n")
	
}

# grabs the table of contents from a given file
grabToc <- function(mdText, tocString="TOC"){
	allHead <- regexpr("^[#]{1} ([[:alnum:]]|[[:punct:]]|[[:space:]])", mdText)
	mainHead <- (which(allHead != -1))[1]
	fileHead <- substring(mdText[mainHead], 3)
	tocHead <- regexpr(paste('^[#]+ ', tocString, sep=""), mdText)
	tocLines <- regexpr("\\[([[:alnum:]]|[[:punct:]]|[[:space:]])+\\]\\(#([[:alnum:]])+\\)", mdText) # looking for [textstring](#anchorstring)
	
	tocLines <- which(tocLines != -1)
	tocLines <- tocLines[tocLines > which(tocHead != -1)]
	tocTxt <- mdText[tocLines]
	return(list(fileHead=fileHead, tocTxt=tocTxt))
}


# OK, you have a bunch of markdown files, and now you want to generate a navigation page that provides links to each one, and if
# a table of contents is present in the file, to make that available as well.
# this means you need to provide to this function the main file (or blank if you just want the text output), the list of filenames
# you want to be linked to, what string identifies the table of contents in those files, and the directory path to prepend to each link.
# For example, for Github wiki pages, the relative path from "github.com" should be supplied, /user/project/wiki/

# assumptions: The link text to use is the first header in each subFile, the TOC in each subFile is between '# tocString' and the next header, and all files are in the same directory.
mdFullToc <- function(mainFile=NULL, subFiles, tocString="TOC", filePath=NULL, is.gitwiki=TRUE){
	subSplit <- strsplit(subFiles, .Platform$file.sep)
	subSplit <- sapply(subSplit, function(x){x[[length(x)]]})
	
	# now for each file
	mainToc <- sapply(subFiles, function(sF){
		mdText <- scan(sF, what="character", sep="\n", blank.lines.skip=F)
		sFWiki <- strsplit(sF, "\\.")[[1]][1]
		wikiLink <- file.path(filePath, sFWiki, fsep="/") # creating the full path the file on the server
		
		tocDat <- grabToc(mdText, tocString)
		headTOC <- paste('* [', tocDat$fileHead, '](', wikiLink, ')', sep="")
		
		if (length(tocDat$tocTxt) != 0){
		
			subLinkLoc <- regexpr('\\(#', tocDat$tocTxt)
			subLinkTxt <- sapply(seq(1,length(subLinkLoc)), function(x){
				tmpTxt <- tocDat$tocTxt[x]
				str1 <- substring(tmpTxt, 1, subLinkLoc[x])
				str2 <- substring(tmpTxt, subLinkLoc[x]+2)
				
				if (is.gitwiki){
					paste(paste(rep(" ", 4), sep="", collapse=""), str1, wikiLink, '#wiki-', str2, sep="")
				} else {
					paste(paste(rep(" ", 4), sep="", collapse=""), str1, wikiLink, str2, sep="")
				}
			})
			headTOC <- c(headTOC, subLinkTxt) # append everything together
		} 
		return(headTOC)
	})
	if (is.null(mainFile)){
		return(unlist(mainToc, use.names=F))
	} else {
		mainTxt <- scan(mainFile, what="character", sep="\n", blank.lines.skip=F)
		allHead <- which(regexpr("^[#]{1} ([[:alnum:]]| )", mainTxt) != -1)
		tocHead <- which(regexpr(paste('^[#]+ ', tocString, sep=""), mainTxt) != -1)
		
		allHead <- allHead[allHead > tocHead]
		
		topTxt <- mainTxt[1:tocHead]
		botTxt <- mainTxt[seq(allHead[1],length(mainTxt))] # this removes any current TOC in the document
		mainTxt <- c(topTxt,"", unlist(mainToc, use.names=F), "", botTxt)
		cat(mainTxt, file=mainFile, sep="\n")
		
	}
}










