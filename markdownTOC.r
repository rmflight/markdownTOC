
# takes a markdown file, and creates a TOC up to the level specified
# because we are working with markdown, we can use the advantage that things live on separate lines for headings.
# Right now we only support # style heading definition.
mdInpageTOC <- function(mdFile, maxLevel=Inf, tocString="TOC"){
	# read in the file
	mdText <- scan(file=mdFile, what="character", sep="\n", blank.lines.skip=F)
	
	# look for lines starting with "#" followed by anything else
	headerExp <- regexpr('^[#]+', mdText)
	headerLev <- attr(headerExp, "match.length")
	
	# which ones are valid, and what is their length or level
	headerLoc <- which(headerExp != -1)
	if (length(headerLoc) == 0){
		stop("No headers found. Please use 'atx' style headers!", call.=F)
	}
	
	headerLev <- headerLev[headerLoc]
	useLev <- headerLev <= maxLevel
	headerLoc <- headerLoc[useLev]
	headerLev <- headerLev[useLev]
	
	tocLoc <- grep(tocString, mdText)
	tocLoc <- tocLoc[(tocLoc %in% headerLoc)] # in case the string also appears in the main text
	if (length(tocLoc) == 0){
		stop("Table of Contents header string not found, exiting!", call.=F)
	}
	
	# this assumes that any header lines above the TOC location are to be ignored as part of the TOC
	lowerHead <- headerLoc > tocLoc
	headerLoc <- headerLoc[lowerHead]
	headerLev <- headerLev[lowerHead]
	
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
		anchTxt <- gsub(" ", "", anchTxt)
		
		mdText[headerLoc[hI]] <<- paste(headTxt, '<a id="', anchTxt, '"></a>', sep="")
		nBlank <- paste(rep(" ", (headerLev[hI] - 1) * 4 ), collapse="")
		headName <- paste(nBlank, "* [", headName, "](#", anchTxt, ")", sep="")
		return(headName)
		
	})
	
	# and insert the TOC immediately after the tocLoc
	topTxt <- mdText[1:tocLoc]
	botTxt <- mdText[seq(headerLoc[1],length(mdText))] # this removes any current TOC in the document
	mdText <- c(topTxt,"", tocText, "", botTxt)
	cat(mdText, file=mdFile, sep="\n")
	
}

# grabs the table of contents from a given file
grabToc <- function(mdText, tocString="TOC"){
	allHead <- regexpr("^[#]{1} ([[:alnum:]]| )", mdText)
	mainHead <- (which(allHead != -1))[1]
	fileHead <- substring(mdText[mainHead], 3)
	tocHead <- regexpr(paste('^[#]+ ', tocString, sep=""), mdText)
	tocLines <- regexpr("\\[([[:alnum:]]| )+\\]\\(#([[:alnum:]]| )+\\)", mdText) # looking for [textstring](#anchorstring)
	
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
mdFullToc <- function(mainFile=NULL, subFiles, tocString="TOC", filePath=NULL){
	subSplit <- strsplit(subFiles, .Platform$file.sep)
	subSplit <- sapply(subSplit, function(x){x[[length(x)]]})
	
	# now for each file
	mainToc <- sapply(subFiles, function(sF){
		mdText <- scan(sF, what="character", sep="\n", blank.lines.skip=F)
		sFWiki <- strsplit(sF, "\\.")[[1]][1]
		wikiLink <- file.path(filePath, sFWiki, fsep="/") # creating the full path the file on the server
		
		tocDat <- grabToc(mdText, tocString)
		headTOC <- paste('* [', tocDat$fileHead, '](', wikiLink, ')', sep="")
		subLinkLoc <- regexpr('\\(#', tocDat$tocTxt)
		subLinkTxt <- sapply(seq(1,length(subLinkLoc)), function(x){
			tmpTxt <- tocDat$tocTxt[x]
			str1 <- substring(tmpTxt, 1, subLinkLoc[x])
			str2 <- substring(tmpTxt, subLinkLoc[x]+1)
			paste(paste(rep(" ", 4), sep="", collapse=""), str1, wikiLink, str2, sep="")
		})
		return(c(headTOC, subLinkTxt))
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

# subFiles <- c("Programming-Resources.md", "Journals-I-follow.md")
# tocString <- "TOC"
# filePath <- "rmflight/general/wiki"
# mdFile <- "Programming-Resources.md"
# maxLevel <- Inf
# tocString <- "TOC"