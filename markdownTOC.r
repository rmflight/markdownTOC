
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
	headerLev <- headerLev[headerLoc]
	useLev <- headerLev <= maxLevel
	headerLoc <- headerLoc[useLev]
	headerLev <- headerLev[useLev]
	
	tocLoc <- grep(tocString, mdText)
	tocLoc <- tocLoc[(tocLoc %in% headerLoc)] # in case the string also appears in the main text
	
	# this assumes that any header lines above the TOC location are to be ignored as part of the TOC
	lowerHead <- headerLoc > tocLoc
	headerLoc <- headerLoc[lowerHead]
	headerLev <- headerLev[lowerHead]
		
	# loop through the header lines, getting all the data we need
	tocText <- sapply(seq(1, length(headerLoc)), function(hI){
		tmpTxt <- mdText[headerLoc[hI]]
		headTxt <- substring(tmpTxt, headerLev[hI]+2) # assumes there is one space between the # and the string
		anchTxt <- tolower(headTxt)
		
		# remove any blanks
		anchTxt <- gsub(" ", "", anchTxt)
		
		mdText[headerLoc[hI]] <<- paste(mdText[headerLoc[hI]], '<a id="', anchTxt, '"></a>', sep="")
		nBlank <- paste(rep(" ", (headerLev[hI] - 1) * 4 ), collapse="")
		headTxt <- paste(nBlank, "* [", headTxt, "](#", anchTxt, ")", sep="")
		return(headTxt)
		
	})
	
	# and insert the TOC immediately after the tocLoc
	topTxt <- mdText[1:tocLoc]
	botTxt <- mdText[seq(tocLoc+1,length(mdText))]
	mdText <- c(topTxt,"", tocText, "", botTxt)
	cat(mdText, file=mdFile, sep="\n")
	
}

