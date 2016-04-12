#################################################################
#                                                               #
#              Survey of programming language project           #
#                                                               #
#              Sentiment Analysis on Twitter Data               #
#                                                               #
#################################################################

#       Date:                   April 19th, 2015
#
#       Team members:           Arunkumar Bagavathi
#                               Naveen Kumar Ananthu Langaram 
#                               Yanghai Cong
#                               Guoqing Yu
#                               Gautham Srikanth 
#                                       
#       REFERENCE:
#       [1]     Training and checking statistics are partial from:
#               http://fivethirtyeight.com/features/2015-nba-playoffs-preview/
#       [2]     The codes in getthedataframe comes from the blog: 
#               https://mkmanu.wordpress.com/2014/08/05/
#               sentiment-analysis-on-twitter-data-text-analytics-tutorial/
#       [3]     Some ideas are from the book: The art of R programming(N. Matloff)
#       [4]     Some ideas are from the book: R in action(R.I. Kabacoff)
#
#       Beginning of the project
#
#       load related libraries

library(twitteR)
library(RColorBrewer)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(Rcpp)
library(NLP)
library(tm)
library(Rstem)
library(sentiment)
library(ggplot2)
library(httr)

#       setup twitter oauth
oauth_endpoints('twitter')
api_key <- "xX4fseWcfKsAxwB5OwQtAnamX"
api_secret <- "ZmbJ8qm4lA65vA1wyeKDd4UJ3rYdx5Ilr5gnOFjlV2ETAs94Sf"
access_token <- "97900134-V4BdOghmII1Va0Q1dFGisUESflsEtUcVXh4hvGidr"
access_token_secret <- "k8tkn0luZ88MS1T61EBhd3oSyUlymxei8psr4IyySSLah"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1

tweets<-function(teamname){

	#       get related tweets   
        tweets = searchTwitter(teamname, n=100, lang="en",since=NULL, until=NULL)
        
        #       data cleansing
        txt = sapply(tweets, function(x) x$getText())
        txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
        txt = gsub("@\\w+", "", txt)
        txt = gsub("[[:punct:]]", "", txt)
        txt = gsub("[[:digit:]]", "", txt)
        txt = gsub("http\\w+", "", txt)
        txt = gsub("[ \t]{2,}", "", txt)
        txt = gsub("^\\s+|\\s+$", "", txt)
        catch.error = function(x)
        {
                y = NA
                catch_error = tryCatch(tolower(x), error=function(e) e)
                if (!inherits(catch_error, "error"))
                        y = tolower(x)
                return(y)
        }
        txt = sapply(txt, catch.error)
        txt = txt[!is.na(txt)]
        names(txt) = NULL		
		return(txt)
}

#       define a function to get the dataframe of a team
getthedataframe<-function(teamname){       
        
		# read team names        
		txt <- tweets(teamname)
        #       sentiment analysis
        class_emo = classify_emotion(txt, algorithm="bayes", prior=1.0)
        emotion = class_emo[,7]
        emotion[is.na(emotion)] = "unknown"
        #
        class_pol = classify_polarity(txt, algorithm="bayes")
        polarity = class_pol[,4]
        #
        sentiment_dataframe = data.frame(text=txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
        sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
        return(sentiment_dataframe)
}

#       define a function to compare two teams
compare<- function(){
        # read team names
        print("Please enter the first team you want to compare...")
        team1<-readline()
        print("Please enter the second team you want to compare...")
        team2<-readline()
        cat("Calculating... Please wait...\n")
        #       get data frames by calling getthedataframe()
        team1df<-getthedataframe(team1)
        team2df<-getthedataframe(team2)
        #       percentage
        pteam1<-sum(team1df["polarity"]=="positive")/sum(team1df["polarity"]=="negative")
        pteam2<-sum(team2df["polarity"]=="positive")/sum(team2df["polarity"]=="negative")
        #       who will win
        pofteam1win<-pteam1/(pteam1+pteam2)
        pofteam2win<-1-pofteam1win
        slices<-c(pofteam1win,pofteam2win)
        #       pie chart
        lbls<-c(team1,team2)
        pct<-round(slices/sum(slices)*100)
        lbls2<-paste("P of team: ",lbls," wins: ",pct,"%" ,sep="")
        pie(slices,labels=lbls2,main=paste("Winning Possibility: ",team1," vs ", team2))
        #       print out
        print(paste(team1," vs ",team2))
        return(slices)
}

#       define a function to compare two teams and plot bar graph
bargraph<- function(){

 # read team names
        print("Please enter the first team you want to generate a bar graph..")
        team1<-readline()
        print("Please enter the second team you want to generate a bar graph...")
        team2<-readline()
                cat("Calculating... Please wait...\n")
        #       get data frames by calling getthedataframe()
        team1df<-getthedataframe(team1)
        team2df<-getthedataframe(team2)

	  # plot distribution of polarity for Team1
        p1 <-ggplot(team1df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
		scale_fill_brewer(palette="RdGy") +
		labs(x="polarity categories", y="number of tweets") + ggtitle(paste("Sentiment Analysis of Tweets of Team: ", team1)) + 
		theme(plot.title = element_text(lineheight=.8, face="bold"))
        
	  # plot distribution of polarity for Team2
        p2 <-ggplot(team2df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
		scale_fill_brewer(palette="RdGy") +
		labs(x="polarity categories", y="number of tweets") + ggtitle(paste("Sentiment Analysis of Tweets of Team: ", team2)) + 
		theme(plot.title = element_text(lineheight=.8, face="bold"))

	multiplot(p1, p2,cols=2)
        print("Please check the bar chart...")
}

#       define a function to create a word cloud
wordcloud<- function(){

		# read team names
        print("Please enter a team to generate word cloud...")
        team1<-readline()
		txt <- tweets(team1)

        # sentiment analysis
        class_emo = classify_emotion(txt, algorithm="bayes", prior=1.0)
        emotion = class_emo[,7]
        emotion[is.na(emotion)] = "unknown"

	# classify polarity
	class_pol = classify_polarity(txt, algorithm="bayes")
	
	# get polarity best fit
	polarity = class_pol[,4]
        
	# data frame with results
	team1df = data.frame(text=txt, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)

	# sort data frame
	team1df = within(team1df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

	# separating text by emotion
	emos = levels(factor(team1df$emotion))
	nemo = length(emos)
	emo.docs = rep("", nemo)
	for (i in 1:nemo)
	{
   	  tmp = txt[emotion == emos[i]]
   	  emo.docs[i] = paste(tmp, collapse=" ")
	}

	# remove stopwords
	emo.docs = removeWords(emo.docs, stopwords("english"))
	# create corpus
	corpus = Corpus(VectorSource(emo.docs))
	tdm = TermDocumentMatrix(corpus)
	tdm = as.matrix(tdm)
	colnames(tdm) = emos

	# comparison word cloud
	comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5)
}

#       define a function to check teams individually
individual<-function(){

        #       read team names
        print("Please enter the team you are interested in...")
        team<-readline()
        cat("Calculating... Please wait...\n")
        #       get data frame by calling getthedataframe()
        teamdf<-getthedataframe(team)
        #       count
        pos<-sum(teamdf["polarity"]=="positive")
        neg<-sum(teamdf["polarity"]=="negative")
        #       pie chart
        lbls<-c("Positve","Negative")
        slices<-c(pos,neg)
        pct<-round(slices/sum(slices)*100)
        lbls2<-paste(lbls," ",pct,"%",sep="")
        pie(slices,labels=lbls2,main=paste("Polarity of Team: ",team))
        #       print out
        print("Please check the pie chart...")
}

showNBAteams<-function(){
        cat("NBA Team names on Twitter:
        [1]Raptors              [2]warriors             [3]denvernuggets
        [4]okcthunder           [5]PelicansNBA          [6]dallasmavs
        [7]hornets              [8]Lakers               [9]nyknicks
        [10]MNTimberwolves      [11]LAClippers          [12]OrlandoMagic
        [13]Pacers              [14]cavs                [15]HoustonRockets
        [16]BrooklynNets        [17]Suns                [18]spurs
        [19]utahjazz            [20]celtics             [21]ATLHawks
        [22]DetroitPistons      [23]chicagobulls        [24]Sixers
        [25]Bucks               [26]WashWizards         [27]MiamiHEAT
        [28]memgrizz            [29]trailblazers        [30]SacramentoKings")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	library(grid)

	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)

	numPlots = length(plots)

	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
	# Make the panel
	# ncol: Number of columns of plots
	# nrow: Number of rows needed, calculated from # of cols
	layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
	}

	if (numPlots==1) {
		print(plots[[1]])

	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
		  # Get the i,j matrix positions of the regions that contain this subplot
		  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

		  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
										  layout.pos.col = matchidx$col))
		}
	}
}

