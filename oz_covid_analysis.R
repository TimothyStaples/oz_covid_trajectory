# Oz covid trajectories and comparisons ####

setwd("/home/timothy/Dropbox/Tim/Personal/oz_covid_trajectory")

# read in latest John Hopkins data
covid <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                  stringsAsFactors = FALSE)

# Australian data, excluding cruise ship
oz_cov <- covid[covid$Country.Region=="Australia" &
                  covid$Province.State != "From Diamond Princess",]

# Comparison countries
comp_cov <- covid[covid$Country.Region %in% c("Singapore", "Hong Kong", "United Kingdom",
                                              "Italy", "US", "Korea, South"),]

# long-form data function
long.form <- function(wide.cols, retain.cols){
  data.frame(count = unlist(wide.cols),
             date = rep(colnames(wide.cols), each=nrow(wide.cols)),
             do.call("cbind", 
                     lapply(retain.cols, function(x){
                       rep(x, ncol(wide.cols))})),
             stringsAsFactors = FALSE)
  
}

# spread into long-form data for modeling
oz_long <- long.form(oz_cov[,grepl("X", colnames(oz_cov))],
                        oz_cov[,!grepl("X", colnames(oz_cov))])
oz_long$date <- as.Date(substr(rownames(oz_long), 2, nchar(rownames(oz_long))-1),
                        format="%m.%d.%y")

# first state > 10 cases as threshold
first.oz <- oz_long$date[which(oz_long$count > 10)[1]]
first.state <- sapply(split(oz_long, f=oz_long$Province.State), 
                            function(x){x$date[which(x$count > 10)[1]]})
oz_long$oz.time <- as.numeric(oz_long$date) - as.numeric(first.oz)
oz_long$state.time <- as.numeric(oz_long$date - first.state[match(oz_long$Province.State, 
                                                       names(first.state))])
oz_long <- oz_long[oz_long$oz.time >= 0,]

comp_long <- long.form(comp_cov[,grepl("X", colnames(comp_cov))],
                       comp_cov[,!grepl("X", colnames(comp_cov))])
comp_long$date <- as.Date(substr(rownames(comp_long), 2, nchar(rownames(comp_long))-1),
                        format="%m.%d.%y")

# now model coefficients for every date we have > 7 data points for, for each state
oz.model <- lapply(split(oz_long, f=oz_long$Province.State), function(x){
print(x$Province.State[1])
  # find if we have more than 7 days of data
  if(sum(x$state.time >= 7) == 0 | sum(is.na(x$state.time)) == nrow(x)){
    return(x[x$count>10,])
  }
  
  # if we do, run a model for each date, pasting on intercept and slope coefficients
  # on just 1 week of data
  a <- t(sapply(7:max(x$state.time), function(n){
    m <- glm(count ~ state.time, family="poisson", 
             x[x$state.time <= n & x$state.time > (n-7), ])
    return(summary(m)$coefficients[,1])
  }))
  
  x$int <- NA
  x$slope = NA
  x$int[x$state.time >=7] = a[,1]
  x$slope[x$state.time >=7] = a[,2]
  return(x)
})

write.csv(oz.model[[2]], "./NSW.csv")


#####################

cont.angles <- function(x,y){
  # converts differenced vector into angles on a continuous 360 degree scale
  angles <- -atan2(x, y) * (180/pi)
  angles <- ifelse(angles > 0, angles, 360 - abs(angles))
  rot.angles <- angles + 90
  rot.angles <- ifelse(rot.angles >= 360, rot.angles - 360, rot.angles)
  return(rot.angles)
}

pdf(paste0("./",Sys.Date(),".pdf"), height=4, width=6, useDingbats = FALSE)
par(mar=c(2,3,1,1), ps=8, tcl=-0.25, mgp=c(3,0.5,0), las=1)
plot(x=NULL, y=NULL, xlim=c(0,max(oz_long$state.time, na.rm=TRUE)+12), 
     ylim=log(c(10,max(oz_long$count, na.rm=TRUE)^1.5)), axes=FALSE, xlab="", ylab="")

axis(side=2, at=log(c(10,100,1000,10000)), labels=c(10,100,1000,10000))
axis(side=2, at=log(c(seq(10,100,10),
                      seq(100,1000,100),
                      seq(1000,10000,1000))), labels=NA)
mtext(side=2, line=2, text="Confirmed COVID-19 cases", las=0)
axis(side=1, mgp=c(3,0,0))
mtext(side=1,line=1, text="Days since 10th case")

doubling.points <- rev(c(1:10, 15,20,30))
doubling.limits <- c(par("usr")[3] + par("usr")[2]*(1/doubling.points),par("usr")[3])

sapply(2:length(doubling.points), function(n){
polygon(x=c(0, par("usr")[2], par("usr")[2], 0),
        y=c(log(10), doubling.limits[n], doubling.limits[n-1], log(10)),
        col=ifelse(n%%2==0, "grey90", "white"), border="grey80")

if(doubling.limits[n-1]>par("usr")[4]){
text.x <- (par("usr")[4]-log(10)) / (1/doubling.points[n-1])
text.y <- par("usr")[4]
} else {
text.x <- par("usr")[2]
text.y <- doubling.limits[n-1]
}

# text.angle <- atan2(text.x / (par("usr")[2]-par("usr")[1]), 
#                    (text.y - par("usr")[3]) / (par("usr")[4]-par("usr")[3])) * (180/pi)
text(x=text.x, y=text.y, 
     labels=paste0("D = ", doubling.points[n-1]), pos=2, adj=c(0.5,0.5), col="grey60")
})

sapply(1:length(sort(unique(oz_long$Province.State))), function(n){
  lab <- sort(unique(oz_long$Province.State))[n]
  ac <- c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")[n]
  col <- c("red", "blue", "darkgreen", "orange", "purple", "skyblue", "cyan", "black")[n]
  col.rgb <- col2rgb(col)/255
  
  oz_state <- oz_long[oz_long$Province.State==lab,]
  oz_state$time <- as.numeric(oz_state$date - oz_state$date[which(oz_state$count>=10)[1]])
  oz_state <- oz_state[!is.na(oz_state$time),]
  
  if(nrow(oz_state)>0){
  lines(log(oz_state$count) ~ oz_state$time, lwd=2, col=col)
  
  state.traj <- glm(count ~ time, family=poisson,
                    data=oz_state[oz_state$time > max(oz_state$time)-7,])
  pred.df<-data.frame(time=seq(max(oz_state$time),
                               max(oz_state$time)+7,len=100))
  state.pred <- cbind(pred.df,
                      as.data.frame(predict(state.traj, 
                                            newdata=pred.df,
                                            se.fit=TRUE)))
                                    
  state.pred$upper <- state.pred$fit + 1.96*state.pred$se.fit
  state.pred$lower <- state.pred$fit - 1.96*state.pred$se.fit
  state.pred$mean <- state.pred$fit
  
  polygon(x=c(state.pred$time,rev(state.pred$time)),
          y=c(state.pred$upper, rev(state.pred$lower)),
          border=NA, col=rgb(col.rgb[1], col.rgb[2], col.rgb[3], 0.25))
  lines(x=state.pred$time, y=state.pred$fit, col=col, lty="31")
  text(x=rev(state.pred$time)[1], y=rev(state.pred$fit)[1], 
       pos=4, col=col, labels=ac)
  }
  })

box()
dev.off()