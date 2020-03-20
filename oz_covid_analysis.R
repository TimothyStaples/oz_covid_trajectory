# Oz covid trajectories and comparisons ####
setwd("/home/timothy/Dropbox/Tim/oz_covid_trajectory")# local github repo location

# read in latest John Hopkins data
covid <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                  stringsAsFactors = FALSE)

# Australian data, excluding cruise ship
oz_cov <- covid[covid$Country.Region=="Australia" &
                  covid$Province.State != "From Diamond Princess",]

# Comparison countries - TO BE DONE STILL
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

# collect into long-form data for modeling
oz_long <- long.form(oz_cov[,grepl("X", colnames(oz_cov))],
                        oz_cov[,!grepl("X", colnames(oz_cov))])
oz_long$date <- as.Date(substr(rownames(oz_long), 2, nchar(rownames(oz_long))-1),
                        format="%m.%d.%y")
acs <- c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
oz_long$state <- acs[as.numeric(factor(oz_long$Province.State,
                                        levels=sort(unique(oz_long$Province.State))))]

# first state > 10 cases as threshold per state
first.oz <- oz_long$date[which(oz_long$count > 10)[1]]
first.state <- sapply(split(oz_long, f=oz_long$Province.State), 
                            function(x){x$date[which(x$count > 10)[1]]})
oz_long$oz.time <- as.numeric(oz_long$date) - as.numeric(first.oz)
oz_long$state.time <- as.numeric(oz_long$date - first.state[match(oz_long$Province.State, 
                                                       names(first.state))])
oz_long <- oz_long[oz_long$state.time >= 0 & !is.na(oz_long$state.time),]
oz_long$date <- as.character(oz_long$date)
oz_long$date<-paste0(substr(oz_long$date,nchar(oz_long$date)-1,nchar(oz_long$date)),
                     "/",
                     substr(oz_long$date,nchar(oz_long$date)-4,nchar(oz_long$date)-3))

# now model coefficients for every date we have > 7 data points for, for each state
oz.model <- do.call("rbind", lapply(split(oz_long, f=oz_long$Province.State), function(x){
print(x$Province.State[1])
  
  #set data columns
  x$int <- 0
  x$slope = 0
  x$max <- 0
  x$min = 0
  x$incr = 0
  x$double = 0
    
  x$date <- as.character(gsub("-","/",x$date))
  rownames(x) = NULL
  colnames(x) <- gsub("\\.", "", colnames(x))
  
  # find if we have more than 7 days of data
  if(sum(x$statetime >= 7) == 0 | sum(is.na(x$statetime)) == nrow(x)){
    return(x[x$count>10,])
  }
  
  # if we do, run a model for each date, pasting on intercept and slope coefficients
  # on just 1 week of data
  a <- t(sapply(6:max(x$statetime), function(n){
    m <- glm(count ~ statetime, family="poisson", x[x$statetime <= n & x$statetime >= (n-7), ])
    return(summary(m)$coefficients[,1])
  }))
  
  # set final coefficients
  x$int[x$statetime >=6] = a[,1]
  x$slope[x$statetime >=6] = a[,2]
  x$min = exp(x$int + x$slope*(x$statetime-7))
  x$max = exp(x$int + x$slope*(x$statetime+7))
  x$incr = exp(x$slope)-1
  x$double = ifelse(x$slope != 0, log(2) / x$slope, 0)
    return(x)
}))

# write data file for commit to repo.

write.table(oz.model, "./oz_model.csv", row.names=FALSE, sep=",")