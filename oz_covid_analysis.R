# Oz covid trajectories and comparisons ####

setwd("/home/timothy/Dropbox/Tim/data/oz_covid_trajectory")# local github repo location

# read in latest John Hopkins data
covid <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                  stringsAsFactors = FALSE)

# Make New-zealand part of Australia to make modelling easier
covid$Province.State[covid$Country.Region=="New Zealand"] = "New Zealand"
covid$Country.Region[covid$Country.Region=="New Zealand"] = "Australia"

# Australian data, excluding cruise ship
oz_cov <- covid[covid$Country.Region=="Australia" &
                  covid$Province.State != "From Diamond Princess",]

# Comparison countries
covid$Country.Region[covid$Province.State=="Hong Kong"] = "Hong Kong"
#comp_cov <- covid
comp_cov <- covid[covid$Country.Region %in% c("Singapore", "Hong Kong", "United Kingdom",
                                              "Italy", "US", "Korea, South"),]

# collapse locality data from other countries to country-level totals
comp_cov <- do.call("rbind", lapply(split(comp_cov, f=comp_cov$Country.Region), function(x){
  if(nrow(x)==1){return(x)}
  date.cols <- grepl("X", colnames(x))
  return(cbind(x[1,!date.cols], t(colSums(x[,date.cols]))))
  }))

comp_cov$Province.State = comp_cov$Country.Region

oz_cov <- rbind(oz_cov, comp_cov)

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

## MANUAL DATA CORRECTIONS ####

#John Hopkins is not updating new state cases well. I need to switch to an alternate
# data source, but for now I'll update manually.

oz_long$count[oz_long$Province.State=="Victoria" &
              oz_long$date %in% c("19/03", "20/03", "22/03", "23/03")] = c(150, 178, 296, 355)
 
oz_long$count[oz_long$Province.State=="New South Wales" &
               oz_long$date %in% c("22/03", "31/03", "01/04")] = c(533, 2182, 2298)
 
oz_long$count[oz_long$Province.State=="Tasmania" &
               oz_long$date %in% c("24/03", "25/03", "27/03", "01/04")] = c(36,42,58,71)

oz_long$count[oz_long$Province.State=="South Australia" &
                oz_long$date %in% c("25/03")] = c(197)

oz_long$count[oz_long$Province.State=="Western Australia" &
                oz_long$date %in% c("25/03", "27/03")] = c(205, 255)

oz_long$count[oz_long$Province.State=="Australian Captial Territory" &
                oz_long$date %in% c("25/03")] = c(44)

oz_long$count[oz_long$Province.State=="Northern Territory" &
                oz_long$date %in% c("27/03")] = c(14)

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
  x$double = ifelse(x$slope > 0, log(2) / x$slope, 0)
  x$double <- ifelse(x$double < 1000, paste0("Doubling every ", sprintf("%.1f", x$double), " days"), "")
  
    return(x)
}))

# write data file for commit to repo.
comp.model <- droplevels(oz.model[oz.model$CountryRegion!="Australia",])
oz.model <- droplevels(oz.model[oz.model$CountryRegion=="Australia",])

acs <- c("ACT", "NSW", "NZ", "NT", "QLD", "SA", "TAS", "VIC", "WA")
oz.model$state <- acs[as.numeric(factor(oz.model$ProvinceState,
                                       levels=sort(unique(oz_cov$Province.State[oz_cov$Country.Region=="Australia"]))))]

comp.acs <- c("Hong Kong", "Italy", "Sth Korea", "Singapore", "UK", "USA")
comp.model$state <- comp.acs[as.numeric(factor(comp.model$ProvinceState,
                                        levels=sort(unique(comp.model$ProvinceState))))]

  

# for the comparison countries, we want to extract trajectories when they had similar case
# loads to us, as their testing when cases were rare are pretty unreliable.

# Aus max case at current time
oz.max <- max(sapply(split(oz.model, f=oz.model$ProvinceState), function(x){return(rev(x$count)[1])}))

comp.model <- do.call("rbind", lapply(split(comp.model, f= comp.model$ProvinceState),
                     function(x){
                     
                      x.case.match <- which.min(abs(x$count - oz.max))
                    
                      # time conversion
                      x$oztimemax <- max(oz.model$statetime)
                      x$oztime = x$statetime + (max(oz.model$statetime) - x$statetime[x.case.match])
                      return(x)    
                      }))

write.table(oz.model, "./oz_model.csv", row.names=FALSE, sep=",")
write.table(oz.model[unique(c(which(oz.model$statetime %% 5 == 0), 
                              which(oz.model$date == oz.model$date[nrow(oz.model)]))),],
            "./oz_model_point.csv", row.names=FALSE, sep=",")
write.table(oz.model[oz.model$statetime >5,], "./oz_slope.csv", row.names=FALSE, sep=",")
