load.cpi <- function() {
  rawcpi.df <- read.csv('cpi-all.csv', header=F)
  months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
             'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  names(rawcpi.df) <- c('Year', months)
  cpi.data <- NULL
  for (mo in months) {
    cpi.data <- rbind(cpi.data, cbind(rawcpi.df$Year,
                                      rep(mo, nrow(rawcpi.df)),
                                      rawcpi.df[[mo]]))
  }
  cpi.df <- data.frame(cpi.data)
  names(cpi.df) <- c('Year', 'Month', 'CPI')
  # Set types
  cpi.df$Year <- as.numeric(levels(cpi.df$Year))
  cpi.df$Month <- factor(cpi.df$Month, levels=months)
  cpi.df$CPI <- as.numeric(as.character(cpi.df$CPI))
  # Compute inflation
  n <- nrow(cpi.df)
  cpi.df$Inflation <- 
    c(NA, (cpi.df$CPI[2:n] - cpi.df$CPI[1:n-1]) / cpi.df$CPI[1:n-1] * 100)
  cpi.df
}

load.unemp <- function() {
  rawunemp.df <- read.csv('unemp-all.csv', header=F)
  months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
              'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  names(rawunemp.df) <- c('Year', months)
  unemp.data <- NULL
  for (mo in months) {
    unemp.data <- rbind(unemp.data, cbind(rawunemp.df$Year,
                                      rep(mo, nrow(rawunemp.df)),
                                      rawunemp.df[[mo]]))
  }
  unemp.df <- data.frame(unemp.data)
  names(unemp.df) <- c('Year', 'Month', 'Unemployment')
  # Set types
  unemp.df$Year <- as.numeric(levels(unemp.df$Year))
  unemp.df$Month <- factor(unemp.df$Month, levels=months)
  unemp.df$Unemployment <- as.numeric(as.character(unemp.df$Unemployment))
  unemp.df
}

# Arbitrary time periods
phillips <- function(startyr=NULL, endyr=NULL, lag=12, leg=TRUE,
                     labs=FALSE, clustersize=NULL, clusters=1) {
  cpi.df <- load.cpi()
  unemp.df <- load.unemp()
  # Cull for same start year
  min.yr <- max(min(cpi.df$Year), min(unemp.df$Year))
  cpi.df <- cpi.df[cpi.df$Year >= min.yr,]
  unemp.df <- unemp.df[unemp.df$Year >= min.yr,]
  # Aggregate
  df <- data.frame(unemp.df$Year, unemp.df$Month,
                   unemp.df$Unemployment, cpi.df$Inflation,
                   cpi.df$CPI)
  names(df) <- c("Year", "Month", "Unemployment", "Inflation", "CPI")
  # Sort by year then month
  df <- df[order(df$Year, df$Month),]
  row.names(df) <- c()
  # Check start and end year
  if (is.null(startyr)) {
    startyr <- min(df$Year)
  }
  if (is.null(endyr)) {
    endyr <- max(df$Year)
  }
  # Isolate the correct years
  df <- df[df$Year >= startyr & df$Year <= endyr,]
  n <- nrow(df)
  nyr <- n / 12
  # Set up clustering
  if (! is.null(clustersize)) {
    df$Cluster <- (df$Year - min(df$Year)) %/% clustersize + 1
  } else {
    if (clusters == 1) {
      df$Cluster <- rep(1, nrow(df))
    } else {
      df$Cluster <- as.numeric(cut(df$Year, breaks=clusters, 
                                   labels=1:clusters))
    }
  }
  # Plot that shit
  if (lag >= 0) {
    unemp.set <- 1:(n-lag)
    inf.set <- (1+lag):n
    inf.label <- paste("Inflation Rate (%) Subsequent", lag, "Month Period")
  } else {
    unemp.set <- (1-lag):n
    inf.set <- 1:(n+lag)
    inf.label <- paste("Inflation Rate (%) Preceding", -lag, "Month Period")
  }
  plot(df$Unemployment[unemp.set], df$Inflation[inf.set],
       xlab="Unemployment Rate (%)", ylab=inf.label,
       pch=21, bg=df$Cluster)
  if (labs == TRUE) {
    text(df$Unemployment[unemp.set], df$Inflation[inf.set],
         labels=df$Year[unemp.set], pos=4, cex=0.7)
  }
  if (leg == TRUE) {
    for (i in unique(df$Cluster)) {
      text(x=max(df$Unemployment[unemp.set]) - 0.25,
           y=max(df$Inflation[inf.set]) - 0.75 * i + 0.75,
           label=paste(min(df$Year[df$Cluster == i]),
                       "-", max(df$Year[df$Cluster == i])),
           col=i, cex=0.75)
    }
  }
  model <- lm(df$Inflation[inf.set] ~ df$Unemployment[unemp.set])
  lines(df$Unemployment[unemp.set], fitted(model), lwd=2, col="black")
  title(paste("Phillips Curve - US", startyr, "to", endyr))
  return(df)
}