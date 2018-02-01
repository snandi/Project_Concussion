########################################################################
## Get data for Project 1
## Analyze Project 1 question
########################################################################
rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )

########################################################################
## Load header files and source functions
########################################################################
library( ggplot2 )
library( RFunctionsSN )
library( xtable )
########################################################################
## Command line arguments for some static variables
########################################################################
ProjectPath <- '~/Stat/Stat_Consulting/TraciSnedden/Project1/'
#ProjectPath <- '~/Documents/snandi/Stat/Stat_Consulting/TraciSnedden/Project1/'

DataPath <- paste0( ProjectPath, 'Data/' )
fileNameData <- 'InjuryDataFinal_TotalDays.csv'
fileNameCodedData <- 'DataCoded.RData'

source( 'Codebook.R' )
source( 'Project1_Static.R' )

########################################################################

########################################################################
## Functions
########################################################################
getCodedData <- function( folderPath, fileName ){
  filePath <- paste0( folderPath, fileName )
  load( filePath )
  return( DataCoded )
}

getProject1Data <- function( folderPath, fileName ){
  DataCoded <- getCodedData( folderPath, fileName )
  DataProject1 <- DataCoded[, Project1_VarNames ]
  colnames( DataProject1 ) <- Project1_VarRenames
  DataProject1 <- subset( DataProject1, InjuryType == 'Concussion' )
  DataProject1$InjuryType <- NULL
  return( DataProject1 )
}


########################################################################
## Data
########################################################################
DataProject1 <- getProject1Data( folderPath = DataPath, fileName = fileNameCodedData )
DataProject1NoNA <- subset( DataProject1, !is.na( as.vector( ReportedConcussion ) ) )
DataProject1NoNA$ReportedConcussion <- as.factor( as.vector( DataProject1NoNA$ReportedConcussion ) )
table( DataProject1$ReportedConcussion )

########################################################################
## Preliminary analysis
########################################################################

## Age
summary( DataProject1$Age )

Plot1Age <- qplot() + geom_boxplot( aes( x = ReportedConcussion, y = Age, fill = ReportedConcussion ), 
                        data = subset( DataProject1NoNA, Age < 35 ) ) +
  scale_fill_manual( breaks = c( "NO", "YES" ), values = c( "red", "gray" ) ) +
  theme( legend.position = 'top' ) + 
  ylab( label = 'Age' )

Plot2AgeGender <- qplot() + geom_boxplot( aes( x = ReportedConcussion, y = Age, fill = ReportedConcussion ), 
                                    data = subset( DataProject1NoNA, Age < 35 ) ) +
  scale_fill_manual( breaks = c( "NO", "YES" ), values = c( "red", "gray" ) ) +
  theme( legend.position = 'top' ) +
  ylab( label = 'Age' ) +
  facet_grid( ~ Gender )

## Gender
DataProject1$Gender <- as.factor( as.vector( DataProject1$Gender ) )
tableGender <- table( DataProject1$ReportedConcussion, DataProject1$Gender )
tableGenderNoNA <- table( DataProject1NoNA$ReportedConcussion, DataProject1NoNA$Gender )

xtable( t( tableGender ), caption = "Self-reporting of concussion", label = "tab:Tab2Gender" )

chisq.test( t( tableGenderNoNA ) )

## Race
tableRace <- table( DataProject1$ReportedConcussion, DataProject1$Race )
xtable( t( tableRace ), caption = "Self-reporting of concussion by Race", label = "tab:Tab3Race" )

## Ethnicity
tableEth <- table( DataProject1$ReportedConcussion, as.vector( DataProject1$Ethnicity ) )
xtable( t( tableEth ), caption = "Self-reporting of concussion by Ethnicity", label = "tab:Tab4Eth" )

## Academic status
tableAcad <- table( DataProject1$ReportedConcussion, DataProject1$AcadStatus )
colnames( tableAcad )[1] <- 'NA'
xtable( t( tableAcad ), caption = "Self-reporting of concussion by Academic status", label = "tab:Tab5Acad" )

## Describes you
tableDesc <- table( DataProject1$ReportedConcussion, DataProject1$DescribeYou )
colnames( tableDesc )[1] <- 'NA'
xtable( t( tableDesc ), caption = "Self-reporting of concussion by type of activity", label = "tab:Tab6Desc" )

## Reason
tableReason <- table( DataProject1$ReportedConcussion, as.vector( DataProject1$Reason ) )
xtable( t( tableReason ), caption = "Self-reporting of concussion by reason", label = "tab:Tab7Reason" )

## How long after injury did you report?
summary( DataProject1$ReportHours )
DataReported <- subset( DataProject1, ReportedConcussion == "YES" )


aggregate( ReportHours ~ Gender, data = subset( DataReported, ReportHours < 168 ), FUN = length )
aggregate( ReportHours ~ Gender, data = subset( DataReported, ReportHours < 168 ), FUN = mean )
aggregate( ReportHours ~ Gender, data = subset( DataReported, ReportHours < 168 ), FUN = sd )
aggregate( ReportHours ~ Gender, data = subset( DataReported, ReportHours < 168 ), FUN = median )
mean( subset( DataReported, ReportHours < 168 )$ReportHours )
median( subset( DataReported, ReportHours < 168 )$ReportHours )
sd( subset( DataReported, ReportHours < 168 )$ReportHours )

aggregate( ReportHours ~ Gender, data = DataReported, FUN = length )
aggregate( ReportHours ~ Gender, data = DataReported, FUN = mean )
aggregate( ReportHours ~ Gender, data = DataReported, FUN = sd )
aggregate( ReportHours ~ Gender, data = DataReported, FUN = median )
mean( DataReported$ReportHours )
median( DataReported$ReportHours )
sd( DataReported$ReportHours )


Plot3ReportHours <- qplot() + geom_boxplot( aes( x = Gender, y = ReportHours, fill = Gender ), 
                                    data = subset( DataReported, ReportHours < 100 ) ) +
  theme( legend.position = 'top' ) +
  ylab( label = 'Hours reported after' )

t.test( x = DataReported$ReportHours[ DataReported$Gender == "Male" ], 
        y = DataReported$ReportHours[ DataReported$Gender == "Female" ], 
        var.equal = FALSE, 
        alternative = "two.sided" )

## Who did you report it to? 
DataReported$ReportedTo <- as.factor( as.vector( DataReported$ReportedTo ) )
tableWho <- table( DataReported$ReportedTo )

## If no, why did you not report it?  
DataNotReported <- subset( DataProject1, ReportedConcussion == "NO" )
tableWhyNot <- table( as.vector( DataNotReported$WhyNotReported ) )
xt <- xtable( tableWhyNot, caption = "Why did they not report", label = "tab:Tab8WhyNot" )
names(xt) <- 'responses'
xt

## Model
str( DataProject1NoNA$ReportedConcussion )
DataProject1NoNA$Y <- TRUE
DataProject1NoNA$Y[DataProject1NoNA$ReportedConcussion == "NO" ] <- FALSE
table( DataProject1NoNA$Y )
DataProject1NoNA$Gender <- as.factor( as.vector( DataProject1NoNA$Gender ) )
str( DataProject1NoNA$Gender )

Data <- DataProject1NoNA[,c('Y', 'Age', 'Gender')]
Data$Age <- Data$Age - mean( Data$Age )
Model1 <- glm( as.numeric(Y) ~ Age + Gender , data = Data, family = "binomial" )
summary( Model1)

Model2 <- glm( as.numeric(Y) ~ Age + Gender + Age*Gender, data = Data, family = "binomial" )
summary( Model2 )
confint( Model2 )
round( exp( confint( Model2 ) ), 4 ) 
exp( coef( Model2 ) )

print( xtable( round( exp( confint( Model2 ) ), 4 ), digits = c(0, 3, 3), 
               caption = "Confidence intervals of odds ratio", label = "tab:Tab9CI" ), 
       table.placement = "H" )

newData <- as.data.frame( rbind( cbind( Age = 18, Gender = "Female" ), 
                  cbind( Age = 18, Gender = "Male" ), 
                  cbind( Age = 22, Gender = "Female"),
                  cbind( Age = 22, Gender = "Male")
) )
newData$Age <- as.numeric( as.vector( newData$Age ) ) -  mean(DataProject1NoNA$Age)

exp( predict( Model2, newData ) )[1] /( 1 + exp( predict( Model2, newData ) )[1] )
exp( predict( Model2, newData ) )[2] /( 1 + exp( predict( Model2, newData ) )[2] )
exp( predict( Model2, newData ) )[3] /( 1 + exp( predict( Model2, newData ) )[3] )
exp( predict( Model2, newData ) )[4] /( 1 + exp( predict( Model2, newData ) )[4] )

####################################################################
## New output table format
####################################################################
Summary <- summary( Model2 )$coefficients

Table1 <- cbind( col1 = paste0( round( Summary[,'Estimate'], 2 ), ' (', round( Summary[,'Std. Error'], 2 ), ')' ), 
                col2 = round( Summary[,'z value'], 2 ), 
                col3 = round( exp( Summary[,'Estimate'] ), 2 )
)

Table2 <- round( exp( confint( Model2 ) ), 2 )

Table <- cbind( Table1, Table2 )
Table <- cbind( Predictor = rownames( Table ), Table )
colnames( Table ) <- c( 'Predictor', 'b(SE)', 'Wald', 'Odds Ratio', 'Lower OR', 'Upper OR' )
Table[, 'Predictor'] <- c( 'Constant', 'Age', 'Male', 'Age:Male' )

for( Row in 1:nrow( Table ) ){
  if( Summary[Row, 4] < 0.05 ){
    Table[Row, 'Wald'] <- paste0( Table[Row, 'Wald'], '*' )
  } 
}

logLik( Model2 )

Model0 <- glm( as.numeric(Y) ~ 1, data = Data, family = "binomial" )
summary( Model0 )
logLik( Model0 )

X2 <- 2*( logLik( Model2 )- logLik( Model0 ) ) # log-likelihood ratio test statistic
as.numeric( X2 )
pvalX2 <- 1 - pchisq( X2, 3 )

R2 <- ( summary( Model2 )$null.deviance - summary( Model2 )$deviance )/summary( Model2 )$null.deviance

Fitted <- fitted.values( Model2 ) > 0.5
sum( Fitted == Data$Y )/length( Fitted )
