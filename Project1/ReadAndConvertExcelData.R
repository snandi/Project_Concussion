########################################################################
## Reads and converts data from Excel
########################################################################
rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )

saveCodedData <- function( DataCoded, folderPath, fileName ){
  filePath <- paste0( folderPath, fileName )
  save( DataCoded, file = filePath )
}

readRawData <- function( folderPath, fileName ){
  filePath <- paste0( folderPath, fileName )
  dataRaw <- read.table( file = filePath, header = T, sep = ',', quote = "\"" )
  return( dataRaw )
}

codeVariable <- function( vectorNum, vectorLevels, vectorLabels ){
  vectorCoded <- factor( vectorNum, levels = vectorLevels, labels = vectorLabels )
  return( vectorCoded )
}

codeAge <- function( ageRaw ){
  ageNAList <- c( '4', '-18', '21`', 'freshman', 'Freshman', 'junion', 'senior', 'Twenty years' )
  ageNum <- as.vector( ageRaw )
  ageNum[ ageNum %in% ageNAList ] <- NA
  ageNum <- as.numeric( ageNum )
  return( ageNum )
}

########################################################################
## Load header files and source functions
########################################################################
library( RFunctionsSN )

########################################################################
## Command line arguments for some static variables
########################################################################
#ProjectPath <- '~/Stat/Stat_Consulting/TraciSnedden/Project1/'
ProjectPath <- '~/Documents/snandi/Stat/Stat_Consulting/TraciSnedden/Project1/'

DataPath <- paste0( ProjectPath, 'Data/' )
fileNameData <- 'InjuryDataFinal_TotalDays.csv'
fileNameCodedData <- 'DataCoded.RData'
source( 'Codebook.R' )
########################################################################

Data <- readRawData( folderPath = DataPath, fileName = fileNameData )

Data$Q1.1_coded <- codeAge( ageRaw = Data$Q1.1 )
summary( Data$Q1.1 )
summary( Data$Q1.1_coded )

Data$Q1.4_coded <- codeVariable( vectorNum = Data$Q1.4, vectorLevels = Q1.4Levels, vectorLabels = Q1.4Labels )

Data$Q1.5_coded <- codeVariable( vectorNum = Data$Q1.5, vectorLevels = Q1.5Levels, vectorLabels = Q1.5Labels )
Data$Q1.5_coded <- as.factor( as.vector( Data$Q1.5_coded ) )

Data$Q1.6_coded <- codeVariable( vectorNum = Data$Q1.6, vectorLevels = Q1.6Levels, vectorLabels = Q1.6Labels )

Data$Q1.8_coded <- codeVariable( vectorNum = Data$Q1.8, vectorLevels = Q1.8Levels, vectorLabels = Q1.8Labels )

Data$Q1.9_coded <- codeVariable( vectorNum = Data$Q1.9, vectorLevels = Q1.9Levels, vectorLabels = Q1.9Labels )

Data$Q1.10_coded <- codeVariable( vectorNum = Data$Q1.10, vectorLevels = Q1.10Levels, vectorLabels = Q1.10Labels )

Data$Q2.1_coded <- codeVariable( vectorNum = Data$Q2.1, vectorLevels = Q2.1Levels, vectorLabels = Q2.1Labels )

Data$Q2.8_coded <- codeVariable( vectorNum = Data$Q2.8, vectorLevels = Q2.8Levels, vectorLabels = Q2.8Labels )

Data$Q3.1_coded <- codeVariable( vectorNum = Data$Q3.1, vectorLevels = Q3.1Levels, vectorLabels = Q3.1Labels )

Data$Q3.3_coded <- codeVariable( vectorNum = Data$Q3.3, vectorLevels = Q3.3Levels, vectorLabels = Q3.3Labels )

Data$Q3.4_coded <- codeVariable( vectorNum = Data$Q3.4, vectorLevels = Q3.4Levels, vectorLabels = Q3.4Labels )

i <- 1
for( i in 1:22 ){
  varName <- paste0( 'Q5.3_', i )
  varNameCoded <- paste0( varName, '_coded' )
  Data[, varNameCoded ] <- codeVariable( vectorNum = Data[, varName ], vectorLevels = Q5.3_1Levels, 
                            vectorLabels = Q5.3_1Labels )
  if( length( table( Data[, varNameCoded] ) ) > 6 ) print( table( Data[, varNameCoded] ) )
}

for( i in 1:8 ){
  varName <- paste0( 'Q7.1_', i )
  varNameCoded <- paste0( varName, '_coded' )
  Data[, varNameCoded ] <- codeVariable( vectorNum = Data[, varName ], vectorLevels = Q7.1_1Levels, 
                                         vectorLabels = Q7.1_1Labels )
  if( length( table( Data[, varNameCoded] ) ) > 3 ) print( table( Data[, varNameCoded] ) )
}

Data$Q4.1_coded <- codeVariable( vectorNum = Data$Q4.1, vectorLevels = Q4.1Levels, vectorLabels = Q4.1Labels )
Data$Q4.3_coded <- codeVariable( vectorNum = Data$Q4.3, vectorLevels = Q4.3Levels, vectorLabels = Q4.3Labels )
Data$Q4.4_coded <- codeVariable( vectorNum = Data$Q4.4, vectorLevels = Q4.4Levels, vectorLabels = Q4.4Labels )
Data$Q4.6_coded <- codeVariable( vectorNum = Data$Q4.6, vectorLevels = Q4.6Levels, vectorLabels = Q4.6Labels )
Data$Q4.8_coded <- codeVariable( vectorNum = Data$Q4.8, vectorLevels = Q4.8Levels, vectorLabels = Q4.8Labels )
Data$Q4.9_coded <- codeVariable( vectorNum = Data$Q4.9, vectorLevels = Q4.9Levels, vectorLabels = Q4.9Labels )

saveCodedData( DataCoded = Data, folderPath = DataPath, fileName = fileNameCodedData )

