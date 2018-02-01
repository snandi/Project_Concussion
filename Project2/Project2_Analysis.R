########################################################################
## Get data for Project 2
## Analyze Project 2 question
########################################################################
rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )

########################################################################
## Load header files and source functions
########################################################################
library( plyr )
library( ggplot2 )
library( RFunctionsSN )
library( xtable )
########################################################################
## Command line arguments for some static variables
########################################################################
#ProjectPath <- '~/Stat/Stat_Consulting/TraciSnedden/Project2/'
ProjectPath <- '~/Documents/snandi/Stat/Stat_Consulting/TraciSnedden/Project2/'

DataPath <- paste0( ProjectPath, 'Data/' )
fileNameData <- 'InjuryDataFinal_TotalDays.csv'
fileNameCodedData <- 'DataCoded.RData'

source( '../Project1/Codebook.R' )
source( paste0( ProjectPath, 'Project2_Static.R' ) )

########################################################################
## Functions
########################################################################
saveData <- function( Data, folderPath ){
  fileNameTxt <- 'Project2Data.csv'
  fileNameR <- 'Project2Data.RData'
  filePath <- paste0( folderPath, fileNameTxt )
  write.table( x = Data, file = filePath, quote = F, sep = ',', row.names = F )
  
  filePath <- paste0( folderPath, fileNameR )
  save( Data, file = filePath )
}

barPlotByQuestion <- function( TableLong, TitleText ){
  barPlot <- qplot() + geom_bar( aes( x = Severity, y = Responses, fill = Injury, group = Injury ), 
                                 data = TableLong, position = 'dodge', stat = 'identity' ) +
    coord_flip() +
    scale_fill_manual( breaks = c( "Concussion", "Musculoskeletal" ), values = c( "red", "gray40" ) ) +
    xlab( label = '' ) +
    ggtitle( label = TitleText ) +
    theme( 
      legend.position = 'top'
    )
  
  return( barPlot )
}

getTableByQuestion <- function( DataProject2, Question ){
  library( reshape2 )
  varName <- Question
  varNameCoded <- paste0( Question, '_coded' )
  DataNoNa <- subset( DataProject2, !( is.na( as.vector( get( varNameCoded ) ) ) ) )
  DataNoNa[, varNameCoded] <- as.factor( as.vector( DataNoNa[, varNameCoded] ) )
  Table <- table( DataNoNa$InjuryType, DataNoNa[, varNameCoded] )
  DataNoNa <- DataNoNa[, c( 'InjuryType', varNameCoded, varName ) ]
  TableLong <- melt( Table )
  colnames( TableLong ) <- c( 'Injury', 'Severity', 'Responses' )
  TableLong$Severity <- factor( TableLong$Severity, Q5.3Levels ) ## Reorder factor levels manually
  
  return( list( Table = Table, TableLong = TableLong, DataNoNa = DataNoNa ) )
}

getCodedData <- function( folderPath, fileName ){
  filePath <- paste0( folderPath, fileName )
  load( filePath )
  return( DataCoded )
}

getProject2Data <- function( folderPath, fileName ){
  DataCoded <- getCodedData( folderPath, fileName )
  DataProject2 <- DataCoded[, Project2_VarNames ]
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q2.1_coded'] <- 'InjuryType'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.1_coded'] <- 'Age'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.4_coded'] <- 'Gender'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.5_coded'] <- 'Race'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.6_coded'] <- 'Ethnicity'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.8_coded'] <- 'AcadStatus'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q1.9_coded'] <- 'MentalIllness'
  colnames( DataProject2 )[colnames( DataProject2 ) == 'Q2.7_TotalDays'] <- 'Days'
  
  DataProject2 <- subset( DataProject2, InjuryType == 'Concussion' | InjuryType == 'Musculoskeletal' )
  
  DataProject2$InjuryType <- as.factor( as.vector( DataProject2$InjuryType ) )
  # colnames( DataProject2 ) <- Project2_VarRenames
  # DataProject2$InjuryType <- NULL
  return( DataProject2 )
}

########################################################################
## Data
########################################################################
DataProject2 <- getProject2Data( folderPath = DataPath, fileName = fileNameCodedData )
saveData( Data = DataProject2, folderPath = DataPath )

Responses5.3DF <- as.data.frame( cbind( Q5.3Text, pValueChi = 0, pValueT = 0, muDiff = 0 ), 
                                stringsAsFactors = F )
Responses5.3DF$pValueChi <- as.numeric( Responses5.3DF$pValueChi )
Responses5.3DF$pValueT <- as.numeric( Responses5.3DF$pValueT )

########################################################################
## Demographics
########################################################################
TableRace <- table( DataProject2$Race, DataProject2$InjuryType )
TableEth <- table( DataProject2$Ethnicity, DataProject2$InjuryType )

RaceEth <- rbind( TableRace, TableEth ) 
rownames( RaceEth )[6] <- 'NA'
RaceEth <- RaceEth[ rownames( RaceEth ) != 'NA', ]

TableGender <- table( DataProject2$InjuryType, DataProject2$Gender )
TableInjury <- table( DataProject2$InjuryType )
Male <- TableGender[,2]
Female <- TableGender[,3]
TableAge <- aggregate( Age ~ InjuryType, data = DataProject2, FUN = mean )
SDAge <- aggregate( Age ~ InjuryType, data = DataProject2, FUN = sd )

Table1 <- rbind( t( TableInjury ), t( Male ), t( Female ), 
                 as.character( round( as.numeric( t( TableAge )[2,] ), 2) ),
                 as.character( round( as.numeric( t( SDAge )[2,] ), 2) ) )

rownames( Table1 ) <- c( 'N', 'N (males)', 'N (females)', 'Average Age', 'Std Dev Age' )

Table1 <- rbind( Table1, RaceEth )

TableDays <- round( aggregate( Days ~ InjuryType, data = DataProject2, FUN = mean )[, 2], 0 )
Table1 <- rbind( Table1, TableDays )

TableHistory <- table( DataProject2$MentalIllness, DataProject2$InjuryType )
rownames( TableHistory ) <- c( "NA", "ADHD/ADD", "Learning Disability", "Psychiatric Disorder", 
                               "Migraine Headaches", "None" )
Table1 <- rbind( Table1, TableHistory )

i <- 1
for( i in 1:22 ){
  Question <- paste0( 'Q5.3_', i )
  ## 1 Get tables
  Tables <- getTableByQuestion( DataProject2, Question )

  ## 2 Chi-square test
  TestChi <- chisq.test( Tables$Table )
  pValueChi <- round( TestChi$p.value, 5 )
  
  Responses5.3DF[Question, 'pValueChi'] <- pValueChi

  ## 3 T test
  DataNoNa <- Tables$DataNoNa
  dataConc <- subset( DataNoNa, InjuryType == 'Concussion' )[, Question ]
  dataMusc <- subset( DataNoNa, InjuryType == 'Musculoskeletal' )[, Question ]
  TestT <- t.test( x = dataConc, y = dataMusc, alternative = "greater", var.equal = T )
  pValueT <- round( TestT$p.value, 5 )
  muDiff <- round( diff( TestT$estimate ), 2 )
  
  Responses5.3DF[Question, 'pValueT'] <- pValueT
  Responses5.3DF[Question, 'muDiff'] <- muDiff
  
  ## 4 Bar plot
  # barPlot <- barPlotByQuestion( TableLong = Tables$TableLong, TitleText = Q5.3Text[[ Question ]] )
  # Question <- gsub( pattern = "\\.", replacement = "_", x = Question )
  # filePath <- paste0( ProjectPath, Question, '_barPlot.pdf' )    
  # ggsave( filename = filePath, plot = barPlot, device = 'pdf', height = 6, width = 8 )
  # 
  # filePath <- paste0( ProjectPath, Question, '_barPlot.jpg' )    
  # ggsave( filename = filePath, plot = barPlot, device = 'jpeg', height = 6, width = 8 )
}

print( xtable( Responses5.3DF, align = "llrrr", caption = "Responses By Injury Type", 
               label = "tab:Tab1Summary" ), table.placement = "H" )


