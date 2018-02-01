rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )

########################################################################
## Load header files and source functions
########################################################################
library( ggplot2 )
library( lm.beta )
library( plyr )
library( RFunctionsSN )
library( xtable )
########################################################################
## Command line arguments for some static variables
########################################################################
ProjectPath <- '~/Stat/Stat_Consulting/TraciSnedden/Project2/'
#ProjectPath <- '~/Documents/snandi/Stat/Stat_Consulting/TraciSnedden/Project2/'

DataPath <- paste0( ProjectPath, 'Data/' )

source( '../Project1/Codebook.R' )
source( paste0( ProjectPath, 'Project2_Static.R' ) )


########################################################################
## Functions
########################################################################
loadProject2Data <- function( folderPath ){
  fileNameR <- 'Project2Data.RData'
  filePath <- paste0( folderPath, fileNameR )
  load( filePath )
  return( Data )
}

DataProject2 <- loadProject2Data( folderPath = DataPath )
DataProject2$AcadEffects <- 0
  
for( i in 1:22 ){
  columnName <- paste0( 'Q5.3_', i )
  DataProject2[, columnName][DataProject2[, columnName] == -7 ] <- 0
  if( i != 7 ){
    DataProject2$AcadEffects <- DataProject2$AcadEffects + DataProject2[, columnName]
  }
}
VarsForModel <- c( 'AcadEffects', 'Age', 'Gender', 'Race', 'MentalIllness', 'InjuryType', 'AcadStatus' )

DataForModel <- subset( DataProject2[, VarsForModel ], AcadEffects > 0 )

DataForModel <- DataForModel[ !is.na( as.vector( DataForModel$Gender ) ), ]
#DataForModel <- DataForModel[ !is.na( as.vector( DataForModel$Race ) ), ]
#DataForModel <- DataForModel[ !is.na( as.vector( DataForModel$MentalIllness ) ), ]
DataForModel <- DataForModel[ !is.na( as.vector( DataForModel$InjuryType ) ), ]
DataForModel <- DataForModel[ !is.na( as.vector( DataForModel$AcadStatus ) ), ]

# DataForModel <- within( data = DataForModel,{
#   Gender        <- as.factor( is.na( as.vector( Gender ) ) )
#   Race          <- factor( as.vector( Race ) )
#   MentalIllness <- factor( as.vector( MentalIllness ) )
#   InjuryType    <- factor( as.vector( InjuryType ) )
#   AcadStatus    <- factor( as.vector( AcadStatus ) )
# })
str( DataForModel )

qplot() + geom_histogram( aes( x = AcadEffects ), data = DataForModel )

Model1 <- lm( log( AcadEffects ) ~ Age + Gender + Race + MentalIllness + InjuryType + AcadStatus +
                InjuryType*Gender + InjuryType*Race + InjuryType*MentalIllness + InjuryType*AcadStatus, 
              data = DataForModel  )
summary( Model1 )
anova( Model1 )

Model2 <- step( Model1, direction = 'backward' )

qplot() + geom_boxplot( aes( x = Q7.1_2_coded, y = AcadEffects ), data = DataProject2 )

Model2 <- lm( log( AcadEffects ) ~ Gender + AcadStatus + InjuryType, data = DataForModel )
summary( Model2 )
anova( Model2 )

Model3 <- lm( AcadEffects ~ Gender + AcadStatus + InjuryType, data = DataForModel )
summary( Model3 )

Model3_Standardized <- lm.beta( object = Model3 )
summary( Model3_Standardized )

Coeff <- as.data.frame( ( summary( Model3_Standardized )$coefficients )[, c( 1, 3, 2, 5 ) ] )
colnames( Coeff ) <- c( 'B', 'SE_B', 'beta', 'pValue' )
Coeff <- within( data = Coeff, expr = {
  B = round( B, 2 )
  SE_B = round( SE_B, 2 )
  beta = round( beta, 2 )
  pValue = round( pValue, 4 )
})
rownames( Coeff ) <- c(
  'Intercept',
  'Gender: Female',
  'Academic Status: completed 1-2 yrs',
  'Academic Status: completed 2-3 yrs',
  'Academic Status: completed 3-4 yrs',
  'Academic Status: completed 4 or more yrs',
  'Injury Type: Musculoskeletal'
)

Model3Anova <- aov( AcadEffects ~ Gender + AcadStatus + InjuryType, data = DataForModel )
TukeyHSD( x = Model3Anova, which = 'AcadStatus' )
