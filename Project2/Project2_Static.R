########################################################################
## This script has all the static variables for Project 2
########################################################################

Project2_VarNames <- c( 'Q1.1_coded', 'Q1.4_coded', 'Q1.5_coded', 'Q1.6_coded', 'Q1.9_coded',
                        'Q2.1_coded', 'Q1.8_coded', 'Q2.7_TotalDays',
                        paste0( 'Q7.1_', 1:8 ), paste0( 'Q7.1_', 1:8, '_coded' ),
                        paste0( 'Q5.3_', 1:22 ), paste0( 'Q5.3_', 1:22, '_coded' ) )

Q5.3Text <- c( 
  'I forget what has been said in class', 
  'I get overwhelmed when studying',	   
  'I get overwhelmed in class',	   
  'I get nervous before tests',	   
  'I have trouble managing my time',	   
  'I have difficulty taking notes while in a lecture class',	   
  'I am late to class',	   
  'I have trouble prioritizing assignments and meeting deadlines',	   
  'Others do not understand my problems',	   
  'I procastinate on things I need to do',	   
  'I have to review materials more than I used to',	   
  'I dont always understand instructions for assignments',	   
  'I need extra time to complete assignments',	   
  'I have difficulty putting my thoughts into words',	   
  'I have trouble reading books or magazines',	   
  'I have trouble paying attention in class',	   
  'I have trouble paying attention while studying',	   
  'I have trouble doing work on a computer or other mobile device',	   
  'I cant study for as long as I used to prior to my injury',	   
  'I get headaches when I read or write',	   
  'I need extra time to complete assignments',	   
  'I have fewer friends than before my injury'
)

names( Q5.3Text ) <- paste0( 'Q5.3_', 1:22 )

Q5.3Levels <- c( "Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", 
                 "Strongly Agree" )
