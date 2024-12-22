#calculated BMI and put it into the clean Dame and Linne processed files

DameDemographicsAndAnswers$BMI <- DameDemographicsAndAnswers$Weight / (DameDemographicsAndAnswers$Height / 100)^2
LinneDemographicsAndAnswers$BMI <- LinneDemographicsAndAnswers$Weight / (LinneDemographicsAndAnswers$Height / 100)^2