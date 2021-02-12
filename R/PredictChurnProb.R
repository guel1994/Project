
ChurnProbFct <- function(dataset, CustID = 000000){
  library(data.table)
  library(lubridate)
  if(CustID %in% dataset$CustomerId ) {
    glmChurn <- glm(Exited ~ CreditScore + Gender + Age + Tenure
                    + Balance + NumOfProducts + HasCrCard + IsActiveMember
                    + EstimatedSalary,family = binomial(),data = dataset )
    dataset$ChurnProbability <-predict(glmChurn, dataset, type="response")
    CustChurnPro <- dataset[dataset$CustomerId == CustID, ]$ChurnProbability
    return(CustChurnPro)
  }else{
    print("Customer ID is not available in the data set provided")
  }
}
