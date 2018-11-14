# Ekow Lewis
# Yuecheng Fang
churnR <- function(start_ind,
                   end_ind,
                   start_dep,
                   end_dep,
                   evaluate=TRUE) {
  f <- "%m/%d/%Y"
  t1 <- as.Date(start_ind, f)
  t2 <- as.Date(end_ind, f)
  t3 <- as.Date(start_dep, f)
  t4 <- as.Date(end_dep, f) #dump date
  length_ind <- t2 - t1
  #Load all required packages
  for (i in c("AUC","lift","ada", "imputeMissings")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i,
                       repos='http://cran.rstudio.com',
                       quiet=TRUE)
      require(i,
              character.only=TRUE,
              quietly=TRUE)
    }
  }
  readAndPrepareData <- function(train=TRUE,...){
    #DATA UNDERSTANDING
    ###############################################################
    cat("Reading in data:")
    time <- Sys.time()
    f <- "%d/%m/%Y"
    setClass('fDate')
    setAs(from ="character",
          to="fDate",
          def=function(from) as.Date(from, format=f))
    
    classes1 <- c("character",
                  "character",
                  "character",
                  "fDate",
                  "factor",
                  "factor",
                  "factor")
    
    complaints <- read.table("complaints.txt", sep = ";", header = T, colClasses = classes1)
    
    classes2 <- c("character",
                  "character",
                  "character",
                  "factor",
                  "fDate",
                  "fDate",
                  "numeric",
                  "integer",
                  "fDate",
                  "character",
                  "character",
                  "fDate",
                  "integer",
                  "numeric",
                  "numeric",
                  "numeric",
                  "numeric",
                  "numeric",
                  "numeric",
                  "numeric",
                  "numeric")
    
    subscriptions <- read.table("subscriptions.txt", sep = ";", header = T, colClasses = classes2)
    
    classes3 <- c("character",
                  "factor",
                  "fDate",
                  "numeric",
                  "character",
                  "character")
    
    customers <- read.table("customers.txt", sep = ";", header = T, colClasses = classes3)
    
    classes4 <- c("character", 
                  "character", 
                  "factor", 
                  "character",
                  "factor", 
                  "fDate", 
                  "fDate")
    
    delivery <- read.table("delivery.txt", sep = ";", header = T, colClasses = classes4)
    
    classes5 <- c("character", 
                  "character",
                  "factor",
                  "fDate", 
                  "factor",
                  "numeric", 
                  "integer")
    
    credit <- read.table("credit.txt", sep = ";", header = T, colClasses = classes5)
    
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    
    cat("Preparing basetable:")
    time <- Sys.time()
    ###############################################################
    #DATA PREPARATION
    
    if (train==FALSE){
      dots <- list(...) # list(...) evaluates all arguments and
      # returns them in a named list
      t2 <- dots$end_ind
      t1 <- t2 - dots$length_ind
      rm(dots)
    }
    
    now <- Sys.Date()
    age <- as.numeric(now - customers$DOB)/365
    customers$Age <- age
    # Split the data into independent and dependebt data
    subscriptionsIND <- subscriptions[subscriptions$StartDate <= t2 & subscriptions$StartDate >= t1, ]
    
    if (train == TRUE){
    subscriptionsDEP <- subscriptions[subscriptions$EndDate > t3 & subscriptions$EndDate <= t4, ]
    }
    # Get customers who had subscriptions before t2
    cust_before_t2 <- unique(subscriptions[subscriptions$RenewalDate <= t2 & subscriptions$StartDate >= t1, "CustomerID"]) 
    # Subset only variables in the independent period and use as predictors
    subscriptionsIND <- subscriptionsIND[subscriptionsIND$CustomerID %in% cust_before_t2,]
    
    if (train == TRUE){
      dependent <- merge(data.frame(CustomerID = unique(subscriptionsIND[,"CustomerID"], drop = F), stringsAsFactors = F),
                         data.frame(CustomerID = unique(subscriptionsDEP[, "CustomerID", drop = F]), Churn = 0),
                         by = "CustomerID", all.x = T)
      dependent$Churn[is.na(dependent$Churn)] <- 1
      
      dependent$Churn <- as.factor((dependent$Churn))
      
    }
    
    
    # Merge all the tables to create predictors
    cust_subsript <- merge(subscriptionsIND, customers[, c(1,4,7), drop = F], by = "CustomerID", all.x = T)
    
    # Merge subscriptions and complaint
    # Identtify customers who complain and create a complaint indicator
    complaint_cust <- unique(complaints$CustomerID)
    cust_subsript$ComplaintIndicator <- ifelse(cust_subsript$CustomerID %in% complaint_cust, 1, 0)
    
    # Merge custoemers, subscriptions, complaints and delivery tables
    cus_sub_del <- merge(cust_subsript, delivery[which(delivery$EndDate <= t2 & delivery$StartDate >= t1),c(2, 3, 4, 5), drop = F], by = "SubscriptionID", all.x = T)
    
    # Finally, merge the credit table
    cus_sub_del_cre <- merge(cus_sub_del, credit[which(credit$ProcessingDate <= t2 & credit$ProcessingDate >= t1), c(2,5,6), drop = F], by = "SubscriptionID", all.x = T)
    
    # Find number of instance for each unique CustomerID
    df <- aggregate(subscriptionsIND$CustomerID, list(CustomerID = subscriptionsIND$CustomerID), length)
    
    # Monetary value for each customerID
    df_moneytory <- aggregate(subscriptionsIND$TotalPrice, list(CustomerID = subscriptionsIND$CustomerID), sum)
    names(df_moneytory)[2] <- "Moneytary"
    
    # Frequency of purchase for each customerID
    df_frequency <- aggregate(subscriptionsIND$RenewalDate, list(CustomerID = subscriptionsIND$CustomerID), length)
    names(df_frequency)[2] <- "Frequency"
    
    # Number of complaints
    Complaint_Before_t2 <- complaints[complaints$ComplaintDate <= t2 & complaints$ComplaintDate > t1,]
    df_NbrComplaints <- aggregate(Complaint_Before_t2$CustomerID, list(CustomerID = Complaint_Before_t2$CustomerID), length)
    names(df_NbrComplaints)[2] <- "NumberOfComplaints"
    
    
    #Add Number of complaints
    subscr_newIND1 <- merge(cus_sub_del_cre, df_NbrComplaints, by = "CustomerID", all.x = T)
    
    # Add monetary column
    subscr_newIND <- merge(subscr_newIND1, df_moneytory, by = "CustomerID", all.x = T)
    
    subscr_newIND$NumberOfComplaints[is.na(subscr_newIND$NumberOfComplaints)] <- 0
    
    # Add Frequency column
    subscri_newIND <- merge(subscr_newIND, df_frequency, by = "CustomerID", all.x = T)
    
    newdata <- list()
    j <- 0
    for(i in unique(subscri_newIND$CustomerID)){
      j <- j + 1
      newdata[[j]] <- subscri_newIND[which(subscri_newIND$CustomerID == i),]
    }
    # Select maximum subsription end date for each customerId 
    newlist <- list()
    for (i in 1:length(newdata)){
      newlist[[i]] <- newdata[[i]][which(newdata[[i]]$EndDate == max(newdata[[i]]$EndDate)),]
    }
    
    # Join the entries in the list
    subscripIND <- do.call(rbind, newlist)
    
    # select 1 observation for each customer
    subscriptIND <- do.call("rbind", by(subscripIND, INDICES=subscripIND$CustomerID, FUN=function(DF) DF[which.max(DF$EndDate), ]))
    
    model1 <- subscriptIND[, c(1, 7, 8, 10, 11, 19, 20, 21, 23, 24, 26, 29, 30, 31, 32)]
    
    # Credit Indicator
    model1$Amount[which(!is.na(model1$Amount))] <- 1
    model1$Amount[which(is.na(model1$Amount))] <- 0
    names(model1)[12] <- "CreditIndicator"
    
    # PaymentType Indicator
    model1$PaymentType <- ifelse(model1$PaymentType == "BT", 1, 0)
    
    # PaymentStatus Indicator
    model1$PaymentStatus <- ifelse(model1$PaymentStatus == "Paid", 1, 0)
    
    # DeliveryClass Indicator
    model1$DeliveryClass <- ifelse(model1$DeliveryClass == "NOR", 1, 0)
    
    # Creating a Basetable
    Basetable1 <- model1
    # Impute missing values
    Basetable1 <- imputeMissings::impute(Basetable1)
    
    if (train == TRUE){
      data <- list(data.frame(CustomerID = Basetable1$CustomerID, NbrNewspapers = Basetable1$NbrNewspapers),
                   data.frame(CustomerID = Basetable1$CustomerID, NbrStart = Basetable1$NbrStart),
                   data.frame(CustomerID = Basetable1$CustomerID, PaymentType = Basetable1$PaymentType),
                   data.frame(CustomerID = Basetable1$CustomerID, PaymentStatus = Basetable1$PaymentStatus),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalDiscount = Basetable1$TotalDiscount),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalPrice = Basetable1$TotalPrice),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalCredit = Basetable1$TotalCredit),
                   data.frame(CustomerID = Basetable1$CustomerID, Age = Basetable1$Age),
                   data.frame(CustomerID = Basetable1$CustomerID, ComplaintIndicator = Basetable1$ComplaintIndicator),
                   data.frame(CustomerID = Basetable1$CustomerID, DeliveryClass = Basetable1$DeliveryClass),
                   data.frame(CustomerID = Basetable1$CustomerID, CreditIndicator = Basetable1$CreditIndicator),
                   data.frame(CustomerID = Basetable1$CustomerID, NumberofComplaints = Basetable1$NumberOfComplaints),
                   data.frame(CustomerID = Basetable1$CustomerID, Monetary = Basetable1$Moneytary),
                   data.frame(CustomerID = Basetable1$CustomerID, Frequency = Basetable1$Frequency),
                   data.frame(CustomerID = dependent$CustomerID, Churn = dependent$Churn))
    } else {
      data <- list(data.frame(CustomerID = Basetable1$CustomerID, NbrNewspapers = Basetable1$NbrNewspapers),
                   data.frame(CustomerID = Basetable1$CustomerID, NbrStart = Basetable1$NbrStart),
                   data.frame(CustomerID = Basetable1$CustomerID, PaymentType = Basetable1$PaymentType),
                   data.frame(CustomerID = Basetable1$CustomerID, PaymentStatus = Basetable1$PaymentStatus),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalDiscount = Basetable1$TotalDiscount),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalPrice = Basetable1$TotalPrice),
                   data.frame(CustomerID = Basetable1$CustomerID, TotalCredit = Basetable1$TotalCredit),
                   data.frame(CustomerID = Basetable1$CustomerID, Age = Basetable1$Age),
                   data.frame(CustomerID = Basetable1$CustomerID, ComplaintIndicator = Basetable1$ComplaintIndicator),
                   data.frame(CustomerID = Basetable1$CustomerID, DeliveryClass = Basetable1$DeliveryClass),
                   data.frame(CustomerID = Basetable1$CustomerID, CreditIndicator = Basetable1$CreditIndicator),
                   data.frame(CustomerID = Basetable1$CustomerID, NumberofComplaints = Basetable1$NumberOfComplaints),
                   data.frame(CustomerID = Basetable1$CustomerID, Monetary = Basetable1$Moneytary),
                   data.frame(CustomerID = Basetable1$CustomerID, Frequency = Basetable1$Frequency))
      
    }
    
    Basetable <- Reduce(function(x,y) merge(x, y, by = "CustomerID"), data)
    
    if (train==TRUE){
      Basetable$CustomerID <- NULL
      Churn <- Basetable$Churn
      Basetable$Churn <- NULL
    }
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    #Our basetable is now ready and we can
    #move on the modeling phase
    
    if (train==TRUE){
      return(list(predictors=Basetable, Churn=Churn))
    } else {
      return(Basetable)
    }
  }#end readAndPrepareData
  Basetable <- readAndPrepareData()
  
  if (evaluate==TRUE){
    cat("Evaluating model:")
    time <- Sys.time()
    #We'll be using adaptive boosting and hence we will not require a validation set.
    #Split the data in a train and test set
    ind <- 1:nrow(Basetable$predictors)
    indTRAIN <- sample(ind,round(0.5*length(ind)))
    indTEST <- ind[-indTRAIN]
    #Fit the random forest on the training set
    ABmodel <- ada(x=Basetable$predictors[indTRAIN,],
                   y=Basetable$Churn[indTRAIN],
                   iter=150)
    #Deploy the ada boost on the test set
    predAB <- as.numeric(predict(ABmodel,
                                 Basetable$predictors[indTEST,],
                                 type="probs")[,2])
    
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    cat(" Number of predictors:", ncol(Basetable$predictors[indTRAIN,]),
        "predictors\n")
    cat(" AUROC:",
        round(AUC::auc(roc(predAB,Basetable$Churn[indTEST])),4),"\n")
    cat(" Top decile lift of:",
        round(TopDecileLift(predAB,Basetable$Churn[indTEST]),4),"\n")
  }
  cat("Creating model:")
  time <- Sys.time()
  
  #Build model on all data
  ABmodel <- ada(x=Basetable$predictors,
                 y=Basetable$Churn)
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  l <- list(ABmodel = ABmodel,
            readAndPrepareData = readAndPrepareData,
            f = f,
            length_ind = length_ind)
  class(l) <- "churnR"
  return(l)
}

churnModel <- churnR(start_ind="01/02/2006",
                     end_ind="07/25/2009",
                     start_dep="07/26/2009",
                     end_dep="03/02/2011",
                     evaluate=TRUE)


predict.churnR <- function(object, dumpDate) {
  #Load all required packages
  for (i in c("AUC","lift", "ada", "imputeMissings")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i,
                       repos='http://cran.rstudio.com',
                       quiet=TRUE)
      require(i,
              character.only=TRUE,
              quietly=TRUE)
    }
  }
  #Make sure all variables in the readAndPrepareData
  #enclosing environment (where it was defined) are removed
  #to avoid unexpected results
  environment(object$readAndPrepareData) <- environment()
  basetable <- object$readAndPrepareData(train=FALSE,
                                         end_ind = as.Date(dumpDate, object$f),
                                         length_ind=object$length_ind)
  cat("Predicting: ")
  time <- Sys.time()
  ans <- data.frame(CustomerID=basetable$CustomerID,
                    Score=predict(object=object$ABmodel,
                                  newdata=basetable,
                                  type="prob")[,2])
  ans <- ans[order(ans$Score, decreasing=TRUE),]
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  ans
}

pred <- predict(object=churnModel, dumpDate="03/02/2011")
head(pred, 15)
