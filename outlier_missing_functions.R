replace_outlier_with_quantile <-
  function(x,
           quant = TRUE,
           probs = c(0.01, 0.99),
           na.rm = TRUE) {
    if (quant == T) {
      qnt <- quantile(x, probs = probs, na.rm = na.rm)  # get %iles
      U <- qnt[2]
      L <- qnt[1]
      y <- x
      y[x < L] <- L  # replace values below lower bounds
      y[x > U] <- U
      y
    } else if (quant == F) {
      qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm)  # get %iles
      H <- 3 * IQR(x, na.rm = na.rm)  # outlier limit threshold
      y <- x
      y[x < (qnt[1] - H)] <-
        (qnt[1] - H)  # replace values below lower bounds
      y[x > (qnt[2] + H)] <-
        (qnt[2] + H)  # replace values above higher bound
      y  # returns treated variable
    } 
    
  }


replace_missing_with_median <-
  function(x,
           default.colona = as.name(default.y),
           probs = c(0.01, 0.99),
           na.rm = TRUE,
           training = T,
           ...) {
    x < as.data.frame(x)
    median. <-
      median(x, na.rm = na.rm)
    median.h = median(x[default.colona == 0], na.rm = na.rm)
    median.d = median(x[default.colona == 1], na.rm = na.rm)
    
    if (training == T) {
      x[is.na(x) & default.colona == 0] <- median.h
      x[is.na(x) &
          default.colona == 1] <-
        median.d
    } else {
      x[is.na(x)] <- median.
    }
    x
  }




replace_outlier_with_na <- function(x,quant = TRUE,
                                    probs = c(0.01, 0.99),
                                    na.rm = TRUE) {
  if (quant == T) {
    qnt <- quantile(x, probs = probs, na.rm = na.rm)  # get %iles
    U <- qnt[2]
    L <- qnt[1]
    y <- x
    y[x < L] <- NA  # replace values below lower bounds
    y[x > U] <- NA
    y
  } else if (quant == F) {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm)  # get %iles
    H <- 3 * IQR(x, na.rm = na.rm)  # outlier limit threshold
    y <- NA
    y[x < (qnt[1] - H)] <- NA  # replace values below lower bounds
    y[x > (qnt[2] + H)] <- NA  # replace values above higher bound
    y  # returns treated variable
    
  }
}




replace_missing_with_knn <- function(x, response_name = "default.y", training = T) {
  
  x <- as.data.frame(x)
  
  response<-x[,eval(response_name)]
  if (training == F) {
    if (anyNA(x)) {
      x[,!names(x) %in% response_name] <-
        knnImputation(x[,!names(x) %in% response_name])  # missing value treatment
    }
  } else {
    if(anyNA(x)) {
      x[response==1,!names(x) %in% response_name] <-
        knnImputation(x[response==1,!names(x) %in% response_name])  # missing treatment
    }
    
    if (anyNA(x)) {
      x[response==0,!names(x) %in% response_name] <-
        knnImputation(x[response==0,!names(x) %in% response_name])  # missing value treatment  
      
    }
    
  }
  as.data.table(x)
}
