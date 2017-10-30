#' Library and function for testing ML classification algorithms trained on simulated data
#'
#' Generate random data and apply different ML algorithms
#'
#' @param lwr lower bound for numerical data
#' @param upr upper bound for numerical data
#' @param N sample size
#'
#' @details In order to see the list of all private functions used in this library type mlsim::: and then hit the Tab key
#' @author Jacek Kowalski, \email{@australianunity.com.au}
#' @export
mlsim <- function (lwr = -5, upr = 5, N = 500) 
{
    rpart_chk <- check_package("rpart", "use rpart option")
    party_chk <- check_package("party", "use ctree option")
    rfsrc_chk <- check_package("randomForestSRC", "use rfsrc option")
    xgboost_chk <- check_package("xgboost", "use xgboost option")
    nnet_chk <- check_package("nnet", "use nnet option")
    svm_chk <- check_package("e1071", "use svm option")
    caret_chk <- check_package("caret", "output confusion matrix")
    sn <- as.integer(readline("Enter integer seed or blank for default: "))
    if (is.na(sn)) 
        sn <- 1
    set.seed(sn)
    cnt <- as.numeric(readline("Enter impurity fraction for each class (0-1) or blank for default 0.05: "))
    if (is.na(cnt)) 
        cnt <- 0.05
    T_shapes <- ls(pat = "_sh$", envir = getNamespace("mlsim"))
    T_shapes <- append(T_shapes, ls(pat = "_sh$", envir = globalenv()))

    shp_nr <- 0
    while (!shp_nr %in% 1:length(T_shapes)) {
        cat("Select number from the list for class T data shape: \n")
        for (i in 1:length(T_shapes)) cat(paste(i, "-", gsub("_sh", 
            "", T_shapes[i]), "\n"))
        shp_nr <- as.integer(readline(": "))
        cat("\n")
        if (!shp_nr %in% 1:length(T_shapes)) 
            cat("Wrong number selected. Try again.")
    }
    eval(parse(text = paste("fun <-", T_shapes[shp_nr])))
    data_in <- datagen(lwr = lwr, upr = upr, fun = fun, N = N, contam = cnt)
    par(mfrow = c(1, 2))
    continue <- T
    ml_num <- 0
    ML_alg <- ls(pat = "^run_", envir = getNamespace("mlsim"), all.names=TRUE)
    
    while (continue == T) {
        while (!ml_num %in% 1:length(ML_alg)) {
            cat("Select ML algorithm from the list: \n")
            for (i in 1:length(ML_alg))
	    	cat(paste(i, "-", gsub("^run_", "", ML_alg[i]), "\n"))
            ml_num <- as.integer(readline(": "))
            cat("\n")
            if (!ml_num %in% 1:length(ML_alg)) 
                cat("Wrong number selected. Try again.")
        }

        if (ML_alg[ml_num] == "run_logreg") {
            data_out <- run_logreg(indat = data_in)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("logistic regr. fit")
	    if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
        }

        if (ML_alg[ml_num] == "run_rpart")
	{
	   if(rpart_chk)
	   {
            dpt <- as.integer(readline("Max tree depth (1-30) or blank for default 10: "))
            if (is.na(dpt)) 
                dpt <- 10
            data_out <- run_rpart(indat = data_in, dp = dpt)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("rpart fit")
            if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
           }
	else
	    cat("library rpart not installed\n")
	}
	    
        if (ML_alg[ml_num] == "run_ctree")
	{
	   if(party_chk)
	   {
            dpt <- as.integer(readline("Max tree depth or blank for ctree to work it out: "))
            if (is.na(dpt)) 
                dpt <- 0
            data_out <- run_ctree(indat = data_in, dp = dpt)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("ctree fit")
	    if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
 
        }
	else 
	    cat("library party not installed\n")
	}
	    
        if (ML_alg[ml_num] == "run_rfsrc") {
	   if(rfsrc_chk)
	   {
            ntr <- as.integer(readline("Enter number of trees or blank for default 50: "))
            if (is.na(ntr)) 
                ntr <- 50
            data_out <- run_rfsrc(indat = data_in, nt = ntr)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("rfsrc fit")
            if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
        }
	else
	    cat("library rfsrc not installed\n")
 	}
	
        if (ML_alg[ml_num] == "run_xgboost") {
	   if(xgboost_chk)
	   {
            nrs <- as.integer(readline("Enter number of rounds or blank for default 50: "))
            if (is.na(nrs)) 
                nrs <- 50
            data_out <- run_xgboost(indat = data_in, nr = nrs)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("xgboost fit")
	    if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")            
        }
	else 
    	    cat("library xgboost not installed\n")
        }

        if (ML_alg[ml_num] == "run_svm") {
	   if(svm_chk)
	   {
	    kernels <- c("linear", "polynomial", "radial", "sigmoid")
            k <- 0
            while (!k %in% 1:4) {
                cat("Select kernel: \n")
                for (k in 1:4) cat(k, "-", kernels[k], "\n")
                k <- as.integer(readline(": "))
                cat("\n")
                if (!k %in% 1:4) 
                  cat("Number not on the list. Try again.\n")
            }
            data_out <- run_svm(indat = data_in, kern = k)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("svm fit")
	    if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
        }	
	else 
	    cat("library svm not installed\n")
	}
	    
        if (ML_alg[ml_num] == "run_nnet") {
	   if(nnet_chk)
	   {
            nc <- as.integer(readline("Enter number of cells in hidden layer or blank for default 15: "))
            if (is.na(nc)) 
                nc <- 15
            data_out <- run_nnet(indat = data_in, sz = nc)
            plot_data(data_in)
            title("sample")
            plot_data(data_out)
            title("nnet fit")
	    if(caret_chk)
		print(confusionMatrix(data = data_out$data$class, reference = data_in$data$class, positive = "C"))
		else
	    	cat("No diagnostic information since library caret not installed\n")
       }
	else 
	    cat("library nnet not installed\n")
	    }
	    
        if (as.character(readline("Continue (y/n)?: ")) == "y") {
            continue <- T
            ml_num <- 0
        }
        else continue <- F
    }
}
