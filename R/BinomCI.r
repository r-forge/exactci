BinomCI<-function (n1, n2, x, y, confidence.level = 0.95, alternative = "Lower", precision = 0.00001,grid.one=30,grid.two=20) 
{
    if (length(n1) != 1 || (n1 < 1)) {
        stop("number of subjects n1 must be a positive integer")
    }
    if (length(n2) != 1 || (n2 < 1)) {
        stop("number of subjects n2 must be a positive integer")
    }
    if (length(grid.one) != 1 || (grid.one < 1)) {
        stop("number of grid in the first step search grid.one must be a positive integer")
    }
    if (length(grid.two) != 1 || (grid.two < 1)) {
        stop("number of grid in the second step search grid.two must be a positive integer")
    }    
    if (n1+n2>100) {
        "It may take more time to compute the confidence limits"
    }
    if (length(x) != 1 || (x < 0) || (x > n1)) {
        stop("observed number of response x must be an integer betwen 0 and n1")
    }
    if (length(y) != 1 || (y < 0) || (y > n2)) {
        stop("observed number of response y must be an integer betwen 0 and n2")
    }    
    if (length(confidence.level) != 1 || confidence.level < 0 || confidence.level > 
        1) {
        stop("confidence.level must be a positive number between 0 and 1, default 0.95")
    }
    if (length(precision) != 1 || precision < 0) {
        stop("precision must be a positive number, default 0.00001")
    }
    alternative <- match.arg(alternative, choices = c("Lower", 
        "Upper"))
    CI <- BinomialCIone(n1=n1, n2=n2, x=x, y=y, confidence.level = confidence.level, alternative = alternative,precision=precision,grid.one=grid.one,grid.two=grid.two)
    Result <- list(confidence.level = confidence.level, alternative = alternative,estimate = CI[1], ExactOneCI = CI[2:3])
    Result
}
