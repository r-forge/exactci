PairedCI<-function (n12, t, n21, confidence.level = 0.95, alternative = "Lower", precision = 0.00001,grid.one=30,grid.two=20) 
{
    if (length(n12) != 1 || (n12 < 0)) {
        stop("number of subjects n12 must be a positive integer")
    }
    if (length(t) != 1 || (t < 0)) {
        stop("number of subjects t must be a positive integer")
    }
    if (length(n21) != 1 || (n21 < 0)) {
        stop("number of subjects n21 must be a positive integer")
    }
    if (length(grid.one) != 1 || (grid.one < 1)) {
        stop("number of grid in the first step search grid.one must be a positive integer")
    }
    if (length(grid.two) != 1 || (grid.two < 1)) {
        stop("number of grid in the second step search grid.two must be a positive integer")
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
    CI <- PairedCIone(n12=n12, t=t, n21=n21, confidence.level = confidence.level, alternative = alternative,precision=precision,grid.one=grid.one,grid.two=grid.two)
    Result <- list(confidence.level = confidence.level, alternative = alternative, estimate = CI[1], ExactOneCI = CI[2:3])
    Result
}
