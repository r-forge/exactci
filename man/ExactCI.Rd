\name{Exact one-sided 1-alpha confidence interval}
\alias{BinomCI}
\alias{PairedCI}
\title{Inductive confidence intervals for the difference between two proportions}
\description{
Exact one-sided confidence intervals of level 1-alpha are constructed for p1-p2,
 the difference of two proportions under two cases. 
 
 Case 1:  X follows Binomial(n1,p1), Y follows Binomial(n2,p2) and X and Y are independent. Intervals [L(X,Y),1], [-1,U(X,Y)] and [L(X,Y),U(X,Y)] are of interest. 
 
 Case 2: A random vector (n11,n12,n21,n22) follows 
 multinomial(N, p11,p12,p21,p22) where N=n11+n12+n21+n22, 
 and p1-p2=(p11+p12)-(p11+p21)=p12-p21. Let t=n11+n22. 
 Intervals [L(n12,t,n21),1], [-1,U(n12,t,n21)] and [L(n12,t,n21),U(n12,t,n21)] are of interest. 
 
 This package can be used to calculate these intervals using an inductive method
  developed by Wang (2010, 2012).           
}

\usage{
BinomCI(n1,n2,x,y,confidence.level,alternative,precision,grid.one,grid.two)
PairedCI(n12,t,n21,confidence.level,alternative,precision,grid.one,grid.two)}
\arguments{
  \item{n1}{the number of subjects in the first group in the parallel two-arm study}
  \item{n2}{the number of subjects in the second group in the parallel two-arm study}
  \item{x}{the number of response from the first group in the parallel two-arm study}
  \item{y}{the number of response from the second group in the parallel two-arm study}
  \item{n12}{the number of subjects in the paired study who have response from the treatment and no response from the control}
  \item{t}{the number of subjects in the paired study who have the same results from the treatment and control, t=n11+n22}
  \item{n21}{the number of subjects in the paired study who have response from the contol and no response from the treatment}
  \item{confidence.level}{Confience level for one sided}
  \item{alternative}{c("Lower","Upper"), one sided lower confidence interval [L,1], one sided upper confidence interval [-1,U]}
  \item{precision}{Precision of the confidence interval, default is 0.00001 rounded to 5 decimals}
  \item{grid.one}{two-step grid search algorithm is used. grid.one is the number of points for searching the global maximum of the tail probability in the first step. 30 is the default value}   
  \item{grid.two}{grid.two is the number of points for searching the global maximum of the tail probability in the second step. 20 is the default value}   
  }

\details{
 An inductive interval construction is carried out. At each step we rank sample 
 point by its potential confidence interval and then select the one with the
  shortest interval. The difference of the two proportions is the parameter of interest.
   There is a nuisance parameter in the tail probability 
   (Eq (6) in Wang 2010, or Eq (8) in Wang 2012). The nuisance parameter is 
   eliminated by the maximization originally proposed by Buehler (1957).
    A two-step grid search algorithm is applied to find the maximum. 
    The first step is to roughly identify a neighbor area of the global 
    maximization of the tail probability,  more points used more accurate results achieved. 
    We recommend to use grid.one at least 30 to have accurate confidence intervals. 
    The second step is to search for maximum within that smaller neighbor area. 
    We recommend to use grid.two at least 20. We find that this two-step grid search 
    algorithm works much more accurate and efficient than the traditional one-step grid search
     algorithm. 
}

\value{
BinomCI gives the estimate of (p1-p2), which is x/n-y/m, and the exact one sided confidence interval.

PairedCI gives the estimate of (p1-p2), which is (n12/-n21)/(n12+t+n21), and the exact one sided confidence interval.
}


\examples{

#lower confidence interval for the difference of two independent proportions with n=4,m=1,x=2,and y=0 in Wang 2010 
BinomCI(4,1,2,0)

#Upper confidence interval for the difference of two independent proportions with n=4,m=1,x=2,and y=0.
BinomCI(4,1,2,0,alternative="Upper")

#lower confidence interval for the difference of two dependent proportions with n12=9,t=n11+n22=20,n21=3 in Wang 2012 
PairedCI(9,20,3)

#lower confidence intervals for the difference of two dependent proportions in the Table 1 of Wang 2012 
PairedCI(3,1,1,confidence.level=0.95)
PairedCI(2,0,2,confidence.level=0.9)

#Upper confidence intervals for the difference of two dependent proportions
PairedCI(3,1,1,alternative="Upper",confidence.level=0.95)
PairedCI(1,1,2,alternative="Upper",confidence.level=0.9,grid.one=40,grid.two=25)

#Two-sided 95\% confidence intervals
u95=BinomCI(10,10,5,2,confidence.level=0.975,alternative='Upper')$ExactOneCI
l95=BinomCI(10,10,5,2,confidence.level=0.975,alternative='Lower')$ExactOneCI
ci95=c(l95[1],u95[2]) 

More examples and details see: http://www.wright.edu/~weizhen.wang/software/ExactTwoProp/examples.pdf
}

\author{Guogen Shan<guogen.shan@unlv.edu>, and Weizhen Wang<weizhen.wang@wright.edu>}

\references{
Wang, W. (2010). On construction of the smallest one-sided confidence interval for the difference of two proportions. The Annals of Statistics, 38, 1227--1243.

Wang, W. (2012). An inductive order construction for the difference of two dependent proportions. Statistical and Probability Letters, 82, 1623--1628.

Buehler, R. (1957). Confidence intervals for the porduct of two binomial parameters. JASA, 52, 482--493.
}

