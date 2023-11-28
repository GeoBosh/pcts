\name{mcOptimCore-class}
\Rdversion{1.1}
\docType{class}
\alias{mcOptimCore-class}

\title{Class mcOptimCore}
\description{Class mcOptimCore.}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("mcOptimCore")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{flag.exploring}:}{Object of class \code{logical} ~~ }
    \item{\code{private.counter.fn}:}{Object of class \code{integer} ~~ }
    \item{\code{period}:}{Object of class \code{integer} ~~ }
    \item{\code{len.block}:}{Object of class \code{integer} ~~ }
    \item{\code{n_col}:}{Object of class \code{integer} ~~ }
    \item{\code{r_col}:}{Object of class \code{integer} ~~ }
    \item{\code{cp_col}:}{Object of class \code{integer} ~~ }
    \item{\code{ind.r.eigval}:}{Object of class \code{integer} ~~ }
    \item{\code{r_ind}:}{Object of class \code{integer} ~~ }
    \item{\code{ind.cp.eigval}:}{Object of class \code{integer} ~~ }
    \item{\code{cp_ind}:}{Object of class \code{integer} ~~ }
    \item{\code{filter.order}:}{Object of class \code{integer} ~~ }
    \item{\code{ev_abs}:}{Object of class \code{matrix} ~~ }
    \item{\code{ev_sign}:}{Object of class \code{matrix} ~~ }
    \item{\code{co_r}:}{Object of class \code{matrix} ~~ }
    \item{\code{co_arg}:}{Object of class \code{matrix} ~~ }
    \item{\code{flag.generators}:}{Object of class \code{logical} ~~ }
    \item{\code{par.ind}:}{Object of class \code{ANY} ~~ }
    \item{\code{auto.ind}:}{Object of class \code{ANY} ~~ }
    \item{\code{inf.ind}:}{Object of class \code{ANY} ~~ }
    \item{\code{universe}:}{Object of class \code{ANY} ~~ }
    \item{\code{col.minusinf.ind}:}{Object of class \code{ANY} ~~ }
    \item{\code{col.inf.ind}:}{Object of class \code{ANY} ~~ }
    \item{\code{ind1}:}{Object of class \code{integer} ~~ }
    \item{\code{ind2}:}{Object of class \code{integer} ~~ }
    \item{\code{ind3}:}{Object of class \code{integer} ~~ }
    \item{\code{ind4}:}{Object of class \code{integer} ~~ }
    \item{\code{n1}:}{Object of class \code{integer} ~~ }
    \item{\code{n2}:}{Object of class \code{integer} ~~ }
    \item{\code{n3}:}{Object of class \code{integer} ~~ }
    \item{\code{n4}:}{Object of class \code{integer} ~~ }
    \item{\code{seqn1}:}{Object of class \code{integer} ~~ }
    \item{\code{seqn2}:}{Object of class \code{integer} ~~ }
    \item{\code{seqn3}:}{Object of class \code{integer} ~~ }
    \item{\code{seqn4}:}{Object of class \code{integer} ~~ }
    \item{\code{co}:}{Object of class \code{matrix} ~~ }
    \item{\code{initmodel}:}{Object of class \code{ANY} ~~ }
    \item{\code{wrkmodel}:}{Object of class \code{ANY} ~~ }
    \item{\code{model}:}{Object of class \code{list} ~~ }
    \item{\code{tempnam}:}{Object of class \code{ANY} ~~ }
    \item{\code{param_names}:}{Object of class \code{character} ~~ }
    \item{\code{lo_bound}:}{Object of class \code{ANY} ~~ }
    \item{\code{up_bound}:}{Object of class \code{ANY} ~~ }
    \item{\code{mcss.cnt}:}{Object of class \code{integer} ~~ }
    \item{\code{wrkev}:}{Object of class \code{ANY} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{mclik(param, x, eps)}:}{ ~~ }
    \item{\code{minimBBlu(x, ..., CONDLIK)}:}{ ~~ }
    \item{\code{mcss(param, x, eps)}:}{ ~~ }
    \item{\code{matepshat(param, x, eps)}:}{ ~~ }
    \item{\code{process.mcss.cnt(mclik, mepshat)}:}{ ~~ }
    \item{\code{sigma2hat(param, x, eps)}:}{ ~~ }
    \item{\code{initialize(...)}:}{ ~~ }
    \item{\code{mcparam2optparam()}:}{ ~~ }
    \item{\code{optparam2mcparam(param)}:}{ ~~ }
    \item{\code{minimBB(x, ..., CONDLIK)}:}{ ~~ }
    \item{\code{optimize(optimfn, mcmethod, x, ..., MCBOUNDS)}:}{ ~~ }
    \item{\code{mcsigma2(param, x, eps)}:}{ ~~ }
    \item{\code{minim(x, ..., CONDLIK)}:}{ ~~ }
  }
}
\keyword{internal}
