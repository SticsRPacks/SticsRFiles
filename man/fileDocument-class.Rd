\name{fileDocument-class}
\alias{fileDocument-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An S4 class to represent file or dir
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\section{Slots}{

\describe{
\item{\code{type}}{A file type ('file','dir')}

\item{\code{name}}{A file name (with an extension or not)}

\item{\code{dir}}{A file dir or path}

\item{\code{ext}}{A file extension}

\item{\code{con}}{A file connexion}

\item{\code{content}}{A file content}
}}


\usage{
doc <- fileDocument(filepath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
              a file or dir path.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
xmlDocument package
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
