% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_render_downloadable_dataframe.R
\name{conditionalUI}
\alias{conditionalUI}
\title{Conditional UI}
\usage{
conditionalUI(expression, ui_element)
}
\arguments{
\item{expression}{compile-time evaluated expression (no server/reactive variables)}

\item{ui_element}{the UI element to display if expression = TRUE}
}
\value{
if expression==TRUE: taglist wrapping passed UI element. If expression == FALSE, empty taglist
}
\description{
This function takes some UI element and a compiletime-evaluated expression 
and returns the UI element ONLY if the condition is true
}
\examples{
# In UI:
somevariable=TRUE
shinyWidgets::panel(
  heading="constitutivepanel",
  CRUX:::conditionalUI(
    somevariable, 
    shinyWidgets::panel(somevariable, heading="ConditionalPanel")
  ),
                    
  shiny::h1("constitutive title"),
  shiny::p("constitutive paragraph")
)
}
