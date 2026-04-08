#' @title Create a loading spinner
#'
#' @description To be used within Shiny UI
#'
#' @noRd
#' @return html
#' @author Chad Murchison, \email{cfmurch@uab.edu}
#' 
#' @examples
#' \dontrun{
#' preloader_spinner()
#' }
preloader_spinner <- function(){
  list(
    color = "#006400",#ADRCDashHelper::color_palette$green_dark,
    html = shiny::tagList(
      tags$img(src = 'www/logo-watermark.png',
               style = 'width: 200px; margin-bottom: 40px',
               alt = 'UAB logo'),
      br(),
      waiter::spin_ellipsis()
    )
  )
}
