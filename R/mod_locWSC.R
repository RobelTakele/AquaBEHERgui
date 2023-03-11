#' locWSC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_locWSC_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' locWSC Server Functions
#'
#' @noRd 
mod_locWSC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_locWSC_ui("locWSC_1")
    
## To be copied in the server
# mod_locWSC_server("locWSC_1")
