#' spWSC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spWSC_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' spWSC Server Functions
#'
#' @noRd 
mod_spWSC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_spWSC_ui("spWSC_1")
    
## To be copied in the server
# mod_spWSC_server("spWSC_1")
