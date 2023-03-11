#' spSWB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spSWB_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' spSWB Server Functions
#'
#' @noRd 
mod_spSWB_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_spSWB_ui("spSWB_1")
    
## To be copied in the server
# mod_spSWB_server("spSWB_1")
