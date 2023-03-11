#' locSWB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_locSWB_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' locSWB Server Functions
#'
#' @noRd 
mod_locSWB_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_locSWB_ui("locSWB_1")
    
## To be copied in the server
# mod_locSWB_server("locSWB_1")
