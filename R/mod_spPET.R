#' spPET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spPET_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' spPET Server Functions
#'
#' @noRd 
mod_spPET_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_spPET_ui("spPET_1")
    
## To be copied in the server
# mod_spPET_server("spPET_1")
