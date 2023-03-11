#' locPET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_locPET_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' locPET Server Functions
#'
#' @noRd 
mod_locPET_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_locPET_ui("locPET_1")
    
## To be copied in the server
# mod_locPET_server("locPET_1")
