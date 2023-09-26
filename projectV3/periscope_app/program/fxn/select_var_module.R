select_var_ui <- function(id) {
    ns <- NS(id) # Creates Namespace
    
    tagList(
        uiOutput(ns("DynamicContent"))
    )
}

select_var_server <- function(id, label, choices) {
    moduleServer(id, function(input, output, session) {
        output[["DynamicContent"]] <- renderUI({
            selectInput(session$ns("S_A_Input"),
                label, choices = choices
            )
        })
    })
}