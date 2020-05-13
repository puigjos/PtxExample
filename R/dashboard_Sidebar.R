
dashboard_Sidebar <- function(){
  shinydashboard::dashboardSidebar(
    mod_menu_module_ui("menu_module_ui_1")
  )
}