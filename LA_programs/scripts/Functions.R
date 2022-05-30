to_pop_up <- function(x,y){
  table_programs |> 
    filter(Pais==x) |> 
    pull(y)
}
  
test <- function(x){paste(sep = "<br/>",
                           "<style> div.leaflet-popup-content {width:auto !important;}</style>",
                           "<b>Country</b> :", list_programs[[x]][2,1],
                           "<b>Intervention</b>:",list_programs[[x]][2,2],
                           "<b>Type</b>:",list_programs[[x]][4,4],
                           "<b>Target pop:</b>:",list_programs[[x]][4,3],
                           "<b>Setting</b>:",list_programs[[x]][1,4],
                           "<b>Risk Factors</b>:",list_programs[[x]][2,4],
                           "<b>Protective Factors</b>:",list_programs[[x]][1,4])}
  
pop <- function(x){paste(sep = "<br/>",
                         "<style> div.leaflet-popup-content {width:auto !important;}</style>",
                         "<b>Country</b> :", list_programs[[x]][2,1],
                         "<b>Intervention</b>:",list_programs[[x]][2,2],
                         "<b>Type</b>:",list_programs[[x]][4,4],
                         "<b>Target pop:</b>:",list_programs[[x]][4,3])}
