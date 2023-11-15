$(document).on("shiny:connected", function(){
  Shiny.addCustomMessageHandler("focus", function(x){
    $("#factor1_main").click();
  });
});
