$(document).on('dblclick', '.linkydbl', function(e) {
   if(typeof BUTTON_CLICK_COUNT == "undefined") {
      BUTTON_CLICK_COUNT = 1;
    } else {
      BUTTON_CLICK_COUNT ++;
    }
    Shiny.onInputChange("js.button_double_clicked",
      e.target.id + "_" + BUTTON_CLICK_COUNT);
});
