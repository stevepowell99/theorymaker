$(document).on('dblclick', '.rank-list', function(e) {
   if(typeof BUTTON_CLICK_COUNT == "undefined") {
      BUTTON_CLICK_COUNT = 1;
    } else {
      BUTTON_CLICK_COUNT ++;
    }
    Shiny.onInputChange("js.draggable_button_clicked",
    e.currentTarget.id + "_" + BUTTON_CLICK_COUNT);
});
