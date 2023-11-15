$(document).on('click', '.linky', function(e) {
   if(typeof BUTTON_CLICK_COUNT == "undefined") {
      BUTTON_CLICK_COUNT = 1;
    } else {
      BUTTON_CLICK_COUNT ++;
    }
    Shiny.onInputChange("js.button_clicked",
      e.target.id + "_" + BUTTON_CLICK_COUNT);
});
