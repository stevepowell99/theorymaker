$(document).on("click", ".edge", function(e) {
if(typeof BUTTON_CLICK_COUNT === "undefined") {
BUTTON_CLICK_COUNT = 1;
} else {
BUTTON_CLICK_COUNT++;
}
Shiny.setInputValue("js_edge_clicked",
e.currentTarget.id + "_" + BUTTON_CLICK_COUNT);
});
