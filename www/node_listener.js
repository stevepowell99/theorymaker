$(document).on("click", ".node", function(e) {
if(typeof BUTTON_CLICK_COUNT === "undefined") {
BUTTON_CLICK_COUNT = 1;
} else {
BUTTON_CLICK_COUNT++;
}
Shiny.setInputValue("js_node_clicked",
e.currentTarget.id + "_" + BUTTON_CLICK_COUNT);
});
