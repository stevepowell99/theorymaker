$(document).ready(function(){
  $('body').on('mousedown', 'span', function(evt){
    Shiny.setInputValue('span_info', evt.target.id);
  });
})
