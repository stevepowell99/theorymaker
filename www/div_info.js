$(document).ready(function(){
  $('body').on('mousedown', 'span', function(evt){
    Shiny.setInputValue('div_info', evt.target.parentElement.id);
  });
})
