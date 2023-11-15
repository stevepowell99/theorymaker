$(document).ready(function(){
  $('body').on('mouseover', 'span', function(evt){
    Shiny.setInputValue('span_hover', evt.target.id);
  });
  $('body').on('mouseover', 'div', function(){
    if ($('#check_tooltips').is(':checked')){
    $('[class*="tooltip"]').css({'display': ''})
    } else {
      $('[class*="tooltip"]').css({'display': 'none'})
    }
  });
})
