
function copyToClipboard(text) {
    var $temp = $("<input>");
    $("body").append($temp);
    $temp.val(text).select();
    document.execCommand("copy");
    $temp.remove();
}

Shiny.addCustomMessageHandler("copystatement", function(selected) {
   // document.execCommand("copy");
   copyToClipboard(selected);
   console.log("copied!");
});




var selection_options = {
"separateWordSearch" : 0,
"acrossElementCs" : 1,
"className" : "highlighted"
};



function getSelectionText() {
    var text = "";
    if (window.getSelection) {
        text = window.getSelection().toString();
    } else if (document.selection) {
        text = document.selection.createRange().text;
    }
    return text;
}

/*document.onmouseup = document.onkeyup = document.onselectionchange = function() {
    var selection = getSelectionText();
    Shiny.onInputChange("highlightedText", selection);
};*/

const getSelectionHtml = function () {
    let html = "";
    if (typeof window.getSelection != "undefined") {
        let sel = window.getSelection();
        if (sel.rangeCount) {
            let container = document.createElement("div");
            for (let i = 0, len = sel.rangeCount; i < len; ++i) {
                container.appendChild(sel.getRangeAt(i).cloneContents());
            }
            html = container.innerHTML;
        }
    } else if (typeof document.selection != "undefined") {
        if (document.selection.type == "Text") {
            html = document.selection.createRange().htmlText;
        }
    }
    return html;
}




$(document).on('mouseup','#textbanner', function(){
  var selected = getSelectionText();
  var htmlText = getSelectionHtml();
//  highlightTextbanner(selected);

if(selected !==''){
    //new Mark(document.querySelector("#textbanner")).unmark().mark(selected, options );
  //$("#textbanner").unmark().mark(selected, options );
   $("#textbanner").unmark().mark(selected, selection_options );
    console.log("selected = "+ selected);
    Shiny.onInputChange("highlightedText", selected);
    Shiny.onInputChange("highlightedHtml", htmlText);
}else{
  console.log("empty selection");
}
});









/*
const highlightTextbanner = function(input, all=false){
  let currentHtml = $('#textbanner div').html();
  if (input != '' & all==false){
      let replaced,
            parsed = input.replace(/^\s|\s+$/g,'');
          console.log(input)
          parsed = parsed.replace(/\<\/span\>$/g, '');
          let span_start = /^\<span[^\>]*\>/g.test(parsed) ? parsed.match(/^\<span[^\>]*\>/g)[0] : '';
          parsed = currentHtml.includes(parsed) ? parsed : parsed.replace(span_start, '');
          
          let span_prepend = /^(?<!\<span[^\>]*\>)\D+\<\/span[^\>]*\>/g.test(parsed) ? '</span>' : '';
          
          let span_end = /\<span[^\>]*\>(?!.*\<\/span\>)/g.test(parsed) ? parsed.match(/\<span[^\>]*\>(?!.*\<\/span\>)/g)[0] : '';
          parsed = currentHtml.includes(parsed) ? parsed : parsed.replace(span_end, '');
          console.log(currentHtml);
          console.log(parsed);
          //let spans = parsed.match(/\<span[^\>]*\>/g);
         // spans = [...new Set(spans)]; //keep only unique
         // let final_parsed = parsed;
         // for (let span of spans){
          //  final_parsed = final_parsed.replace(span,`<!--${span}-->`);
         // }
         // final_parsed = final_parsed.replace(/\<\/span\>/g,`<!--</span>-->`);
        //  console.log(final_parsed)
          console.log(currentHtml.replace(parsed,`${span_prepend}<tspan class="highlighted">${input}</tspan>${span_end}`))
          $('#textbanner div').html(currentHtml.replace(parsed,`${span_prepend}<tspan class="highlighted">${input}</tspan>${span_end}`));
          //$('#textbanner div').html(currentHtml.split(parsed).join(`${span_prepend}<tspan class="highlighted">${input}</tspan>${span_end}`));
  } else if (all==true){
    $('#textbanner div').html(`<tspan class="highlighted">${$('#textbanner div').html()}</tspan>`);
  }
};

const removeSpans = function(){
    $('#textbanner div').find('span[title="\'NA\'"],span[title="\'\'"]').each(function() { // remove useless spans
    $(this).replaceWith(function(){
        return $(this).html();
      })
    });
};

const resetTextbanner = function(){
  removeSpans();
    $('#textbanner .highlighted').each(function(){ 
      $('#textbanner div').html($('#textbanner div').html().replace('<tspan class="highlighted">', ''));
      $('#textbanner div').html($('#textbanner div').html().replace('</tspan>', ''));
    });
    //$('#textbanner div').html($('#textbanner div').html().replace(/\<\!--/g,'').replace(/--\>/g,''))
    
    //merge all identical spans
    $('#textbanner div').contents().each(function(){
    if (this.nodeType != 1) return;
    while (this.nextSibling && this.nextSibling.id == this.id) {
        this.innerHTML = (this.innerHTML + this.nextSibling.innerHTML);
        this.parentNode.removeChild(this.nextSibling);
        }
    });

  console.log('reset')
}

*/


/**************************************************/

/*
$(document).on('mouseup','#displayStatementPanel', function(){
    var selected = getSelectionText();
    //alert(selected);
//  highlightTextbanner(selected);
    //console.log("selected = "+ selected);
    let htmlText = getSelectionHtml();
    resetTextbanner();
    highlightTextbanner(htmlText);
      
    Shiny.onInputChange("highlightedText", selected);
    Shiny.onInputChange("highlightedHtml", htmlText);
      
    console.log(htmlText);
})
*/
//$(document).on('mousedown','#displayStatementPanel', function(){
  //var selected = getSelectionText();
  //highlightTextbanner(selected);
   //console.log("selected = "+ selected);
//   resetTextbanner();
//});



/*
async function load (selectors){ // asynchronous function - wait till #textbanner and #quote are added to page
      for (let selector of selectors){
        while(!document.querySelector(selector)) {
        await new Promise(resolve => setTimeout(resolve, 500));
        }
      }
  return (function (){ // return and execute synchronous function, escape async  
    removeSpans();
   
    $('#displayStatementPanel').on("mousedown", () => { // remove classes initially
      event.stopImmediatePropagation();
     // resetTextbanner();
    });
    
    $('#displayStatementPanel').on("mouseup", () => { // add classes
      event.stopImmediatePropagation();
      let text = getSelectionHtml();
       resetTextbanner(); // inserted here instead of in mousedown
      highlightTextbanner(text);
      console.log(getSelectionHtml())
    });
    
    document.querySelector('#displayStatementPanel').addEventListener("selectstart", () => { // add text to quote only if textbanner is selected
      console.log('select started')
      document.onmouseup = document.onkeyup = document.onselectionchange = function() {
          event.stopImmediatePropagation();
          console.log('selectstart')
          var selection = getSelectionText();
          Shiny.onInputChange("highlightedText", selection);
          Shiny.onInputChange("highlightedHtml", getSelectionHtml());
      };
    });
    
    document.querySelector('#displayStatementPanel').addEventListener("mouseleave", () => { // remove event listener on mouse leave
      console.log('mouseleave')
      document.onmouseup = document.onkeyup = document.onselectionchange = function(){
        
      }
    })
  })();
}


load(['#textbanner', 'textarea#quote']);//textarea#quote

*/
/**************************************************/


 Shiny.addCustomMessageHandler('quote_repeat', function(data) {
     resetTextbanner();
     highlightTextbanner(data);
  });

 Shiny.addCustomMessageHandler('quote_whole', function(data) {
     resetTextbanner();
     highlightTextbanner('', true);
  });
  
 Shiny.addCustomMessageHandler('quote_reset', function(data) {
   resetTextbanner();
});
