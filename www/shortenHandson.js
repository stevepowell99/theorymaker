function shortenHandson(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.innerHTML = '<div class="fixed">' + td.innerHTML + '</div>'
}