shinyjs.c2c=function() {
  function listener(e) {
    e.clipboardData.setData("text/html", document.getElementById('gt').innerHTML);
    e.clipboardData.setData("text/plain", document.getElementById('gt').innerHTML);
    e.preventDefault();
  }
  document.addEventListener("copy", listener);
  document.execCommand("copy");
  document.removeEventListener("copy", listener);
};


