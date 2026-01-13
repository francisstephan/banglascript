copyTextToClipboard = function(id) {
  // source : https://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript
  var text = document.getElementById(id).innerHTML ;
  console.log("text =" + text);
  navigator.clipboard.writeText(text).then(function() {
      console.log('Async: Copying to clipboard was successful!' + text);
  }, function(err) {
      console.error('Async: Could not copy text: ', err);
  });
  document.getElementById("entree").focus();
}