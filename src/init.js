function start(){
  var app = Elm.Bangla.init({
    node: document.getElementById('main')
  });
  app.ports.copyToClip.subscribe(function(data) {
    copyTextToClipboard(data);
  });
  app.ports.resetFocus.subscribe(function(data) {
    document.getElementById("entree").focus();
  });
  app.ports.openDic.subscribe(function(data) {
    openDic(data);
  });
}
