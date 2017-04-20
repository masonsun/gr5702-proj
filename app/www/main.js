Shiny.addCustomMessageHandler("refresh_message",
  function(message) {
    alert(JSON.stringify(message));
  });