// help-panels.js - Save this in a 'www' subfolder of your app directory

$(document).ready(function() {
  // Function to initialize all help panel click handlers
  function initHelpPanels() {
    console.log("Initializing help panels...");
    
    // Help panel toggle
    $('.help-title').off('click').on('click', function() {
      console.log("Help panel clicked");
      $(this).parent().toggleClass('active');
    });
    
    // Display type selector
    $('.display-type-box').off('click').on('click', function() {
      console.log("Display type clicked: " + $(this).data('value'));
      $('.display-type-box').removeClass('active');
      $(this).addClass('active');
      
      // Extract the value from the data attribute
      var value = $(this).data('value');
      
      // Set the value in the hidden select input
      $('#results-display_type').val(value).trigger('change');
    });
  }
  
  // Initialize on page load
  $(document).on('shiny:connected', function() {
    setTimeout(initHelpPanels, 500);
  });
  
  // Re-initialize after any Shiny output updates
  $(document).on('shiny:value', function(event) {
    setTimeout(initHelpPanels, 200);
  });
  
  // Re-initialize when tabs change
  $(document).on('shown.bs.tab', function() {
    setTimeout(initHelpPanels, 200);
  });
  
  // Fallback initialization if events don't trigger
  setTimeout(initHelpPanels, 1000);
});