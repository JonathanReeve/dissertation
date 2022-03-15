$(function() {
    // Engage lazy loading. But does this even work?
    $('.lazy').Lazy();
    // Captions should be margin notes by default.
    $('figcaption').addClass('marginnote');
});

// Initialize Mermaid, for charts.
mermaid.initialize(
    { startOnLoad: true,
      securityLevel: 'loose' // needed for html inlines
    }
);


// Dirty hack to make citations appear inline with color-annotated blocks.
$('div.annotated p > span.citation').parent().css('display', 'inline')
