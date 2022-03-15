$(function() {
    $('.lazy').Lazy();
    // Captions should be margin notes by default.
    $('figcaption').addClass('marginnote');
});

mermaid.initialize(
    { startOnLoad: true,
      securityLevel: 'loose' // needed for html inlines
    }
);
