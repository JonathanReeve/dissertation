// Dirty hack to make citations appear inline with color-annotated blocks.
// Translate to JavaScript:
// $('div.annotated p > span.citation').parent().css('display', 'inline')
document.querySelectorAll('div.annotated p > span.citation').forEach(
    (el) => (el.parentNode.style.display('inline'))
)

// Captions should be margin notes by default.
document.querySelectorAll('figcaption').forEach((el) => el.classList.add('marginnote'))

// Add paragraph numbers
document.querySelectorAll('main p').forEach(
    (p, i) => p.innerHTML += `<span class="paragraphNumber marginnote">${i}</span>`
)

// Initialize Mermaid, for charts.
mermaid.initialize(
    { startOnLoad: true,
      securityLevel: 'loose' // needed for html inlines
    }
)
