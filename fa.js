var fa = document.createElement('style');
    fa.type = 'text/css';
    fa.textContent = '@font-face { font-family: FontAwesome; src: url("'
        + chrome.extension.getURL('fontawesome-webfont.woff') //?v=4.7.0')
        + '"); }';
console.log(fa.textContent);
document.head.appendChild(fa);

