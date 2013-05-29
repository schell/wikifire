wikifire
========
Syntax
------
A typical document could look like this:

    <html>
        <head>
            <title>Typical Document</title>
        </head>
        <body>
            [[renderTemplate header.html
                brand [[WikiFire]]
                title [[Welcome to Wiki Fire]]
            ]]

            [[renderTemplate body.html 
                content [[This is the body. It can go on for a long time and
                          even have line breaks and "special" characters. If
                          you want to put in <b>html</b> or something, that's cool.]]
                extras  [[Another thing of text here.]]
            ]]

            [[renderTemplate footer.html
                copyright [[Schell Scivally 2013]]
            ]]
        </body>
    </html>

Then, each of those referenced documents could look something like

header.html:

    <header>
        <div id="brand">[[brand]]</div>
        <h1>[[title]]</h1>
    </header>

body.html:
    
    <body>
        [[content]]
    </body>

footer.html:

    <footer>
        <small>[[copyright]]</small>
    </footer>
