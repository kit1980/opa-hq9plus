// HQ9+ Interpreter in Opa language

import stdlib.themes.bootstrap

// "99 Bottles of Beer" song
// http://99-bottles-of-beer.net/lyrics.html
song = 
  ~{lyrics ...} =
    for(
      {lyrics="" i=99},
      (~{lyrics i} -> {lyrics="{lyrics}{i} bottles of beer on the wall, {i} bottles of beer.
Take one down, pass it around, {i-1} bottles of beer on the wall.\n\n" i=i-1}),
      (~{lyrics i} -> i > 2)
    )
  "{lyrics}2 bottles of beer on the wall, 2 bottles of beer.
Take one down and pass it around, 1 bottle of beer on the wall.

1 bottle of beer on the wall, 1 bottle of beer.
Take one down and pass it around, no more bottles of beer on the wall.

No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.\n"


// main HQ9+ work, plusses just ignored
hq9plus(source) = 
   rec expr = parser
   | [Hh] ~expr -> "Hello, world!\n{expr}"
   | [Qq] ~expr -> "{source}\n{expr}"
   | [9] ~expr -> "{song}\n{expr}"
   | [+] ~expr -> expr
   | "" -> ""
   expr

// count plusses for hidden accumulator
accumulate = 
   rec acc = parser
   | [+] ~acc -> acc + 1
   | [HhQq9] ~acc -> acc
   | "" -> 0
   acc

hq9plus_interpret() = 
  (output, error) =
    match Parser.try_parse(hq9plus(Dom.get_value(#expr)), Dom.get_value(#expr)) with
    | {some=result} -> (<>{result}</>, <></>)
    | {none} -> (<></>, <br /><div class="alert-message block-message error">Syntax error. Only symbols [HhQq9+] accepted.</>)
  do Dom.transform([#output <- output])
  do Dom.transform([#error <- error])

  // accumulator is just a hidded "feature" of HQ9+
  accumulator =
    match Parser.try_parse(accumulate, Dom.get_value(#expr)) with
    | {some=result} -> (result)
    | {none} -> (0)
  // do print("{accumulator}") // uncomment this line to see hidden value of accumulator
  void

page() =
  <body>
        <div class="container">

             <div class="page-header">
                  <h1>HQ9+ Interpreter in Opa language</>
             </>

             <div>
                <p>
                    <a href="http://progopedia.com/language/hq9-plus/">HQ9+</a> is a joke language  which has only 4 instructions and is thus capable of only 4 actions:
                </>
                <ul>
                    <li><code>H</code> prints “Hello, World!”</li>
                    <li><code>Q</code> prints the text of the source code (thus, single-command code “Q” is a quine)</li>
                    <li><code>9</code> prints the lyrics of “99 Bottles of Beer”</li>
                    <li><code>+</code> increments the accumulator (which you can’t access anyways)</li>
                </>
             </>

             <div class="form-stacked">
                  <label for="expr">Editor</>     
                  <textarea id=#expr class="xxlarge" style="width:100%" rows=5>H</>
                  <span class="help-block">HQ9+ program source</>

                  <button class="btn primary" id=#run onclick={_ -> hq9plus_interpret()}>Run</>
                  <div id=#error />

                  <br />
                  <label>Output</>                      
                  <pre id=#output>&nbsp;</>

             </>
        </>
  </>

server = Server.one_page_bundle("Opa HQ9+ Interpreter", [], [], page)
