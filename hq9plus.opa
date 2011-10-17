// +-<>[].,

import stdlib.themes.bootstrap

ws(p) = parser
| Rule.ws res=p Rule.ws -> res

calculator =
  rec term = parser
  | f={ws(Rule.float)} -> f
  | {ws(parser "(")} ~expr {ws(parser ")")} -> expr
  and factor = parser
  | ~term "*" ~factor -> term * factor
  | ~term "/" ~factor -> term / factor
  | ~term -> term
  and expr = parser
  | ~factor "+" ~expr -> factor + expr
  | ~factor "-" ~expr -> factor - expr
  | ~factor -> factor
  expr

//bf_simple = parser res=[+-<>.] -> Text.from_character(res)

//bf = parser 
//     | res=bf_simple+ -> res

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
Go to the store and buy some more, 99 bottles of beer on the wall."

hq9plus(source) = 
   rec expr = parser
   | [Hh] ~expr -> "Hello, world!\n{expr}"
   | [Qq] ~expr -> "{source}\n{expr}"
   | [9] ~expr -> "{song}\n{expr}"
   | [+] ~expr -> expr
   | "" -> ""
   expr

accumulate = 
   rec acc = parser
   | [+] ~acc -> acc + 1
   | [HhQq9] ~acc -> acc
   | "" -> 0
   acc

calculate() =
  (output, error) =
    match Parser.try_parse(calculator, Dom.get_value(#expr)) with
    | {some=result} -> (<>{4.0 + result}</>, <></>)
    | {none} -> (<></>, <>Sorry, but I'm a simple calculator that only understands +, -, *, / and parentheses (...).</>)
  do Dom.transform([#output <- output])
  do Dom.transform([#error <- error])
  void

hq9plus_interpret() = 
  (output, error) =
    match Parser.try_parse(hq9plus(Dom.get_value(#expr)), Dom.get_value(#expr)) with
    | {some=result} -> (<>{result}</>, <></>)
    | {none} -> (<></>, <>Syntax error. Only symbols [HhQq9+] accepted.</>)
  do Dom.transform([#output <- output])
  do Dom.transform([#error <- error])

  accumulator =
    match Parser.try_parse(accumulate, Dom.get_value(#expr)) with
    | {some=result} -> (result)
    | {none} -> (0)
  do print("{accumulator}")
  void

page() =
  <body>
        <div class="container">

             <div class="page-header">
                  <h1>HQ9+ Interpreter in Opa language</>
             </>

             <div>
                <p>
                    HQ9+ Interpreter in Opa language.
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
