module Elm.Html

import IdrisScript
import IdrisScript.Objects
import public Elm.Types
import Elm.Attributes


-- Ported from https://github.com/elm-lang/html/blob/2.0.0/src/Html.elm
%default total
%access public export


data Html : Type -> Type where
  MkHtml : (ref : JSRef) -> Html a
  
%used MkHtml ref


unpack : Html msg -> Ptr
unpack (MkHtml ptr) = ptr
  

implementation Functor Html where
  map f (MkHtml ref) = 
    MkHtml
      $ unsafePerformIO
      $ jscall
        "A2(_elm_lang$virtual_dom$Native_VirtualDom.map, %0, %1)"
        (Ptr -> Ptr -> JS_IO Ptr)
        (believe_me f) ref


||| General way to create HTML nodes. It is used to define all of the helper
||| functions in this library.
|||    div : List (Attribute msg) -> List (Html msg) -> Html msg
|||    div attributes children =
|||        node "div" attributes children
||| You can use this to create custom nodes if you need to create something that
||| is not covered by the helper functions in this library.
node : String -> ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
node tag facts childs =
  MkHtml $ unsafePerformIO $ jscall "A3(_elm_lang$virtual_dom$Native_VirtualDom.node, %0, %1, %2)" (String -> Ptr -> Ptr -> JS_IO Ptr) tag (unpack facts) (unpack childs)


node_ : String -> ElmList (Html msg) -> Html msg
node_ name =
  node name []


||| Works just like `Html.node`, but you add a unique identifier to each child
||| node. You want this when you have a list of nodes that is changing: adding
||| nodes, removing nodes, etc. In these cases, the unique identifiers help make
||| the DOM modifications more efficient.
keyedNode : String -> ElmList (Attribute msg) -> ElmList (String, Html msg) -> Html msg
keyedNode tag facts childs =
  MkHtml
    $ unsafePerformIO
    $ jscall
     "A3(_elm_lang$virtual_dom$Native_VirtualDom.keyedNode, %0, %1, %2)"
     (String -> Ptr -> Ptr -> JS_IO Ptr)
     tag (unpack facts) (unpack childs)



||| Just put plain text in the DOM. It will escape the string so that it appears
||| exactly as you specify.
|||    text "Hello World!"
text : String -> Html msg
text txt =
  MkHtml $ unsafePerformIO $ jscall "_elm_lang$virtual_dom$Native_VirtualDom.text(%0)" (String -> JS_IO Ptr) txt



-- NESTING VIEWS


-- ||| Create a [`Program`][program] that describes how your whole app works.
-- ||| Read about [The Elm Architecture][tea] to learn how to use this. Just do it.
-- ||| Commands and subscriptions make way more sense when you work up to them
-- ||| gradually and see them in context with examples.
-- ||| [program]: http://package.elm-lang.org/packages/elm-lang/core/latest/Platform#Program
-- ||| [tea]: https://guide.elm-lang.org/architecture/
-- program
--   : { init : (model, Cmd msg)
--     , update : msg -> model -> (model, Cmd msg)
--     , subscriptions : model -> Sub msg
--     , view : model -> Html msg
--     }
--   -> Program Never model msg
-- program =
--   VirtualDom.program


-- ||| Create a [`Program`][program] that describes how your whole app works.
-- ||| It works just like `program` but you can provide &ldquo;flags&rdquo; from
-- ||| JavaScript to configure your application. Read more about that [here][].
-- ||| [program]: http://package.elm-lang.org/packages/elm-lang/core/latest/Platform#Program
-- ||| [here]: https://guide.elm-lang.org/interop/javascript.html
-- programWithFlags
--   : { init : flags -> (model, Cmd msg)
--     , update : msg -> model -> (model, Cmd msg)
--     , subscriptions : model -> Sub msg
--     , view : model -> Html msg
--     }
--   -> Program flags model msg
-- programWithFlags =
--   VirtualDom.programWithFlags



-- SECTIONS


||| Represents the content of an HTML document. There is only one `body`
||| element in a document.
body : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
body =
  node "body"

body_ : ElmList (Html msg) -> Html msg
body_ =
  node "body" []


||| Defines a section in a document.
section : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
section =
  node "section"

section_ : ElmList (Html msg) -> Html msg
section_ =
  node "section" []


||| Defines a section that contains only navigation links.
nav : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
nav =
  node "nav"

nav_ : ElmList (Html msg) -> Html msg
nav_ =
  node "nav" []


||| Defines self-contained content that could exist independently of the rest
||| of the content.
article : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
article =
  node "article"

article_ : ElmList (Html msg) -> Html msg
article_ =
  node "article" []


||| Defines some content loosely related to the page content. If it is removed,
||| the remaining content still makes sense.
aside : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
aside =
  node "aside"

aside_ : ElmList (Html msg) -> Html msg
aside_ =
  node "aside" []


||| <h1/>
h1 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h1 =
  node "h1"

h1_ : ElmList (Html msg) -> Html msg
h1_ =
  node "h1" []


||| <h2/>
h2 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h2 =
  node "h2"

h2_ : ElmList (Html msg) -> Html msg
h2_ =
  node "h2" []


||| <h3/>
h3 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h3 =
  node "h3"

h3_ : ElmList (Html msg) -> Html msg
h3_ =
  node "h3" []


||| <h4/>
h4 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h4 =
  node "h4"

h4_ : ElmList (Html msg) -> Html msg
h4_ =
  node "h4" []


||| <h5/>
h5 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h5 =
  node "h5"

h5_ : ElmList (Html msg) -> Html msg
h5_ =
  node "h5" []


||| <h6/>
h6 : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
h6 =
  node "h6"

h6_ : ElmList (Html msg) -> Html msg
h6_ =
  node "h6" []


||| Defines the header of a page or section. It often contains a logo, the
||| title of the web site, and a navigational table of content.
header : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
header =
  node "header"

header_ : ElmList (Html msg) -> Html msg
header_ =
  node "header" []


||| Defines the footer for a page or section. It often contains a copyright
||| notice, some links to legal information, or addresses to give feedback.
footer : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
footer =
  node "footer"

footer_ : ElmList (Html msg) -> Html msg
footer_ =
  node "footer" []


||| Defines a section containing contact information.
address : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
address =
  node "address"

address_ : ElmList (Html msg) -> Html msg
address_ =
  node "address" []


||| Defines the main or important content in the document. There is only one
||| `main` element in the document.
main : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
main =
  node "main"

main_ : ElmList (Html msg) -> Html msg
main_ =
  node "main" []


-- GROUPING CONTENT

||| Defines a portion that should be displayed as a paragraph.
p : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
p =
  node "p"

p_ : ElmList (Html msg) -> Html msg
p_ =
  node "p" []


||| Represents a thematic break between paragraphs of a section or article or
||| any longer content.
hr : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
hr =
  node "hr"

hr_ : ElmList (Html msg) -> Html msg
hr_ =
  node "hr" []


||| Indicates that its content is preformatted and that this format must be
||| preserved.
pre : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
pre =
  node "pre"

pre_ : ElmList (Html msg) -> Html msg
pre_ =
  node "pre" []


||| Represents a content that is quoted from another source.
blockquote : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
blockquote =
  node "blockquote"

blockquote_ : ElmList (Html msg) -> Html msg
blockquote_ =
  node "blockquote" []


||| Defines an ordered list of items.
ol : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
ol =
  node "ol"

ol_ : ElmList (Html msg) -> Html msg
ol_ =
  node "ol" []


||| Defines an unordered list of items.
ul : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
ul =
  node "ul"

ul_ : ElmList (Html msg) -> Html msg
ul_ =
  node "ul" []


||| Defines a item of an enumeration list.
li : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
li =
  node "li"

li_ : ElmList (Html msg) -> Html msg
li_ =
  node "li" []


||| Defines a definition list, that is, a list of terms and their associated
||| definitions.
dl : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
dl =
  node "dl"

dl_ : ElmList (Html msg) -> Html msg
dl_ =
  node "dl" []


||| Represents a term defined by the next `dd`.
dt : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
dt =
  node "dt"

dt_ : ElmList (Html msg) -> Html msg
dt_ =
  node "dt" []


||| Represents the definition of the terms immediately listed before it.
dd : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
dd =
  node "dd"

dd_ : ElmList (Html msg) -> Html msg
dd_ =
  node "dd" []


||| Represents a figure illustrated as part of the document.
figure : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
figure =
  node "figure"

figure_ : ElmList (Html msg) -> Html msg
figure_ =
  node "figure" []


||| Represents the legend of a figure.
figcaption : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
figcaption =
  node "figcaption"

figcaption_ : ElmList (Html msg) -> Html msg
figcaption_ =
  node "figcaption" []


||| Represents a generic container with no special meaning.
div : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
div =
  node "div"

div_ : ElmList (Html msg) -> Html msg
div_ =
  node "div" []


-- TEXT LEVEL SEMANTIC

||| Represents a hyperlink, linking to another resource.
a : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
a =
  node "a"

a_ : ElmList (Html msg) -> Html msg
a_ =
  node "a" []


||| Represents emphasized text, like a stress accent.
em : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
em =
  node "em"

em_ : ElmList (Html msg) -> Html msg
em_ =
  node "em" []


||| Represents especially important text.
strong : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
strong =
  node "strong"

strong_ : ElmList (Html msg) -> Html msg
strong_ =
  node "strong" []


||| Represents a side comment, that is, text like a disclaimer or a
||| copyright, which is not essential to the comprehension of the document.
small : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
small =
  node "small"

small_ : ElmList (Html msg) -> Html msg
small_ =
  node "small" []


||| Represents content that is no longer accurate or relevant.
s : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
s =
  node "s"

s_ : ElmList (Html msg) -> Html msg
s_ =
  node "s" []


||| Represents the title of a work.
cite : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
cite =
  node "cite"

cite_ : ElmList (Html msg) -> Html msg
cite_ =
  node "cite" []


||| Represents an inline quotation.
q : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
q =
  node "q"

q_ : ElmList (Html msg) -> Html msg
q_ =
  node "q" []


||| Represents a term whose definition is contained in its nearest ancestor
||| content.
dfn : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
dfn =
  node "dfn"

dfn_ : ElmList (Html msg) -> Html msg
dfn_ =
  node "dfn" []


||| Represents an abbreviation or an acronym; the expansion of the
||| abbreviation can be represented in the title attribute.
abbr : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
abbr =
  node "abbr"

abbr_ : ElmList (Html msg) -> Html msg
abbr_ =
  node "abbr" []


||| Represents a date and time value; the machine-readable equivalent can be
||| represented in the datetime attribute.
time : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
time =
  node "time"

time_ : ElmList (Html msg) -> Html msg
time_ =
  node "time" []


||| Represents computer code.
code : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
code =
  node "code"

code_ : ElmList (Html msg) -> Html msg
code_ =
  node "code" []


||| Represents a variable. Specific cases where it should be used include an
||| actual mathematical expression or programming context, an identifier
||| representing a constant, a symbol identifying a physical quantity, a function
||| parameter, or a mere placeholder in prose.
var : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
var =
  node "var"

var_ : ElmList (Html msg) -> Html msg
var_ =
  node "var" []


||| Represents the output of a program or a computer.
samp : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
samp =
  node "samp"

samp_ : ElmList (Html msg) -> Html msg
samp_ =
  node "samp" []


||| Represents user input, often from the keyboard, but not necessarily; it
||| may represent other input, like transcribed voice commands.
kbd : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
kbd =
  node "kbd"

kbd_ : ElmList (Html msg) -> Html msg
kbd_ =
  node "kbd" []


||| Represent a subscript.
sub : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
sub =
  node "sub"

sub_ : ElmList (Html msg) -> Html msg
sub_ =
  node "sub" []


||| Represent a superscript.
sup : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
sup =
  node "sup"

sup_ : ElmList (Html msg) -> Html msg
sup_ =
  node "sup" []


||| Represents some text in an alternate voice or mood, or at least of
||| different quality, such as a taxonomic designation, a technical term, an
||| idiomatic phrase, a thought, or a ship name.
i : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
i =
  node "i"

i_ : ElmList (Html msg) -> Html msg
i_ =
  node "i" []


||| Represents a text which to which attention is drawn for utilitarian
||| purposes. It doesn't convey extra importance and doesn't imply an alternate
||| voice.
b : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
b =
  node "b"

b_ : ElmList (Html msg) -> Html msg
b_ =
  node "b" []


||| Represents a non-textual annoatation for which the conventional
||| presentation is underlining, such labeling the text as being misspelt or
||| labeling a proper name in Chinese text.
u : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
u =
  node "u"

u_ : ElmList (Html msg) -> Html msg
u_ =
  node "u" []


||| Represents text highlighted for reference purposes, that is for its
||| relevance in another context.
mark : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
mark =
  node "mark"

mark_ : ElmList (Html msg) -> Html msg
mark_ =
  node "mark" []


||| Represents content to be marked with ruby annotations, short runs of text
||| presented alongside the text. This is often used in conjunction with East Asian
||| language where the annotations act as a guide for pronunciation, like the
||| Japanese furigana.
ruby : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
ruby =
  node "ruby"

ruby_ : ElmList (Html msg) -> Html msg
ruby_ =
  node "ruby" []


||| Represents the text of a ruby annotation.
rt : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
rt =
  node "rt"

rt_ : ElmList (Html msg) -> Html msg
rt_ =
  node "rt" []


||| Represents parenthesis around a ruby annotation, used to display the
||| annotation in an alternate way by browsers not supporting the standard display
||| for annotations.
rp : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
rp =
  node "rp"

rp_ : ElmList (Html msg) -> Html msg
rp_ =
  node "rp" []


||| Represents text that must be isolated from its surrounding for
||| bidirectional text formatting. It allows embedding a span of text with a
||| different, or unknown, directionality.
bdi : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
bdi =
  node "bdi"

bdi_ : ElmList (Html msg) -> Html msg
bdi_ =
  node "bdi" []


||| Represents the directionality of its children, in order to explicitly
||| override the Unicode bidirectional algorithm.
bdo : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
bdo =
  node "bdo"

bdo_ : ElmList (Html msg) -> Html msg
bdo_ =
  node "bdo" []


||| Represents text with no specific meaning. This has to be used when no other
||| text-semantic element conveys an adequate meaning, which, in this case, is
||| often brought by global attributes like `class`, `lang`, or `dir`.
span : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
span =
  node "span"

span_ : ElmList (Html msg) -> Html msg
span_ =
  node "span" []


||| Represents a line break.
br : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
br =
  node "br"

br_ : ElmList (Html msg) -> Html msg
br_ =
  node "br" []


||| Represents a line break opportunity, that is a suggested point for
||| wrapping text in order to improve readability of text split on several lines.
wbr : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
wbr =
  node "wbr"

wbr_ : ElmList (Html msg) -> Html msg
wbr_ =
  node "wbr" []


-- EDITS

||| Defines an addition to the document.
ins : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
ins =
  node "ins"

ins_ : ElmList (Html msg) -> Html msg
ins_ =
  node "ins" []


||| Defines a removal from the document.
del : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
del =
  node "del"

del_ : ElmList (Html msg) -> Html msg
del_ =
  node "del" []


-- EMBEDDED CONTENT

||| Represents an image.
img : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
img =
  node "img"

img_ : ElmList (Html msg) -> Html msg
img_ =
  node "img" []


||| Embedded an HTML document.
iframe : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
iframe =
  node "iframe"

iframe_ : ElmList (Html msg) -> Html msg
iframe_ =
  node "iframe" []


||| Represents a integration point for an external, often non-HTML,
||| application or interactive content.
embed : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
embed =
  node "embed"

embed_ : ElmList (Html msg) -> Html msg
embed_ =
  node "embed" []


||| Represents an external resource, which is treated as an image, an HTML
||| sub-document, or an external resource to be processed by a plug-in.
object : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
object =
  node "object"

object_ : ElmList (Html msg) -> Html msg
object_ =
  node "object" []


||| Defines parameters for use by plug-ins invoked by `object` elements.
param : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
param =
  node "param"

param_ : ElmList (Html msg) -> Html msg
param_ =
  node "param" []


||| Represents a video, the associated audio and captions, and controls.
video : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
video =
  node "video"

video_ : ElmList (Html msg) -> Html msg
video_ =
  node "video" []


||| Represents a sound or audio stream.
audio : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
audio =
  node "audio"

audio_ : ElmList (Html msg) -> Html msg
audio_ =
  node "audio" []


||| Allows authors to specify alternative media resources for media elements
||| like `video` or `audio`.
source : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
source =
  node "source"

source_ : ElmList (Html msg) -> Html msg
source_ =
  node "source" []


||| Allows authors to specify timed text track for media elements like `video`
||| or `audio`.
track : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
track =
  node "track"

track_ : ElmList (Html msg) -> Html msg
track_ =
  node "track" []


||| Represents a bitmap area for graphics rendering.
canvas : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
canvas =
  node "canvas"

canvas_ : ElmList (Html msg) -> Html msg
canvas_ =
  node "canvas" []


||| Defines a mathematical formula.
math : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
math =
  node "math"

math_ : ElmList (Html msg) -> Html msg
math_ =
  node "math" []


-- TABULAR DATA

||| Represents data with more than one dimension.
table : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
table =
  node "table"

table_ : ElmList (Html msg) -> Html msg
table_ =
  node "table" []


||| Represents the title of a table.
caption : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
caption =
  node "caption"

caption_ : ElmList (Html msg) -> Html msg
caption_ =
  node "caption" []


||| Represents a set of one or more columns of a table.
colgroup : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
colgroup =
  node "colgroup"

colgroup_ : ElmList (Html msg) -> Html msg
colgroup_ =
  node "colgroup" []


||| Represents a column of a table.
col : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
col =
  node "col"

col_ : ElmList (Html msg) -> Html msg
col_ =
  node "col" []


||| Represents the block of rows that describes the concrete data of a table.
tbody : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
tbody =
  node "tbody"

tbody_ : ElmList (Html msg) -> Html msg
tbody_ =
  node "tbody" []


||| Represents the block of rows that describes the column labels of a table.
thead : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
thead =
  node "thead"

thead_ : ElmList (Html msg) -> Html msg
thead_ =
  node "thead" []


||| Represents the block of rows that describes the column summaries of a table.
tfoot : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
tfoot =
  node "tfoot"

tfoot_ : ElmList (Html msg) -> Html msg
tfoot_ =
  node "tfoot" []


||| Represents a row of cells in a table.
tr : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
tr =
  node "tr"

tr_ : ElmList (Html msg) -> Html msg
tr_ =
  node "tr" []


||| Represents a data cell in a table.
td : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
td =
  node "td"

td_ : ElmList (Html msg) -> Html msg
td_ =
  node "td" []


||| Represents a header cell in a table.
th : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
th =
  node "th"

th_ : ElmList (Html msg) -> Html msg
th_ =
  node "th" []


-- FORMS

||| Represents a form, consisting of controls, that can be submitted to a
||| server for processing.
form : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
form =
  node "form"

form_ : ElmList (Html msg) -> Html msg
form_ =
  node "form" []


||| Represents a set of controls.
fieldset : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
fieldset =
  node "fieldset"

fieldset_ : ElmList (Html msg) -> Html msg
fieldset_ =
  node "fieldset" []


||| Represents the caption for a `fieldset`.
legend : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
legend =
  node "legend"

legend_ : ElmList (Html msg) -> Html msg
legend_ =
  node "legend" []


||| Represents the caption of a form control.
label : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
label =
  node "label"

label_ : ElmList (Html msg) -> Html msg
label_ =
  node "label" []


||| Represents a typed data field allowing the user to edit the data.
input : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
input =
  node "input"

input_ : ElmList (Html msg) -> Html msg
input_ =
  node "input" []


||| Represents a button.
button : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
button =
  node "button"

button_ : ElmList (Html msg) -> Html msg
button_ =
  node "button" []


||| Represents a control allowing selection among a set of options.
select : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
select =
  node "select"

select_ : ElmList (Html msg) -> Html msg
select_ =
  node "select" []


||| Represents a set of predefined options for other controls.
datalist : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
datalist =
  node "datalist"

datalist_ : ElmList (Html msg) -> Html msg
datalist_ =
  node "datalist" []


||| Represents a set of options, logically grouped.
optgroup : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
optgroup =
  node "optgroup"

optgroup_ : ElmList (Html msg) -> Html msg
optgroup_ =
  node "optgroup" []


||| Represents an option in a `select` element or a suggestion of a `datalist`
||| element.
option : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
option =
  node "option"

option_ : ElmList (Html msg) -> Html msg
option_ =
  node "option" []


||| Represents a multiline text edit control.
textarea : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
textarea =
  node "textarea"

textarea_ : ElmList (Html msg) -> Html msg
textarea_ =
  node "textarea" []


||| Represents a key-pair generator control.
keygen : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
keygen =
  node "keygen"

keygen_ : ElmList (Html msg) -> Html msg
keygen_ =
  node "keygen" []


||| Represents the result of a calculation.
output : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
output =
  node "output"

output_ : ElmList (Html msg) -> Html msg
output_ =
  node "output" []


||| Represents the completion progress of a task.
progress : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
progress =
  node "progress"

progress_ : ElmList (Html msg) -> Html msg
progress_ =
  node "progress" []


||| Represents a scalar measurement (or a fractional value), within a known
||| range.
meter : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
meter =
  node "meter"

meter_ : ElmList (Html msg) -> Html msg
meter_ =
  node "meter" []


-- INTERACTIVE ELEMENTS

||| Represents a widget from which the user can obtain additional information
||| or controls.
details : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
details =
  node "details"

details_ : ElmList (Html msg) -> Html msg
details_ =
  node "details" []


||| Represents a summary, caption, or legend for a given `details`.
summary : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
summary =
  node "summary"

summary_ : ElmList (Html msg) -> Html msg
summary_ =
  node "summary" []


||| Represents a command that the user can invoke.
menuitem : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
menuitem =
  node "menuitem"

menuitem_ : ElmList (Html msg) -> Html msg
menuitem_ =
  node "menuitem" []


||| Represents a list of commands.
menu : ElmList (Attribute msg) -> ElmList (Html msg) -> Html msg
menu =
  node "menu"

menu_ : ElmList (Html msg) -> Html msg
menu_ =
  node "menu" []


||| Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
||| it. Calling `(lazy view model)` delays the call until later. During diffing, we
||| can check to see if `model` is referentially equal to the previous value used,
||| and if so, we just stop. No need to build up the tree structure and diff it,
||| we know if the input to `view` is the same, the output must be the same!
lazy : (a -> Html msg) -> a -> Html msg
lazy f a =
  MkHtml
    $ unsafePerformIO
    $ jscall
     "A2(_elm_lang$virtual_dom$Native_VirtualDom.lazy, %0, %1)"
     (Ptr -> Ptr -> JS_IO Ptr)
     (believe_me f) (believe_me a) 

lazy2 : (a -> b -> Html msg) -> a -> b -> Html msg
lazy2 f a b =
  MkHtml
    $ unsafePerformIO
    $ jscall
     "A3(_elm_lang$virtual_dom$Native_VirtualDom.lazy2, %0, %1, %2)"
     (Ptr -> Ptr -> Ptr -> JS_IO Ptr)
     (believe_me f) (believe_me a) (believe_me b)

lazy3 : (a -> b -> c -> Html msg) -> a -> b -> c -> Html msg
lazy3 f a b c =
  MkHtml
    $ unsafePerformIO
    $ jscall
     "A4(_elm_lang$virtual_dom$Native_VirtualDom.lazy3, %0, %1, %2, %3)"
     (Ptr -> Ptr -> Ptr -> Ptr -> JS_IO Ptr)
     (believe_me f) (believe_me a) (believe_me b)(believe_me c)
