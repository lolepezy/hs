
module JsonPlay where

import Text.ParserCombinators.Parsec

{-
object
    {}
    { members } 
members
    pair
    pair , members
pair
    string : value
array
    []
    [ elements ]
elements
    value
    value , elements
value
    string
    number
    object
    array
    true
    false
    null

string
    ""
    " chars "
chars
    char
    char chars
char
    any-Unicode-character-
        except-"-or-\-or-
        control-character
    \"
    \\
    \/
    \b
    \f
    \n
    \r
    \t
    \u four-hex-digits 
number
    int
    int frac
    int exp
    int frac exp 
int
    digit
    digit1-9 digits
    - digit
    - digit1-9 digits 
frac
    . digits
exp
    e digits
digits
    digit
    digit digits
e
    e
    e+
    e-
    E
    E+
    E-

-}

data JSONValue = JSONObject [(String, JSONValue)] 
  | JSONArray [JSONValue]
  | JSONString
  | JSONNull

jvalue :: GenParser Char st JSONValue
jvalue = jobject <|> jarray <|> jstring

jobject = do 
  (char '{') 
  f <- many fields
  (char '}') <?> "} at end of object"
  return JSONNull
  
jarray = do
  (char '[') 
  f <- sepBy jvalue ','
  (char ']') <?> "] at end of array"
  return JSONNull

fields = sepBy (jkeyValue) (char ',')
jkeyValue = do
  key <- jkey
  (char ':')
  val <- jvalue
  return (key, val)

jkey = jquotedKey <|> identifier

jquotedKey = do
  char '\''
  i <- identifier
  char '\''
  return i

jstring = many alphaNum
identifier = many alphaNum
