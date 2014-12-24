
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
  | JSONString String
  | JSONNull
  deriving (Eq, Ord, Show)

jvalue :: Parser JSONValue
jvalue = try jarray <|> try jobject <|> try jstring

jobject :: Parser JSONValue
jobject = do 
  (char '{') 
  f <- fields
  (char '}') <?> "} at end of object"
  return $ JSONObject f
  
jarray :: Parser JSONValue
jarray = do
  (char '[') 
  f <- sepBy jvalue (char ',')
  (char ']') <?> "] at end of array"
  return $ JSONArray f

fields = sepBy (do
    char '\''
    key <- many alphaNum
    char '\''
    char ':'
    jval <- jvalue
    return (key, jval) 
  ) (char ',')

jstring = do 
  s <- many1 alphaNum
  return $ JSONString s
  
parseJSON s = parse jvalue "json" s

