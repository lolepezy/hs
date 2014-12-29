
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
jvalue = jarray <|> jobject 
jany = jvalue <|> jstring

jobject :: Parser JSONValue
jobject = do 
  (char '{') 
  f <- sepBy (do
      char '\''  
      key <- many alphaNum
      char '\''
      char ':'
      jval <- jany
      return (key, jval) 
    ) (char ',')
  (char '}') <?> "} at end of object"
  return $ JSONObject f
  
jarray :: Parser JSONValue
jarray = do
  (char '[' >> spaces) 
  f <- sepBy jany (char ',')
  (spaces >> char ']') <?> "] at end of array"
  return $ JSONArray f

jstring = do 
  s <- many1 alphaNum
  return $ JSONString s
  
parseJSON s = parse jvalue "json" s

