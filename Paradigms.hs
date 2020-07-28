import Data.List
import System.IO 

data TextEditor = TextEditor([Char], [Char], [Char], [Char]) deriving (Show)

inputLine :: TextEditor
inputLine = TextEditor("My name is jeff",""," how are you sir?", [])

addChar :: TextEditor -> Char -> TextEditor
addChar (TextEditor(x, h, y, b))  inputChar = (TextEditor(x++ [inputChar], h, y, b))
21:19

import System.IO

data Editor = Editor (String, String, String, String) deriving (Show)

{-
readFile  :: FilePath -> IO String
input <- readFile


writeFile :: FilePath -> String -> IO ()
writeFile = Editor
-}

create :: Editor 
create = (Editor ("Created new", "", "Text Editor", []))

input :: Editor 
input = (Editor ("No feeling", "", "is final", []))

backspace :: Editor -> Editor
backspace (Editor(l, h, o, n)) = 
    if h == []
    then (Editor((reverse (tail (reverse (l)) )), [], o, n))
    else (Editor (l, [], o, n))

remove :: Editor -> Editor
remove (Editor (l, h, o, n)) = 
    if h == []
    then (Editor (l, h, tail (o), n))
    else (Editor (l, [], o, n))

cut :: Editor -> Editor
cut (Editor (l, h, o, n)) = (Editor (l, [], o, h))

copy :: Editor -> Editor
copy (Editor (l, h, o, n)) = (Editor (l, h, o, h))

paste :: Editor -> Editor
paste (Editor (l, h, o, n)) = (Editor ( l++n, [], o,n))

writeCharacter :: Char -> Editor -> Editor
writeCharacter c (Editor (l, h, o, n)) = (Editor ((l++[c]), h, o, n))

home :: Editor -> Editor
home (Editor (l, h, o, n)) = (Editor ([], [], l++h++o, n))

end :: Editor -> Editor
end (Editor (l, h, o, n)) = (Editor (l++h++o, [], [], n))

-- Move 1 character left
moveCursorCharLeft :: Editor -> Editor
moveCursorCharLeft (Editor (l, h, o, n)) =
    if h == []
    then (Editor((reverse (tail ( reverse (l)))), [], [(head (reverse l))]++ o, n))
    else (Editor (l, [], h++o, n))

{-Move 1 character right. There is a while statement here as if you was to highlight something then pressed 1 character left the outcome would
    be different then when not highlighting text. As while highlited you would move to the beginning of the word.
-}
moveCursorCharRight :: Editor -> Editor
moveCursorCharRight (Editor (l, h, o, n)) =
    if h == []
    then (Editor ((l++[(head (o))]), [], tail (o), n))
    else (Editor ((l++h), [], o, n))

-- High light 1 character left    
selectCharLeft :: Editor -> Editor
selectCharLeft (Editor (l, h, o, n)) = (Editor ((reverse (tail (reverse (l)))), [(head (reverse l))]++h, o, n))

-- Highlight 1 character right
selectCharRight :: Editor -> Editor
selectCharRight (Editor (l, h, o, n)) = (Editor (l, (h++[(head (o))]), tail (o), n))

moveCursorWordLeft :: Editor -> Editor
moveCursorWordLeft (Editor ([],[], o, n)) = (Editor ([], [], o, n))
moveCursorWordLeft (Editor (l, [], o, n)) =
    if ([head (reverse (l))]) == " "
    then (Editor (l, [], o, n))
    else moveCursorWordLeft(Editor ((reverse (tail (reverse (l)))), [], ([head (reverse l)]++o), n))
 
moveCursorWordRight :: Editor -> Editor
moveCursorWordRight (Editor (l,[], [], n)) = (Editor (l, [], [], n))
moveCursorWordRight (Editor (l, h, o, n)) =
    if ([head (o)]) == " "
    then (Editor (l, [], o, n))
    else moveCursorWordRight(Editor ((l++[head (o)]), [], tail (o), n))
    
selectWordLeft :: Editor -> Editor
selectWordLeft (Editor ([], h, o ,n)) = (Editor ([], h, o, n))
selectWordLeft (Editor (l, h, o, n)) =
    if ([head (reverse (l))]) == " "
    then (Editor (l, h, o, n))
    else selectWordLeft (Editor ((reverse (tail (reverse (l)))), [(head (reverse l))]++h, o, n))
    
selectWordRight :: Editor -> Editor
selectWordRight (Editor (l, h, [], n)) = (Editor (l, h, [], n))
selectWordRight (Editor (l, h, o, n)) =
    if ([head (o)]) == " "
    then (Editor (l, h, o, n))
    else selectWordRight (Editor (l, (h++[(head (o))]), tail (o), n))

selectAll :: Editor -> Editor
selectAll (Editor (l, h, o, n)) = (Editor ([], l++h++o, [], n))