-- Alcinder Lewis
-- Final Project
-- 11/25/21 

-- module Main where
----IMPORT -----------------------------------------------------------------------------
import Data.Char
import System.IO 

----DATA TYPES --------------------------------------------------------------------------
{-
    Theses are all of the Data that the program will used during the program
-}

-- Types
type Vars = String
type Name = String
type Root = String
type ProtocalN = String
type Overwrite = String
type Child = (Name,Vars)
type Element = (Vars,(ProtocalN, (Name,[Child])))



-- Ftree
-- data Ftree a = Empty | Folder Ftree a Ftree a| Node Ftree a Ftree a| File a | Root String
--                deriving Show

-- Blocks
data Blocks = RootBlock Root Element | Folder Element | File Child | Forward | BlockError [Tokens   ]
            deriving Show

-- Tokens
data Tokens = Sym String | Lbar | Rbar | Colon | RootT | ProtocolT | ForwardT
                | Lpar | Rpar | FolderT | FileT | Continue
                | VarT | OvT | Comment | Err String
                deriving Show

----LEXER--------------------------------------------------------------------------------
{-
    Convert from string to an array of tokens

    Example:
    [Root: string: Ov: string] -> [Folder: string: Var string: Protocal(name)] -> [File: string: Var string] ->
-}

lexer:: String -> [Tokens]
lexer "" = []
lexer ('(' : ts) = Lpar : lexer ts
lexer (')' : ts) = Rpar : lexer ts
lexer (':' : ts) = Colon : lexer ts
lexer ('[' : ts) = Lbar : lexer ts
lexer (']' : ts) = Rbar : lexer ts
lexer ('-':'>':ts) = ForwardT: lexer ts
lexer ('R': 'o': 'o':'t': ts ) = RootT : lexer ts
lexer ('P': 'r': 'o':'t': 'o':'c':'a':'l': ts) = ProtocolT : lexer ts
lexer ('F': 'i': 'l': 'e': ts) =  FileT: lexer ts
lexer ('F': 'o': 'l': 'd': 'e': 'r': ts) = FolderT : lexer ts
lexer ('V':'a' : 'r' : ts) = VarT: lexer ts
lexer ('O':'v': ts) = OvT: lexer ts
-- lexer ('/':'/':ts) = Comment : lexer ts -- Make this work by making it look for the next newline 

lexer (' ': ts) = lexer ts
lexer (x:xs) | (isLower x) = scanSym [x] xs
                             where  scanSym i "" = [Sym (i)] 
                                    scanSym i (x:xs) | isAlphaNum x = scanSym (i ++ [x]) xs
                                                     | otherwise = Sym (i) : lexer (x:xs)
lexer s = [Err s]
----PARSER-------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------
{-
    [MAKEBLOCKS FUNCTION]
    This fuction makes our array of tokens in an array of blocks and forwards.

    RootBlock (Root:String) (Element: (Var: String,(Name: String, [Child:(Name:String, Vars:String)])))
    Folder (Element: (Var: String,(Name: String, [Child:(Name:String, Vars:String)])))
    File (Name:String, Vars:String)
    Forward
-}
makeBlocks:: [Tokens] -> [Blocks]
-- root block
makeBlocks (Lbar: RootT : Sym a: OvT : Sym b: Rbar: ts) = RootBlock a (a,("RootP",("RootVar", []))) : makeBlocks ts 
-- file block
makeBlocks (Lbar: FileT : Sym a: VarT: Sym b: Rbar: ts) = File (a, b): makeBlocks ts
-- folder block 
makeBlocks (Lbar: FolderT: Sym name: VarT: Sym vname: ProtocolT: Lpar: Sym pname: Rpar: Rbar: ts) = Folder (vname,(name,[]))
-- forward
makeBlocks (ForwardT: ts) = Forward : makeBlocks ts
-- error
makeBlocks s = [BlockError s]
-------------------------------------------------------------------------------------------------------------------------------


-- populate the child protion of folders.

-- Helper that can pull every instance of a thing based on it var type and encapulate them in a single array


-- Then make the element

-- Provide two cases for root 
        -- One for Root forward
        -- One for lone root 
-- Pattern match to block forward for all things other than root
-- Pattern match for the last block in a folder


-- Then make the Ftree 

--Start with the root
    -- Make sure that all of the struct elements that in the array map to a location in the tree









-- -- Find helper that takes a child and returns an element
-- find :: [Element] -> Child -> Element


-- createAlpha :: [Element] -> Ftree

{-
    To make the Folder Tree we are going to create a system enviroment that will be sent to a process converter
    This process converter with turn the system enviroment to a FTree.
-}

----EVALUATE-----------------------------------------------------------------------------
{-
    This is where the main section will be evaluated with the fixed set of fuctions and operations on the struct tree
    before making the final tree for IO production.
-}

----TO IO--------------------------------------------------------------------------------
{-
    Convert the final Ftree to a IO file Structure. 
-}


----MAIN---------------------------------------------------------------------------------
{-

-}
-- main :: IO ()
-- main = do
--     putStrLn "File name:"
--     file <- getLine
--     input <- readFile file
--     -- let single = unwords (lines input)
--     -- let result = exec (parser (lexer single)) []
--     -- putStrLn (show result)
