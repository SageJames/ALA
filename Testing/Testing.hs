{-
    Programming Languages Final Projec: ALA Compiler

    Description:
        ALA is an environment protocol language that will be able to produce a file system environment based 
        on the specification given from an end user. Typically, interactions will involve a user creating a file 
        following the general syntax of the ALA programming language, placing the .ala file as an argument to the 
        compiler’s executable file (./ala), and pressing enter to allow the program to execute. After execution, 
        the program will either return an error showing where a conversion or system error occurred, or it will 
        return an environment within the user's linux system.

    Created by: Alcinder Lewis 
-}

----[Enviroment Section]-----------------------------------------------------------------------------
{- 
    Description:
        This section holds all of factors that contribute the other two follwing section being sucessful.
        Everything that is needed by the other two section is made and maintainted here.

    Subsections:
        Module Declaration:
            This is to call the module as main
        Imports:
            This is where all of the import are located
        Root Protion:
            This is were all of the root elements are located (String of the root and if there can be a overwrite)
        Protocal Protion:
            The element that make up a individual Protocal (Name, Priory, Passing calls, and a compact individual instance).
        Block Protion:
            All of the types that my us the differnet types of blocks (Name and Var to Child and Elements).
        Blocks Datatype:
            All of the data types that a block can be.
        Tokens Datatype: 
            All of the token that can come from lexing.
-}

-- Module Declaration
-- module Main where

-- Imports
import Data.Char
import System.IO 

-- Root Protion
type Root = String
type Overwrite = String

-- Protocal Protion
type Pname = String  
type Priory = Int    
type Passing = Bool  
type Protocal = (Pname,(Priory,Passing)) 

-- Block Protion 
type Vars = String 
type Name = String
type Child = (Name,Vars)
type Element = (Vars,(Pname, (Name,[Child])))

-- Blocks Datatype
data Blocks = RootBlock Root Element | Folder Element | File Child | Forward | BlockError [Tokens]
            deriving (Show, Eq)

-- Tokens Datatype
data Tokens = Sym String | Lbar | Rbar | Colon | RootT | ProtocolT | ForwardT
                | Lpar | Rpar | FolderT | FileT 
                | VarT | OvT | Comment | Err String
                deriving (Show, Eq)

----[Functions Section]------------------------------------------------------------------------------
{-
    Description:
        This section houses all of the fuctions that contribute to the main convertion of the strings to all the 
        data structures that are used within this project. 

    Subsection:
        Lexer: 
            This function convert a string to an array of tokens.
        MakeBlocks:
            Converts an array of token into an array of blocks and forwards. [Block] Forward (->).
        FindAll (Folder Oriented):
            Takes a block, array of blocks, and return an array of all the instances. (Big helper method)
        DetermineRoot:
            Looks for the single instance of Root (Uses FindAll) and set it to the Root term for the entire program.
        Consolidate:
            Populates the child array of each element and make only a single instance of that particular block (Uses FindAll).
            This should still be array of blocks and not more forwards.
        GetChildren:
            Finds all of the children of a particular block and return all of them inside of a array.
        FindOne (Folder Oriented):
            Find a single instance of a block element
        MakeTree:
            Create a FTree from an array of blocks (Uses FindOne).
        FinalStruct:
            Takes the FTree and makes that tree into a File strucutre at the location of the root.
-}

-- lexer
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
lexer (' ': ts) = lexer ts
lexer (x:xs) | (isLower x) = scanSym [x] xs
                             where  scanSym i "" = [Sym (i)] 
                                    scanSym i (x:xs) | isAlphaNum x = scanSym (i ++ [x]) xs
                                                     | otherwise = Sym (i) : lexer (x:xs)
lexer s = [Err s]


-- makeBlocks
makeBlocks:: [Tokens] -> [Blocks]
makeBlocks [] = []
makeBlocks (Lbar: RootT : Colon: Sym a: Colon: OvT :Colon: Sym b: Rbar: ts) = RootBlock (a) ("RootVar",("RootP", (a, []))) : makeBlocks ts 
makeBlocks (Lbar: FolderT: Colon: Sym name: Colon: VarT: Sym vname: Colon:ProtocolT: Lpar: Sym pname: Rpar: Rbar: ts) = Folder (vname,(pname,(name,[]))) : makeBlocks ts
makeBlocks (Lbar: FileT :Colon: Sym a:Colon: VarT: Sym b: Rbar: ts) = File (a, b): makeBlocks ts
makeBlocks (ForwardT: ts) = Forward : makeBlocks ts
makeBlocks s = [BlockError s]

-- determineRoot
determineRoot:: [Blocks] -> Blocks
determineRoot [] = error "There is no root within this structure."
determineRoot (Forward: RootBlock x xs: ts) = error "The root is not the first element in this list"
determineRoot (RootBlock x xs: ts) =  moreThanOne (RootBlock x xs) ts 
                                      where moreThanOne f [] = f
                                            moreThanOne f (RootBlock t tz :xs) = error "There is more than one Root"
                                            moreThanOne f (x:xs) = moreThanOne f xs 
determineRoot (x:xs) = determineRoot xs


-- populateChildren
populateChildren::[Blocks] -> [Blocks] -> [Blocks] -> [Blocks]
populateChildren [] ts tf = tf
populateChildren (Folder x :xs) ts tf = if (notElem (Folder x) ts) then populateChildren xs (Folder x :ts) (Folder (findPointF x xs): tf) else populateChildren xs ts tf
populateChildren (RootBlock r x : xs) ts tf = populateChildren xs ts (RootBlock r (getChildren x xs): tf)
populateChildren (x:xs) ts tf = populateChildren xs ts tf

--findPointF
findPointF:: Element -> [Blocks] -> Element
findPointF d [] = d
findPointF d (x:xs) = if (Folder d) == x then getChildren d xs else findPointF d xs


-- getChildren
getChildren:: Element -> [Blocks] -> Element 
getChildren f (Forward: File x :ts) = getChildren (modElem f (File x)) ts
getChildren f (Forward: Folder x :ts) = getChildren (modElem f (Folder x)) ts
getChildren f s = f

-- modElem
modElem:: Element -> Blocks -> Element
modElem f (File x) = ((fst f),((fst $ snd f), ((fst $ snd $ snd f), (x:(snd $ snd $ snd f)))))
modElem f (Folder x) = ((fst f),((fst $ snd f), ((fst $ snd $ snd f), ((fst $ snd $ snd x, fst x ):(snd $ snd $ snd f)))))
modElem f _ = error "The block is not able to be a child"

-- getAllFiles
getAllFiles:: [Blocks] -> [Blocks]
getAllFiles [] = []
getAllFiles (File x : xs) =  File x : getAllFiles xs
getAllFiles (x:xs) = getAllFiles xs

-- getAllFolders
getAllFolders:: [Blocks] -> [Blocks]
getAllFolders [] = []
getAllFolders (Folder x : xs) =  Folder x : getAllFolders xs
getAllFolders (x:xs) = getAllFolders xs



--make sure to have files be added to the childpop folder

----[Main Section]-----------------------------------------------------------------------------------
{-
    Description:
        This is where all the functions and enviromental element are pulled together to make a files input into a
        file structure.
-}
