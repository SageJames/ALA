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
        Imports:
        Types:
        Data: 
-}

-- Module Declaration
-- Imports
-- Types
-- Data

----[Functions Section]------------------------------------------------------------------------------
{-
    Description:
        This section houses all of the fuctions that contribute to the main convertion of the strings to all the 
        data structures that are used within this project. 

    Subsection:
        Lexer: 
        MakeBlocks:
        FindAll:
        DetermineRoot:
            (Uses FindAll)
        Consolidate:
            GetChildren:
            (Uses FindAll)
        MakeTree:
        FindOne:
-}



----[Main Section]-----------------------------------------------------------------------------------
{-
    Description:
        This is where all the functions and enviromental element are pulled together to make a files input into a
        file structure.
-}
