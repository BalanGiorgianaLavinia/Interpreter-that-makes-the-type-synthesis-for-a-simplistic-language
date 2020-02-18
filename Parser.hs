module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.String
import Data.List
import Control.Monad (mfilter)
import Data.Char


-- Definire Program
--map cheie-valoare: cheie = numele clasei
--valoare = (numele super-clasei, pereche de variabile si functii)
data Program = Program (Map String (String, ClassState))

initEmptyProgram :: Program
initEmptyProgram = Program (Map.insert "Global" ("Global", initEmptyClass) Map.empty)

getVars :: Program -> [[String]]
getVars (Program prog_map) = getValues (snd (fromJust (Map.lookup "Global" prog_map))) Var

getClasses :: Program -> [String]
getClasses (Program prog_map) = Map.keys prog_map

getParentClass :: String -> Program -> String
getParentClass myClass (Program prog_map) = fst (fromJust (Map.lookup myClass prog_map))

--name e numele clasei din care vreau sa returnez functiile
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass name (Program prog_map) = if cond then functions else [] 
            where
                cond = Map.member name prog_map
                containerVarsFuncs = snd (fromJust (Map.lookup name prog_map))
                functions = getValues containerVarsFuncs Func

-- Instruction poate fi ce considerați voi
type Instruction = [String]

replace a = map $ maybe ' ' id . mfilter (/= a) . Just
fullReplace = replace '=' . replace '(' . replace ')' . replace ':' . replace ','


parseAux :: String -> Instruction
parseAux str = if (isInfixOf "infer" str) then infer_instruction else normal_instruction
    where
        infer_instruction = words str
        normal_instruction = words (fullReplace str)

parse :: String -> [Instruction]
parse "" = []
parse str = map parseAux (lines str)

-- parse str = map words (map fullReplace (lines str))

interpret :: Instruction -> Program -> Program
interpret instr (Program prog_map)
    | instr == [] = Program prog_map

    --caz pentru clasa care extinde alta clasa
    | (head instr) == "class" && condExtends && condParentClass = Program newProg
    
    --caz pentru clasa care NU extinde alta clasa 
    | (head instr) == "class" = Program (Map.insert nameClass ("Global", initEmptyClass) prog_map)
   
    --caz pentru variabile 
    | (head instr) == "newvar" && condVarClass = Program newProgVar 
    

    --caz pentru functii
    --instructiunea incepe cu litera mare
    | condUpperLetter && condExistClass && condTypeClass && condArgs = Program newProgFuncs
    
    --caz pentru infer
    | (head instr) == "infer" && (isJust evaluatedInstruction) = interpret ("newvar":firstVariable:newType:[]) (Program prog_map)

    --cazul in care nu se efectueaza nicio interpretare
    | otherwise = Program prog_map
            where
                --caz pentru infer
                --obtin expresia dupa cuvantul infer si elimin toate spatiile concatenand cuvintele despartite in parse
                deleteMargins xs@(_:_) = tail (init xs); deleteMargins _ = []

                -- functia de la curs putin modificata pentru a tine cont de paranteze
                split text = split' text [] [] 0 0
                split' [] current acc p1 p2 = acc ++ [current]
                split' (c:s) current acc p1 p2
                    | (c == '(') = split' s (current++[c]) acc (1+p1) p2
                    | (c == ')') = split' s (current++[c]) acc p1 (1+p2)
                    | (c == ',') = if parenthesisMatch then splitOk else continue
                    | otherwise = split' s (current++[c]) acc p1 p2
                        where 
                            parenthesisMatch = p1 == p2
                            splitOk = split' s [] (acc++[current]) 0 0
                            continue = split' s (current++[c]) acc p1 p2

                transformForEval [] = []
                transformForEval (e:l)
                    | (elem '.' e) = (FCall variable functionName args):(transformForEval l)
                    | otherwise = (Va e):(transformForEval l)
                        where
                            pair1 = break (=='.') e
                            variable = fst pair1
                            expression2 = tail (snd pair1)
                            funcAndArgs = break (=='(') expression2
                            functionName = fst funcAndArgs
                            args = transformForEval (split (deleteMargins (snd funcAndArgs)))

                expression = ((filter (/=' ')) . unwords) (tail instr)
                pairExpression = break (=='=') expression
                firstVariable = fst pairExpression
                expression2 = tail (snd pairExpression)
                pairExpression2 = break (=='.') expression2
                secondVariable = fst pairExpression2
                funcAndArgs = break (=='(') (tail (snd pairExpression2))
                functionName = fst funcAndArgs
                args = transformForEval (split (deleteMargins (snd funcAndArgs)))
                evaluatedExpressions = map fromJust (map ((flip infer) (Program prog_map)) args)
                evaluatedInstruction = infer (FCall secondVariable functionName args) (Program prog_map)
                newType = fromJust evaluatedInstruction
            

                --caz clasa care extinde alta clasa 
                condExtends = length instr > 2
                condParentClass = Map.member (head (drop 3 instr)) prog_map

                --programul cu noua clasa si parintele ei
                newProg = Map.insert (head (tail instr)) (head (drop 3 instr), initEmptyClass) prog_map


                --caz clasa care NU extinde alta clasa
                nameClass = head (tail instr)


                --cazul variabilelor
                --verificare daca clasa exista in program
                condVarClass = Map.member (head (drop 2 instr)) prog_map
                classState = snd (fromJust (Map.lookup "Global" prog_map))
                instruction = (head (drop 1 instr)):(head (drop 2 instr)):[]
                newClassState = insertIntoClass classState Var instruction

                newProgVar = Map.insert "Global" ("Global", newClassState) prog_map


                --cazul functiilor
                condUpperLetter = isUpper (head (head instr))
                condExistClass = Map.member (head (tail instr)) prog_map
                --conditie pentru existenta clasei specifice tipului de return al functiei
                condTypeClass = Map.member (head instr) prog_map
                condArgs = inClasses (drop 3 instr) prog_map

                newProgFuncs = Map.insert nameClass newValue prog_map
                newValue = (parentClass, insertIntoClass oldClass Func instructionList)
                parentClass = getParentClass nameClass (Program prog_map)
                oldClass = (snd (fromJust (Map.lookup nameClass prog_map)))
                instructionList = ((head (drop 2 instr)):(head instr):(drop 3 instr))

                --functie care verifica existenta claselor din lista de parametrii
                inClasses [] _ = True
                inClasses (l:list) prog_map = (Map.member l prog_map) && (inClasses list prog_map)


infer :: Expr -> Program -> Maybe String
infer (Va var) (Program prog_map) = varType var (getVars (Program prog_map))
                    where
                        varType _ [] = Nothing
                        varType var (l:list) = if (var == head l) then (Just (head (tail l))) else (varType var list) 

--var e string, function e string , exprList e o lista de expresii care reprezinta parametrii functiei 
infer (FCall var function exprList) p@(Program prog_map) = if inferCondition then result else Nothing
    where
        inferCondition = (isJust infer_var) && ((null list) == False) && (notElem Nothing infer_expr) && (isJust result)
        result = Just (head (tail (head list)))
        infer_var = infer (Va var) p
        class_name = fromJust infer_var
        list = filter (isSubsequenceOf args) (function_list class_name prog_map)
        infer_expr = map ((flip infer) p) exprList
        args = if (elem Nothing infer_expr) then [] else (map fromJust infer_expr)
        function_list class_name prog_map = if (class_name /= "Global") && (Map.member class_name prog_map) then resultInfer else []
            where
                resultInfer = functionsFromClass ++ functionsFromParent
                functionsFromClass = getFuncsForClass class_name (Program prog_map)
                functionsFromParent = function_list (getParentClass class_name (Program prog_map)) prog_map