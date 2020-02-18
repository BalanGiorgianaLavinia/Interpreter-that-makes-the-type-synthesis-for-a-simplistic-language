module TestsInference
where

import Parser
import Util
import TestPP
import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

-- Get the Tests as Expressions
import InputInference

testInfNoFunc :: TestPP()
testInfNoFunc = testOne 0 $ testVal (infer expr0 finalProg) Nothing "TestInfNoFunc" 2
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test0_infer_prog.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfNoInstance :: TestPP()
testInfNoInstance = testOne 1 $ testVal (infer expr1 finalProg) Nothing "TestInfNoInstance" 2
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test1_infer_prog.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfNoVarParam :: TestPP()
testInfNoVarParam = testOne 2 $ testVal (infer expr2 finalProg) Nothing "TestInfNoVarParam" 2
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test2_infer_prog.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfNoFuncParam :: TestPP()
testInfNoFuncParam = testOne 3 $ testVal (infer expr3 finalProg) Nothing "TestInfNoFuncParam" 2
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test3_infer_prog.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfSuccess :: TestPP()
testInfSuccess = testOne 4 $ testVal (infer expr4 finalProg) (Just "Double") "TestInfSuccess" 3
            where
                progEmpty = initEmptyProgram
                fileContent = unsafePerformIO $ getInputTest "test4_infer_prog.in"
                finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfParentFuncSuccess :: TestPP()
testInfParentFuncSuccess = testOne 5 $ testVal (infer expr5 finalProg) (Just "String") "TestInfParentFuncSuccess" 4
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test5_infer_prog.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent


testInfComplexSuccess :: TestPP()
testInfComplexSuccess = tests 6 15 [
                            testVal (infer expr60 finalProg) (Just "Double") "TestInfComplexSuccess0" 1,
                            testVal (infer expr61 finalProg) (Just "Double") "TestInfComplexSuccess1" 1,
                            testVal (infer expr62 finalProg) (Just "Double") "TestInfComplexSuccess2" 1,
                            testVal (infer expr63 finalProg) Nothing "TestInfComplexSuccess3" 1,
                            testVal (infer expr64 finalProg) Nothing "TestInfComplexSuccess4" 1,
                            testVal (infer expr65 finalProg) (Just "PA") "TestInfComplexSuccess5" 1,
                            testVal (infer expr66 finalProg) (Just "PC") "TestInfComplexSuccess6" 1,
                            testVal (infer expr67 finalProg) (Just "PC") "TestInfComplexSuccess7" 1,
                            testVal (infer expr68 finalProg) Nothing "TestInfComplexSuccess8" 1,
                            testVal (infer expr69 finalProg) (Just "AA") "TestInfComplexSuccess9" 1
                    ]
                    where
                        progEmpty = initEmptyProgram
                        fileContent = unsafePerformIO $ getInputTest "test6_infer_prog.in"
                        finalProg = foldl (flip interpret) progEmpty $ parse fileContent

infTests = [
            testInfNoFunc, testInfNoInstance, testInfNoVarParam,
            testInfNoFuncParam, testInfSuccess, testInfParentFuncSuccess, testInfComplexSuccess
           ]
