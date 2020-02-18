module TestsInferenceB
where

import Parser
import TestPP
import Util
import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

testInfNoFunc :: TestPP()
testInfNoFunc = testOne 0 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfNoFuncBonus" 1
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test0_infer.in"

                    varsForGlobal = sort [["a", "Double"], ["b", "String"], ["c", "Int"]]

                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent


testInfNoInstance :: TestPP ()
testInfNoInstance = testOne 1 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfNoInstanceBonus" 1
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test1_infer.in"

                    varsForGlobal = sort [["a", "Double"], ["b", "String"], ["c", "Int"]]

                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfNoVarParam :: TestPP ()
testInfNoVarParam = testOne 2 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfNoVarParamBonus" 1
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test2_infer.in"

                    varsForGlobal = sort [["a", "A"], ["b", "String"], ["c", "Int"]]

                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfNoFuncParam :: TestPP ()
testInfNoFuncParam = testOne 3 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfNoFuncParamBonus" 1
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test3_infer.in"

                    varsForGlobal = sort [["a", "A"], ["b", "String"], ["c", "Int"]]

                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfSuccess :: TestPP ()
testInfSuccess = testOne 4 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfSuccessBonus" 2
            where
                progEmpty = initEmptyProgram
                fileContent = unsafePerformIO $ getInputTest "test4_infer.in"

                varsForGlobal = sort [["a", "A"], ["b", "String"], ["c", "Int"], ["e", "Double"]]

                finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfParentFuncSuccess = testOne 5 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfParentFuncSuccessBonus" 2
                    where
                        progEmpty = initEmptyProgram
                        fileContent = unsafePerformIO $ getInputTest "test5_infer.in"

                        varsForGlobal = sort [["a", "A"], ["b", "Double"], ["c", "String"], ["d", "String"]]

                        finalProg = foldl (flip interpret) progEmpty $ parse fileContent


testInfSuccessive :: TestPP ()
testInfSuccessive = testOne 6 $ testVal (sort $ getVars finalProg) varsForGlobal "TestInfSuccessiveBonus" 2
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test6_infer.in"

                    varsForGlobal = sort [["a", "A"], ["b", "String"], ["c", "Int"], ["e", "Double"], ["f", "Float"]]

                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInfComplexSuccess :: TestPP()
testInfComplexSuccess = tests 7 10 [
                             testCond (elem ["expr60", "Double"] vars) "TestInfComplexSuccessBonus0" 1,
                             testCond (elem ["expr61", "Double"] vars) "TestInfComplexSuccessBonus1" 1,
                             testCond (elem ["expr62", "Double"] vars ) "TestInfComplexSuccessBonus2" 1,
                             testCond (not $ elem "expr63" onlyName) "TestInfComplexSuccessBonus3" 1,
                             testCond (not $ elem "expr64" onlyName) "TestInfComplexSuccessBonus4" 1,
                             testCond (elem ["expr65", "PA"] vars) "TestInfComplexSuccessBonus5" 1,
                             testCond (elem ["expr66", "PC"] vars) "TestInfComplexSuccessBonus6" 1,
                             testCond (elem ["expr67", "PC"] vars) "TestInfComplexSuccessBonus7" 1,
                             testCond (not $ elem "expr68" onlyName) "TestInfComplexSuccessBonus8" 1,
                             testCond (elem ["expr69", "AA"] vars) "TestInfComplexSuccessBonus9" 1
                    ]
                     where
                        progEmpty = initEmptyProgram
                        fileContent = unsafePerformIO $ getInputTest "test7_infer.in"
                        finalProg = foldl (flip interpret) progEmpty $ parse fileContent
                        vars = getVars finalProg
                        onlyName = map head vars

parseInfTestsB = [
                    testInfNoFunc, testInfNoInstance, testInfNoVarParam, testInfNoFuncParam, testInfSuccess,
                    testInfParentFuncSuccess, testInfSuccessive, testInfComplexSuccess
                 ]
