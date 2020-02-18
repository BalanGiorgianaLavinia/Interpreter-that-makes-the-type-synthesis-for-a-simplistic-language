module TestsParser
where

import Parser
import TestPP
import Util
import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

testEmptyProgramState :: TestPP ()
testEmptyProgramState = tests 0 1  [
                   testVal (getVars progEmpty) [] "initEmptyProgramVars" 1,
                   testVal (getFuncsForClass "Global" progEmpty) [] "initEmptyProgramFuncs" 1
                ]
                where
                    progEmpty = initEmptyProgram

testClass :: TestPP ()
testClass = testOne 1 $ testVal (sort $ getClasses finalProg) expectedRes "TestClasses" 1
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test0.in"
                    expectedRes = sort ["Global", "Double", "Int", "String"]
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testVar :: TestPP ()
testVar = testOne 2 $ testVal (sort $ getVars finalProg) varsForGlobal "TestVars" 1
    where
        progEmpty = initEmptyProgram
        fileContent = unsafePerformIO $ getInputTest "test1.in"

        varsForGlobal = sort [["a", "Double"], ["b", "String"], ["pp", "String"], ["cb", "Double"]]

        finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testInheritance :: TestPP ()
testInheritance = tests 3 8 [
                    testVal (sort $ getClasses finalProg) (sort ["A", "B", "C", "D", "Student", "Global", "NoParent", "Human"]) "TestInheritanceInvalidClass" 1, 
                    testVal (getParentClass "A" finalProg) "C" "TestInheritanceA" 1,
                    testVal (getParentClass "B" finalProg) "A" "TestInheritanceB" 1,
                    testVal (getParentClass "C" finalProg) "Global" "TestInheritanceC" 1,
                    testVal (getParentClass "Human" finalProg) "Global" "TestInheritanceStudent" 1,
                    testVal (getParentClass "Student" finalProg) "Human" "TestInheritanceStudent" 1,
                    testVal (getParentClass "Global" finalProg) "Global" "TestInheritanceGlobal" 1,
                    testVal (getParentClass "NoParent" finalProg) "Global" "TestInheritanceInvalidParent" 1
                ]
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test2.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

testFunction :: TestPP ()
testFunction = tests 5 4 [
                    testVal (sort $ getFuncsForClass "Global" finalProg) funcsForGlobal "TestFuncsGlobal" 1,
                    testVal (sort $ getFuncsForClass "A" finalProg) funcsForA "TestFuncsA" 1,
                    testVal (sort $ getFuncsForClass "B" finalProg) funcsForB "TestFuncsB" 1,
                    testVal (sort $ getFuncsForClass "C" finalProg) funcsForC "TestFuncsC" 1
                ]
                where
                    progEmpty = initEmptyProgram
                    fileContent = unsafePerformIO $ getInputTest "test3.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

                    funcsForGlobal = sort [["f1", "Double", "Double", "Double"], ["f2", "Int", "Double", "String"],
                                           ["f3", "String"], ["f3", "String", "String", "String", "String", "Double", "Double"]]
                    funcsForA = [["aF1", "Double", "Int", "Int"], ["aF2", "String", "String"]]
                    funcsForB = [["bF1", "Double", "Int", "Int"], ["bF2", "Void", "Int", "Int"]]
                    funcsForC = []


testComplex = tests 6 25 [
                    -- Variable Test
                    testVal (sort $ getVars finalProg) vars "TestVars" 1,

                    -- Class Test
                    testVal (sort $ getClasses finalProg) classes "TestClasses" 1,

                    -- Inheritance Test
                    testVal (getParentClass "Professor" finalProg) "Human" "TestInheritanceProfessor" 1,
                    testVal (getParentClass "Human" finalProg) "Global" "TestInheritanceHuman" 1,
                    testVal (getParentClass "Student" finalProg) "Human" "TestInheritanceStudent" 1,
                    testVal (getParentClass "Teacher" finalProg) "Global" "TestInheritanceTeacher" 1,

                    -- Functions Test
                    testVal (sort $ getFuncsForClass "Global" finalProg) funcsForGlobal "TestFuncsGlobal" 1,
                    testVal (sort $ getFuncsForClass "Professor" finalProg) funcsForProfessor "TestFuncsProfessor" 1,
                    testVal (sort $ getFuncsForClass "Student" finalProg) funcsForStudent "TestFuncsStudent" 1,
                    testVal (sort $ getFuncsForClass "Human" finalProg) funcsForHuman "TestFuncsHuman" 1
                ]
                where

                    progEmpty = initEmptyProgram

                    fileContent = unsafePerformIO $ getInputTest "test4.in"
                    finalProg = foldl (flip interpret) progEmpty $ parse fileContent

                    vars = sort [["var1", "Double"], ["var2", "String"], ["var3", "String"],
                                 ["prod", "Void"], ["cool", "Boo"], ["sum", "String"]]
                    classes = sort ["Double", "Int", "String", "Char",
                                    "Void", "Float", "Boo", "Foo",
                                    "Global", "Human", "Professor",
                                    "Student", "A", "Teacher"]
                    varsTypes = sort [
                                    ("var1", "Double"), ("var2", "String"),
                                    ("var3", "String"), ("prod", "Void"),
                                    ("cool", "Boo"), ("sum", "String")
                                ]

                    funcsForGlobal = sort [["apply", "Int", "Int", "Double", "Double"],
                                           ["print", "String"], ["add", "Int", "Int", "Int"],
                                           ["diff", "Double", "Double", "Double"],
                                           ["increment", "Int", "Int"]]
                    funcsForProfessor = sort [["teach", "String", "String"],
                                              ["teach", "Double", "Int"],
                                              ["teach", "Int"],
                                              ["apply", "Double", "Int", "String"]]
                    funcsForStudent = sort [["homework", "Int", "Double", "Double"],
                                            ["homework", "Double", "Int", "Int"],
                                            ["homework", "Double", "Int", "Int", "Int"]]
                    funcsForHuman = sort []

parseInterpretTests = [
                        testEmptyProgramState, testClass, testVar,
                        testInheritance, testFunction, testComplex
                      ]
