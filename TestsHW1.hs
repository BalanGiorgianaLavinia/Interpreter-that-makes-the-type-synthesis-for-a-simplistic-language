import TestsClass
import TestsParser
import TestsInferenceB
import TestsInference
import TestPP

allTests = [
            ("================ Class Tests =================\n", classTests),
            ("================ Interpreter Tests ================\n", parseInterpretTests),
            ("================ Inference Tests ===================\n", infTests),
            ("================ Inference Tests Bonus ===================\n", parseInfTestsB)
           ]

check :: Int -> IO ()

check testNr = do
                putStrLn msg;
                runTestPP $ sequence_ x
            where
                (msg, x) = allTests !! (testNr - 1)

checkAll :: IO ()
checkAll = runTestPP $ sequence_ $ concat $ map snd allTests
