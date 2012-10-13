import Parser
import Expr

main :: IO ()
main = do
  input <- getContents
  let prog = parse input
  putStrLn $ "Input Program: " ++ (show prog)
  --let result = evalProg prog
  --putStrLn $ "Result: " ++ (show result)
  let final = evalProgFinal prog
  putStrLn $ "Final: " ++ (show final)
