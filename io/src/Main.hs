import Parser
import Expr

main :: IO ()
main = do
  input <- getContents
  let prog = parse input
  putStrLn $ "Input Program: " ++ (show prog)
  evalProg prog
