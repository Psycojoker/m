import qualified Utils
import qualified TodoDB
import qualified System.Environment as Environment
import qualified Data.Yaml.Syck as Yaml


printDescriptions :: [TodoDB.Todo] -> IO ()
printDescriptions = putStr . unlines . TodoDB.todosToString

printTodos :: IO ()
printTodos = printDescriptionsIfTodos =<< TodoDB.getTodos
    where printDescriptionsIfTodos todos = case todos of [] -> putStrLn "No todos, use the 'add' command to add one."
                                                         other -> printDescriptions todos

displayHelp :: IO ()
displayHelp = putStrLn "Commands: (l)ist, (a)dd, (h)elp"

handleCLIArguments :: [String] -> IO ()
handleCLIArguments [] = printTodos
handleCLIArguments ("-l":xs) = printTodos
handleCLIArguments ("l":xs) = printTodos
handleCLIArguments ("list":xs) = printTodos
handleCLIArguments ("-a":xs) = TodoDB.addTodo $ unwords xs
handleCLIArguments ("a":xs) = TodoDB.addTodo $ unwords xs
handleCLIArguments ("add":xs) = TodoDB.addTodo $ unwords xs
handleCLIArguments ("-d":xs) = TodoDB.searchAndMarkTodoAsDone $ unwords xs
handleCLIArguments ("d":xs) = TodoDB.searchAndMarkTodoAsDone $ unwords xs
handleCLIArguments ("done":xs) = TodoDB.searchAndMarkTodoAsDone $ unwords xs
handleCLIArguments ("-h":xs) = displayHelp
handleCLIArguments ("--help":xs) = displayHelp
handleCLIArguments ("h":xs) = displayHelp
handleCLIArguments ("help":xs) = displayHelp
handleCLIArguments other = (putStrLn $ "I don't know this command: '" ++ (unwords other) ++ "'\n") >> displayHelp

main = TodoDB.checkDBExist >> Environment.getArgs >>= handleCLIArguments
