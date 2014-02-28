import qualified Utils
import qualified TodoDB
import qualified System.Environment as Environment
import qualified Data.Yaml.Syck as Yaml


printDescriptions :: [TodoDB.Todo] -> IO ()
printDescriptions = putStr . unlines . TodoDB.todosToString

printTodos :: [String] -> IO ()
printTodos ("-a":xs) = printDescriptionsIfTodos =<< TodoDB.getTodos
printTodos ("a":xs) = printDescriptionsIfTodos =<< TodoDB.getTodos
printTodos ("all":xs) = printDescriptionsIfTodos =<< TodoDB.getTodos
printTodos _ = (printDescriptionsIfTodos . filter (not . TodoDB.done)) =<< TodoDB.getTodos

printDescriptionsIfTodos :: [TodoDB.Todo] -> IO ()
printDescriptionsIfTodos todos = case todos of [] -> putStrLn "No todos, use the 'add' command to add one."
                                               other -> printDescriptions todos

displayHelp :: IO ()
displayHelp = putStrLn "Commands: (l)ist, (a)dd, (h)elp"

handleCLIArguments :: [String] -> IO ()
handleCLIArguments [] = printTodos []
handleCLIArguments ("-l":xs) = printTodos xs
handleCLIArguments ("l":xs) = printTodos xs
handleCLIArguments ("list":xs) = printTodos xs
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
handleCLIArguments other = putStrLn ("I don't know this command: '" ++ unwords other ++ "'\n") >> displayHelp

main = TodoDB.checkDBExist >> Environment.getArgs >>= handleCLIArguments
