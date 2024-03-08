module Main where

import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.IO
import           System.Environment
import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.List
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Char
import           Data.Either
import           Text.ParserCombinators.Parsec  ( Parser
                                                , parse
                                                )
import           Common
import           PrettyPrinter
import           Eval
import           Parser

---------------------
-- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S M.empty "")

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

iname, iprompt, intersection :: String
iname = "Álgebra Relacional"
iprompt = "ReAl> "
intersection = "examples/intersection.real"

data State = S
  { env :: Env,      -- Entorno que asocia variables a tablas u operaciones
    lfile :: String  -- Último archivo cargado (para hacer "reload")
  }

-- Read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S env lfile) =
  let rec st = do
        mx <- MC.catch
          (getInputLine iprompt)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (intersection : args) state
        lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  Entra al loop
        rec state'

data Command = Compile CompileForm
             | Print String
             | Recompile
             | Browse
             | Save String
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if ":" `isPrefixOf` x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
    --  Busca comandos
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda"
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ intercalate ", " [ head cs | Cmd cs _ _ _ <- matching ]
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S env lfile) cmd = case cmd of
  Quit   -> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    let (tables, operators) = getNames env
    putStrLn "Tablas:"
    putStrLn (unlines $ tables)
    putStrLn "Operaciones:"
    putStr (unlines $ operators)
    return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile (state { lfile = f }) f
    return (Just state')
  Print s   -> printVar env s >> return (Just state)
  Recompile -> if null lfile
    then lift $ putStrLn "No hay un archivo cargado" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))
  Save s    -> do
    let (n, f') = break isSpace s
        f       = dropWhile isSpace f'
    saveToFile env n f
    return (Just state)

-- Dado un entorno, devuelve una tupla con los nombres de las tablas y operaciones
-- definidas
getNames :: Env -> ([Name], [Name])
getNames env = let tables = M.keys (M.filter (\n -> isLeft n) env)
                   operators = M.keys (M.filter (\n -> isRight n) env)
               in (tables, operators)

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar tablas y definiciones de operaciones desde un archivo"
  , Cmd [":print"] "<var>" Print "Imprimir la definición de una operación o una tabla"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":save"] "<var> <file>" Save "Guardar una tabla o una operación en un archivo"
  , Cmd [":quit"]       "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<rel>                   Evaluar la expresión e imprimir la tabla resultante\n"
    ++ "<var> = <rel>           Definir una relación\n"
    ++ "def <var> = <op>        Definir una operación\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let ct = intercalate
                   ", "
                   (map (++ if null a then "" else " " ++ a) c)
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { lfile = x }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S env lfile) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ err)
      return ""
    )
  stmts <- parseIO f' parseFile x
  maybe (return state) (foldM handleStmt state) stmts

compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" parseStmt x
  maybe (return state) (handleStmt state) x'

parseIO :: String -> Parser a -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case parse (totParser p) f x of
  Left  e -> print e >> return Nothing
  Right r -> return (Just r)

handleStmt :: State -> Stmt -> InputT IO State
handleStmt state stmt = lift $ do
  case stmt of
    DefRel n r      -> case evalR (env state) r of
                         Left s  -> putStrLn s >> return state
                         Right t -> return (state { env = M.insert n (Left t) (env state) })
    DefTable n t    -> if checkTable t
                       then return (state { env = M.insert n (Left t) (env state) })
                       else do putStrLn ("No se puede agregar la tabla "
                                        ++ n
                                        ++ " porque tiene nombres de atributos repetidos"
                                        )
                               return state
    DefOp n args op -> if checkArgs args
                       then case defineOp (env state) (op, args) of
                         Left s  -> putStrLn s >> return state
                         Right r -> return (state { env = M.insert n (Right (r, args)) (env state) })
                       else putStrLn ("El operador tiene parámetros repetidos") >> return state
    Eval r          -> do case evalR (env state) r of
                            Left s  -> putStrLn s
                            Right t -> putStrLn (printTable t)
                          return state
 where
  -- Verifica que la tabla no tenga nombres de atributos repetidos
  checkTable :: Table -> Bool
  checkTable (T as _ _) = length as == length (nub as)
  -- Verifica que la operación a definir no tenga parámetros repetidos
  checkArgs :: Args -> Bool
  checkArgs args = length args == length (nub args)

printVar :: Env -> String -> InputT IO ()
printVar env n =
  do lift $ case M.lookup n env of
       Nothing                 -> putStrLn ("La variable " ++ n ++ " no está definida")
       Just (Left t)           -> putStrLn (printTable t)
       Just (Right (op, args)) -> putStrLn (printOperator n args op)

saveToFile :: Env -> Name -> FilePath -> InputT IO ()
saveToFile _ _ "" = lift $ putStrLn "Se debe ingresar un nombre de archivo para guardar"
saveToFile env n f =
  do lift $ case M.lookup n env of
       Nothing -> putStrLn ("La variable " ++ n ++ " no está definida")
       Just r  -> do putStrLn ("Guardando " ++ n ++ " en " ++ f ++ "...")
                     let errString = "No se pudo escribir el archivo "
                     case r of
                       Left t@(T _ _ rs) -> if S.null rs
                                            then putStrLn "No se puede guardar una tabla vacía"
                                            else do Control.Exception.catch
                                                      (appendFile f (printTableFile n t))
                                                      (\e -> do
                                                        let err = show (e :: IOException)
                                                        hPutStr stderr
                                                          (errString ++ err)
                                                      )
                                                    return ()
                       Right (op, args) -> do Control.Exception.catch
                                                (appendFile f (printOperatorFile n args op))
                                                (\e -> do
                                                  let err = show (e :: IOException)
                                                  hPutStr stderr
                                                    (errString ++ err)
                                                )
                                              return ()
