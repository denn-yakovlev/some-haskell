{-# LANGUAGE TupleSections #-}
module AsmInterpreter (interpret) where
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import Control.Monad.Reader
import Control.Monad.State
import Text.Read (readMaybe)

type Registers = M.Map String Int

getValue :: String -> Registers -> Int
getValue src regs =
  case readMaybe src :: Maybe Int of
      Just x -> x
      Nothing -> fromMaybe 0 (M.lookup src regs)

mov :: String -> String -> State Registers ()
mov regId src = modify $ \regs -> M.insert regId (getValue src regs) regs

inc :: String -> State Registers ()
inc regId = modify $ \regs -> M.adjust (+1) regId regs

dec :: String -> State Registers ()
dec regId = modify $ \regs -> M.adjust (\x -> x-1) regId regs

jnz :: String -> Int -> Registers -> Int
jnz src shift regs = if getValue src regs == 0 then 0 else shift


execCmd :: Int -> ReaderT [String] (State Registers) ()
execCmd i = do
  cmds <- ask
  if i >= length cmds
    then
        lift $ return ()
    else do
        regs <- lift get
        let cmd = cmds !! i;
            cmdWords = words cmd;
            cmdType = head cmdWords;
            arg1 = cmdWords !! 1
            nextI = i +
                if cmdType == "jnz"
                then
                    let sh = read $ cmdWords !! 2 :: Int
                    in jnz arg1 sh regs
                else 1;
        lift $ case cmdType of
            "mov" -> let src = cmdWords !! 2 in mov arg1 src
            "inc" -> inc arg1
            "dec" -> dec arg1
            _ -> return ()
        execCmd nextI

interpret :: [String] -> Registers
interpret cmds = execState (runReaderT (execCmd 0) cmds) M.empty