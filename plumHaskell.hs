import System.IO
import System.Random

data Command a =  Copy Ident Ident
               | TAssign Ident (TExpr a)
               | CAssign Ident (CExpr a)
               | Split Ident Ident Ident
               | Input Ident
               | Print (NExpr a)
               | Draw (TExpr a)
               | Seq [Command a]
               | Cond (BExpr a) (Command a) (Command a)
               | Loop (BExpr a) (Command a)
               | DeclareVector Ident (NExpr a)
               | Push Ident Ident
               | Pop Ident Ident
               deriving Read

data BExpr a = And (BExpr a) (BExpr a)
             | Or (BExpr a) (BExpr a)
             | Not (BExpr a)
             | Gt (NExpr a) (NExpr a)
             | Lt (NExpr a) (NExpr a)
             | Eq (NExpr a) (NExpr a)
             | Empty Ident
             | Full Ident
             deriving Read

data NExpr a = Var Ident
             | Const a
             | Plus (NExpr a) (NExpr a)
             | Minus (NExpr a) (NExpr a)
             | Times (NExpr a) (NExpr a)
             | Length Ident
             | Diameter Ident
             deriving Read

data TExpr a = TVar Ident
             | Merge (TExpr a) (CExpr a) (TExpr a)
             | Tube (NExpr a) (NExpr a)
             deriving Read

data CExpr a = CVar Ident
             | Connector (NExpr a)
             deriving Read

type Ident = String

instance Show a => Show (Command a) where
    show x = showIndent "" x    
    
showIndent :: Show a => String -> (Command a) -> String
showIndent indent (Copy id1 id2) = indent++id1++" = "++id2
showIndent indent (TAssign id t) = indent++id++" = "++(show t) -- la implementacio del Show TExpr ja s'encarregara de crear be la string
showIndent indent (CAssign id conn) = indent++id++" = "++(show conn)
showIndent indent (Split left right t) = indent++"("++left++","++right++") = SPLIT "++t -- (T3,T4) = SPLIT T2
showIndent indent (Input id) = indent++"INPUT "++id
showIndent indent (Print expr) = indent++"PRINT "++(show expr)
showIndent indent (Draw t) = indent++"DRAW "++(show t)
showIndent indent (Seq []) = ""
showIndent indent (Seq (x:xs)) = indent++(show x)++"\n"++(showIndent indent (Seq xs))
showIndent indent (Cond bexpr seqIf seqElse) = indent++"IF ("++(show bexpr)++")\n"++(showIndent (indent++"  ") seqIf)++"ELSE\n"++(showIndent (indent++"  ") seqElse)++"ENDIF" -- comprovar mes d'un nivell
showIndent indent (Loop bexpr seqLoop) = indent++"WHILE ("++(show bexpr)++")\n"++(showIndent (indent++"  ") seqLoop)++"ENDWHILE" -- tambe aqui
showIndent indent (DeclareVector id nexpr) = indent++id++" = TUBEVECTOR OF "++(show nexpr)
showIndent indent (Push vec elem) = indent++"PUSH "++vec++" "++elem
showIndent indent (Pop vec elem) = indent++"POP "++vec++" "++elem


instance Show a => Show (BExpr a) where
    show (And bexpr1 (Or x y)) = (show bexpr1)++" AND ("++(show (Or x y))++")"
    show (And (Or x y) bexpr2) = "("++(show (Or x y))++") AND "++(show bexpr2) -- Not in the example, test needed
    show (And bexpr1 bexpr2) = (show bexpr1)++" AND "++(show bexpr2)
    show (Or bexpr1 bexpr2) = (show bexpr1)++" OR "++(show bexpr2)    
    show (Not (Or x y)) = "NOT ("++(show (Or x y))++")" -- Not with parenthesis not in the example, test needed (Or, And)
    show (Not (And x y)) = "NOT ("++(show (And x y))++")"
    show (Not bexpr) = "NOT "++(show bexpr)    
    show (Gt nexpr1 nexpr2) = (show nexpr1)++" > "++(show nexpr2)
    show (Lt nexpr1 nexpr2) = (show nexpr1)++" < "++(show nexpr2)
    show (Eq nexpr1 nexpr2) = (show nexpr1)++" == "++(show nexpr2)
    show (Empty id) = "EMPTY("++id++")"
    show (Full id) = "FULL("++id++")"
   
instance Show a => Show (NExpr a) where
    show (Var id) = id -- Not in the example!
    show (Const num) = show num
    show (Plus nexpr1 nexpr2) = (show nexpr1)++" + "++(show nexpr2) -- Plus Minus and Times not in the example, test needed
    show (Minus nexpr1 nexpr2) = (show nexpr1)++" - "++(show nexpr2)
    show (Times nexpr1 nexpr2) = (show nexpr1)++" * "++(show nexpr2)
    show (Length id) = "LENGTH("++id++")"
    show (Diameter id) = "DIAMETER("++id++")"
    
instance Show a => Show (TExpr a) where
    show (TVar id) = id
    show (Merge t1 conn t2) = "MERGE "++(show t1)++" "++(show conn)++" "++(show t2)
    show (Tube nexpr1 nexpr2) = "TUBE "++(show nexpr1)++" "++(show nexpr2)
    
instance Show a => Show (CExpr a) where
    show (CVar id) = id
    show (Connector nexpr) = "CONNECTOR "++(show nexpr)

------------ 3. Language Interpreter ----------------
    
class SymTable m where
    update :: m a -> String -> Val a -> m a -- memory, variable, value
    value :: m a -> String -> Maybe (Val a) -- memory, variable
    start :: m a -- Al main, (start :: PairList a) i lo mateix per arbres, on "a" pot ser Int
    getList :: m a -> [(String, Val a)]
    
data Val a = VNum a
           | VTube [a] a -- longitudes and diameter
           | VConn a
           | VTubevector a [ (a, [a]) ] -- max length and list of tubes
           | VNovalue
           deriving (Show, Eq)
           
data PairList a = PairListC [(String, Val a)]   -- PairList constructor
                  deriving Show

data BinTree a = Node (String, Val a) (BinTree a) (BinTree a)
                | EmptyT
                deriving Show
                  
instance SymTable PairList where -- PairList a ? NO, it has to match m
    update (PairListC l) s v
        | null lb   = PairListC (listA++[(s,v)])
        | otherwise = PairListC (listA++((s,v):listB))        
        where (listA, lb) = span (\(xString, _) -> xString /= s) l
              (_:listB) = lb -- lazy evaluation, no error!        
    value (PairListC l) s = lookup s l
    start = PairListC []
    getList (PairListC l) = l

instance SymTable BinTree where
    update EmptyT s v = Node (s, v) EmptyT EmptyT
    update (Node p@(xString, _) t1 t2) s v 
        | s < xString = Node p (update t1 s v) t2
        | s > xString = Node p t1 (update t2 s v)
        | otherwise   = Node (s,v) t1 t2        
    value EmptyT s = Nothing
    value (Node (xString, xVal) t1 t2) s
        | s < xString = value t1 s
        | s > xString = value t2 s
        | otherwise   = Just xVal
    start = EmptyT
    getList EmptyT = []
    getList (Node x left right) = (getList left)++[x]++(getList right) -- inorder: left, root, right    
    
interpretCommand :: (Num a, Ord a, SymTable m)
  => m a -> [a] -> Command a -> ((Either String [a]), m a, [a]) -- output list 1: prints and draws
interpretCommand mem input (Copy id1 id2)                       -- output list 2: input after executing the command
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue -> (Left "variable content no longer exists", mem, input)
                val      -> (Right [], update mem id1 val, input)            
    | otherwise  = (Left "undefined variable", mem, input)
    where mayb = value mem id2
interpretCommand mem input (TAssign id t)
    | Right tube <- eith = (Right [], update memmerge id tube, input)
    | Left str   <- eith = (Left str, mem, input)
    where (eith, memmerge) = getTube mem t
interpretCommand mem input (CAssign id conn)
    | Right conect <- eith = (Right [], update mem id conect, input)
    | Left str     <- eith = (Left str, mem, input)
    where eith = getConn mem conn
interpretCommand mem input (Split left right t) -- all are IDs
    | isRight eith = (Right [], update memright t VNovalue, input) -- tube deleted
    | otherwise = (Left str, mem, input)
    where (eith, _) = getTube mem (TVar t)
          Right (VTube l d) = eith
          Left str = eith
          
          totalLen = sum l
          partr = part2 totalLen
          partl = totalLen - partr          
          listr = calclistr totalLen l
          listl = let auxl = take ((length l) - (length listr)) l
                      auxsum = sum auxl in
                      if auxsum == partl then auxl
                      else auxl++[partl-auxsum]
                      
          memleft  = update mem left (VTube listl d)
          memright = update memleft right (VTube listr d)
          
          calclistr acc listacc -- accumulated sum and list of the right part
              | acc < partr = (partr-acc:listacc)
              | acc > partr = calclistr (acc-x) xs
              | otherwise = listacc
              where (x:xs) = listacc

interpretCommand mem (i:is) (Input id) = (Right [], update mem id (VNum i), is)
interpretCommand mem input (Print nexpr)
    | Right n  <- eith = (Right [n], mem, input)
    | Left str <- eith = (Left str, mem, input)
    where eith = evalNum mem nexpr
interpretCommand mem input (Draw t)
    | Right (VTube l d) <- eith = (Right (d:l), memmerge, input) -- draw modifies the memory if it has a merge expression
    | Left str <- eith = (Left str, mem, input)
    where (eith, memmerge) = getTube mem t
interpretCommand mem input (Seq []) = (Right [], mem, input)
interpretCommand mem input (Seq (x:xs))
    | Right l  <- eithx, Right ls <- eithxs = (Right (l++ls), memxs, inputxs)
    | Left str <- eithx  = (Left str, mem, input) -- if there is an error, the command does not modify the memory
    | Left str <- eithxs = (Left str, memxs, inputxs)
    where (eithx, memx, inputx) = interpretCommand mem input x
          (eithxs, memxs, inputxs) = interpretCommand memx inputx (Seq xs)
interpretCommand mem input (Cond bexpr seqIf seqElse)
    | Right b  <- eith = if b then interpretCommand mem input seqIf
                         else interpretCommand mem input seqElse
    | Left str <- eith = (Left str, mem, input)
    where eith = evalBool mem bexpr
interpretCommand mem input (Loop bexpr seqLoop)
    | Right b  <- eith = if b then case eithFirst of -- we loop once
                                   Right l  -> case eithLoop of -- we do the rest of the loop
                                               Right ls -> (Right (l++ls), memLoop, inputLoop)
                                               Left str -> (Left str, memLoop, inputLoop)
                                   Left str -> (Left str, memFirst, inputFirst)
                         else (Right [], mem, input)
    | Left str <- eith = (Left str, mem, input)
    where eith = evalBool mem bexpr
          (eithFirst, memFirst, inputFirst) = interpretCommand mem input seqLoop
          (eithLoop, memLoop, inputLoop)    = interpretCommand memFirst inputFirst (Loop bexpr seqLoop)
interpretCommand mem input (DeclareVector id nexpr)
    | Right n  <- eith = (Right [], update mem id (VTubevector n []), input)
    | Left str <- eith = (Left str, mem, input)
    where eith = evalNum mem nexpr
interpretCommand mem input (Push vec elem)
    | isJust mayb, Right (VTube ltube d) <- eith  = 
            let val = fromJust mayb in
            case val of
                VNovalue          -> (Left "variable content no longer exists", mem, input)
                VTubevector len l -> let memVec   = update mem vec (VTubevector len (l++[(d,ltube)]) ) 
                                         memFinal = update memVec elem VNovalue in -- tube deleted
                                     if fromIntegral (length l) < len then (Right [], memFinal, input)
                                     else (Left "full vector", mem, input)
                val               -> (Left "type error", mem, input)
    | Left str <- eith = (Left str, mem, input)
    | otherwise  = (Left "undefined variable", mem, input)
    where (eith, _) = getTube mem (TVar elem)
          mayb = value mem vec
interpretCommand mem input (Pop vec elem)
    | isJust mayb =
            let val = fromJust mayb in
            case val of
                VNovalue          -> (Left "variable content no longer exists", mem, input)
                VTubevector len l -> let (d, ltube) = last l
                                         memTube  = update mem elem (VTube ltube d)
                                         memFinal = update memTube vec (VTubevector len (init l)) in
                                     if null l then (Left "empty vector", mem, input)
                                     else (Right [], memFinal, input)
                val               -> (Left "type error", mem, input)
    | otherwise  = (Left "undefined variable", mem, input)
    where mayb = value mem vec

interpretProgram:: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input c = eith
                           where (eith, _, _) = interpretCommand (start::PairList a) input c

evalBool :: (Num a, Ord a, SymTable m)  => m a -> BExpr a -> (Either String Bool) -- Left: errorMessage Right: Result
evalBool mem (And bexpr1 bexpr2)
    | Right b1 <- eith1, Right b2 <- eith2 = Right (b1 && b2)
    | Left str <- eith1 = Left str --------------------------------------- str and str are the SAME?
    | Left str <- eith2 = Left str
    where eith1 = evalBool mem bexpr1
          eith2 = evalBool mem bexpr2
evalBool mem (Or bexpr1 bexpr2)
    | Right b1 <- eith1, Right b2 <- eith2 = Right (b1 || b2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalBool mem bexpr1
          eith2 = evalBool mem bexpr2
evalBool mem (Not bexpr)
    | Right b  <- eith = Right (not b)
    | Left str <- eith = Left str
    where eith = evalBool mem bexpr
evalBool mem (Gt nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 > n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalBool mem (Lt nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 < n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalBool mem (Eq nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 == n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalBool mem (Empty id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue        -> (Left "variable content no longer exists")
                VTubevector _ l -> (Right (null l))
                val             -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id
evalBool mem (Full id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue          -> (Left "variable content no longer exists")
                VTubevector len l -> (Right ( fromIntegral (length l) == len))
                val               -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id

evalNum :: (Num a, Ord a, SymTable m)  => m a -> NExpr a -> (Either String a)
evalNum mem (Var id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue -> (Left "variable content no longer exists")
                VNum num -> (Right num)
                val      -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id    
evalNum mem (Const num) = (Right num) -- num es "a": Int, Float...
evalNum mem (Plus nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 + n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalNum mem (Minus nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 - n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalNum mem (Times nexpr1 nexpr2)
    | Right n1 <- eith1, Right n2 <- eith2 = Right (n1 * n2)
    | Left str <- eith1 = Left str
    | Left str <- eith2 = Left str
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2
evalNum mem (Length id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue  -> (Left "variable content no longer exists")
                VTube l _ -> (Right (sum l))
                val       -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id          
evalNum mem (Diameter id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue  -> (Left "variable content no longer exists")
                VTube _ d -> (Right d)
                VConn d   -> (Right d)
                val              -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id
    
    
getTube :: (Num a, Ord a, SymTable m)  => m a -> TExpr a -> (Either String (Val a), m a)
-- returns Eihter with String or (VTube [a] a) and the memory updated (for the merge)
getTube mem (TVar id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue  -> (Left "variable content no longer exists", mem)
                VTube l d -> (Right (VTube l d), mem)
                val       -> (Left "type error", mem)
    | otherwise  = (Left "undefined variable", mem)
    where mayb = value mem id    
getTube mem (Merge t1 conn t2)
    | isRight eith1 && isRight eithc && isRight eith2 = 
        if d1 == dc && d1 == d2
        then (Right (VTube (l1++l2) d1), memfinal)
        else (Left "unmatched diameter", mem)
    | Left str <- eith1 = (Left str, mem)
    | Left str <- eithc = (Left str, mem)
    | Left str <- eith2 = (Left str, mem)
    where (eith1, memmerge1) = getTube mem t1
          (eith2, memmerge2) = getTube memmerge1 t2
          eithc = getConn mem conn
          Right (VTube l1 d1) = eith1
          Right (VTube l2 d2) = eith2
          Right (VConn dc)    = eithc          
          memfinal = deletevars memmerge2 t1 conn t2
          
          deletevars memdel (TVar id) condel tube2del = 
              deletevars (update memdel id VNovalue) (Tube (Const 0) (Const 0)) condel tube2del
          deletevars memdel tdummy1 (CVar id) tube2del = 
              deletevars (update memdel id VNovalue) tdummy1 (Connector (Const 0)) tube2del
          deletevars memdel tdummy1 condummy (TVar id) = 
              deletevars (update memdel id VNovalue) tdummy1 condummy (Tube (Const 0) (Const 0))
          deletevars memdel _ _ _ = memdel

getTube mem (Tube nexpr1 nexpr2)
    | Right l <- eith1, Right d <- eith2 = (Right (VTube [l] d), mem)
    | Left str <- eith1 = (Left str, mem)
    | Left str <- eith2 = (Left str, mem)
    where eith1 = evalNum mem nexpr1
          eith2 = evalNum mem nexpr2

getConn :: (Num a, Ord a, SymTable m)  => m a -> CExpr a -> (Either String (Val a)) -- returns VConn a
getConn mem (CVar id)
    | isJust mayb = 
            let val = fromJust mayb in
            case val of
                VNovalue -> (Left "variable content no longer exists")
                VConn d  -> (Right (VConn d))
                val      -> (Left "type error")
    | otherwise  = Left "undefined variable"
    where mayb = value mem id
getConn mem (Connector nexpr)
    | Right d  <- eith = Right (VConn d)
    | Left str <- eith = Left str
    where eith = evalNum mem nexpr

---------------------------------
---- Auxiliary functions --------

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

fromJust :: Maybe a -> a
fromJust (Just val) = val

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

part2 :: (Num a, Ord a) => a -> a -- returns the greatest "integer" such that its double is <= x
part2 x = partrec 0
          where partrec numb
                    | numb*2   > x = numb-1
                    | otherwise = partrec (numb+1)

------------ 4. Testing ----------------

main = do
       putStrLn "Does the program work with reals? 0 - Ints, 1 - Reals"
       realS <- getLine
       let real = read realS::Int
       ha1 <- openFile "./programhs1.txt" ReadMode
       s1 <- hGetLine ha1
       ha2 <- openFile "./programhs2.txt" ReadMode
       s2 <- hGetLine ha2
       putStrLn "Do you want to use a Binary Tree for the memory? 0 - List, 1 - Binary Tree"
       treeS <- getLine
       let tree = read treeS::Int
       putStrLn "Choose comparative test type: 0 - Manual, 1 - Automatic"
       autoS <- getLine
       let auto = read autoS::Int
       if auto == 0 then do -- Manual
           putStrLn "Introduce the input list:"
           inputS <- getLine
           if real == 0 then do -- Ints
                let inputI = (read inputS::[Int])++(randomRs (10, 100) (mkStdGen 0)::[Int])
                putStrLn "Results:"
                if tree == 0 then do -- PairList
                    let (out1IP, mem1IP, _) = interpretCommand (start::PairList Int) inputI (read s1::Command Int)
                    let (out2IP, mem2IP, _) = interpretCommand (start::PairList Int) inputI (read s2::Command Int)
                    putStrLn $ show out1IP
                    putStrLn $ show out2IP
                    if out1IP == out2IP then do putStrLn "Equal results"
                    else do putStrLn "The results are not equal"
                    putStrLn "Different variables:"                                       
                    putStrLn $ show $ comparevars mem1IP mem2IP                    
                else do -- BinTree
                    let (out1IT, mem1IT, _) = interpretCommand (start::BinTree Int) inputI (read s1::Command Int)
                    let (out2IT, mem2IT, _) = interpretCommand (start::BinTree Int) inputI (read s2::Command Int)
                    putStrLn $ show out1IT
                    putStrLn $ show out2IT
                    if out1IT == out2IT then do putStrLn "Equal results"
                    else do putStrLn "The results are not equal"
                    putStrLn "Different variables:"                                       
                    putStrLn $ show $ comparevars mem1IT mem2IT                                
           else do -- Doubles
                let inputD = (read inputS::[Double])++(randomRs (10, 100) (mkStdGen 0)::[Double])
                putStrLn "Results:"
                if tree == 0 then do -- PairList
                    let (out1DP, mem1DP, _) = interpretCommand (start::PairList Double) inputD (read s1::Command Double)
                    let (out2DP, mem2DP, _) = interpretCommand (start::PairList Double) inputD (read s2::Command Double)
                    putStrLn $ show out1DP
                    putStrLn $ show out2DP
                    if out1DP == out2DP then do putStrLn "Equal results"
                    else do putStrLn "The results are not equal"
                    putStrLn "Different variables:"                                       
                    putStrLn $ show $ comparevars mem1DP mem2DP                   
                else do -- BinTree
                    let (out1DT, mem1DT, _) = interpretCommand (start::BinTree Double) inputD (read s1::Command Double)
                    let (out2DT, mem2DT, _) = interpretCommand (start::BinTree Double) inputD (read s2::Command Double)
                    putStrLn $ show out1DT
                    putStrLn $ show out2DT
                    if out1DT == out2DT then do putStrLn "Equal results"
                    else do putStrLn "The results are not equal"
                    putStrLn "Different variables:"                                       
                    putStrLn $ show $ comparevars mem1DT mem2DT   
       else do -- Auto
           putStrLn "Introduce the number of tests:"
           kS <- getLine
           let k = read kS::Int
           putStrLn $ show k
           --let (difer = automatictest k real tree (s1, s2)
           --putStrLn $ "Number of tests with different output: "++show difer

       hClose ha1
       hClose ha2
      
showAndCompRes :: String -> String -> IO ()
showAndCompRes out1 out2 = do
    putStrLn $ show out1
    putStrLn $ show out2
    if out1 == out2 then do putStrLn "Equal results" -- TODO: do needed?
    else do putStrLn "The results are not equal"
{-     
automatictest :: Int -> Int -> Int -> (String, String) -> IO Int -- k, real, tree, programs, number of diferentResults
automatictest 0 _ _ _ = 0
automatictest k real tree (s1, s2)
    | real == 0 && tree == 0 = automatictest (k-1)
    | real == 0 && tree == 1 =
    | real == 1 && tree == 0 =
    | otherwise = 
    where inputI = (randomRs (10, 100) (mkStdGen k)::[Int])
          inputD = (randomRs (10, 100) (mkStdGen k)::[Double])
          -- Ints
          (out1IP, _, _) = interpretCommand (start::PairList Int) inputI (read s1::Command Int)
          (out2IP, _, _) = interpretCommand (start::PairList Int) inputI (read s2::Command Int)
          (out1IT, _, _) = interpretCommand (start::BinTree Int) inputI (read s1::Command Int)
          (out2IT, _, _) = interpretCommand (start::BinTree Int) inputI (read s2::Command Int)
          -- Doubles
          (out1DP, _, _) = interpretCommand (start::PairList Double) inputD (read s1::Command Double)
          (out2DP, _, _) = interpretCommand (start::PairList Double) inputD (read s2::Command Double)
          (out1DT, _, _) = interpretCommand (start::BinTree Double) inputD (read s1::Command Double)
          (out2DT, _, _) = interpretCommand (start::BinTree Double) inputD (read s2::Command Double) -}
          

comparevars :: (Num a, Ord a, SymTable m)
  => m a -> m a -> [(String, Maybe(Val a), Maybe(Val a))] -- Nothing means not in memory
comparevars mem1 mem2 = diff (getList mem1) (getList mem2)

diff :: (Num a, Ord a)
  => [(String, Val a)] -> [(String, Val a)] -> [(String, Maybe(Val a), Maybe(Val a))]
diff l1 l2
    | (null l1) && (null l2) = []
    | null l2 = ((str1,Just v1, Nothing) :diff xs1 []) -- only l1
    | null l1 = ((str2,Nothing, Just v2) :diff [] xs2) -- only l2
    | otherwise = if isJust mayb then
                      let val = fromJust mayb in
                      if val /= v1 then ((str1, Just v1, Just val): diff upd1 upd2)
                      else diff upd1 upd2
                  else ((str1, Just v1, Nothing) : diff upd1 l2) -- TODO: l2 or upd2?
    
    where ( (str1, v1) :xs1) = l1
          ( (str2, v2) :xs2) = l2
          mayb = lookup str1 l2
          upd1 = filter (\(lambdas, lambdav) -> lambdas /= str1) l1
          upd2 = filter (\(lambdas, lambdav) -> lambdas /= str1) l2 -- we remove str1 from both lists


-- DEBUG Trees
main1 = do
       let mem = start::BinTree Int
       print (value mem "First")
       let mem2 = update mem "First" (VNum 1)
       let mem3 = update mem2 "Second" (VNum 2)
       print (value mem3 "Second")
       let mem4 = update mem3 "First" (VConn 11)
       putStr "mem4: "; print mem4
       let mem5 = update mem4 "Second" VNovalue
       putStr "mem5: "; print mem5
       let mem6 = update mem5 "Third" (VNum 3)
       let mem7 = update mem6 "Third" (VTube [1] 33)
       putStr "mem7: "; print mem7
       print (value mem7 "Third")
       let mem8 = update mem7 "Fourth" (VNum 4)
       putStr "mem8: "; print mem8
       let mem9 = update mem8 "First" (VNum (-1))
       let mem10 = update mem9 "Fourth" (VNum (-4))
       putStr "mem10: "; print mem10
       let mem11 = update mem10 "Second" (VNum (-2))
       putStr "mem11: "; print mem11
       putStrLn $ show $ getList mem11
       
ex1 :: Command Int
ex1 = (Seq [(Input "X"),(Input "Y"),(TAssign "T1" (Tube (Var "X") (Var "Y"))),(TAssign "T2" (Tube (Const 10) (Var "Y"))),(Split "T3" "T4" "T2"),(Input "Z"),(TAssign "T6" (Tube (Var "Z") (Const 2)))])
