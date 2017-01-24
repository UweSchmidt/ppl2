{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.GenCode where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types
import PPL2.CodeGen.Builder
import PPL2.CodeGen.Monad

-- ----------------------------------------

runGenCode :: (MonadError String m, CoreValue v) =>
              Mnemonics -> Expr v -> m (ACode, [v])
runGenCode mns e =
  either (const $ throwError "error in codegeneration") return $
  genACode mns e

-- ----------------------------------------

genACode :: CoreValue v => Mnemonics -> Expr v -> Either (GCError v) (ACode, [v])
genACode mns e =
  fst $ runGC genProg
  where
    genProg = do
      c <- genCodeExpr mns e
      f <- use fctCode
      g <- use globSeg
      return (toACode (c <> gTerminate <> f)
             , builder2List g
             )

-- ----------------------------------------

genCodeExpr :: CoreValue v => Mnemonics -> Expr v -> GenCode v Code
genCodeExpr mns = go
  where
    go e
      -- load a variable
      | Just a <- e ^? address =
          return $ gLoad a

      -- load an Int literal
      -- use LoadInt instr
      | Just v <- e ^? lit . _Int =
          return $ gLoadInt v

      -- load a literal
      -- init a new cell in the global data segment
      -- and gen code for loading this cell
      | Just v <- e ^? lit =
          newGlobVal v >>= return . gLoad

      -- load indirect, dereference
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gLoadInd

      -- gen code for a compute instr
      | Just (opr, es) <- e ^? hasOp (`elem` mns) . expr = do
          cs <- traverse go es
          return $ gComp opr <> mconcat cs

      | Just (fct, params) <- e ^? hasOp (== "call") . expr2 . _2 = do
          cparams <- go params
          ccall   <- call fct
          return $ cparams <> ccall

      -- code for single and multiple assignments
      --
      -- ":=" consumes then values on the eval stack
      -- so it's not suited to assignments in C in a context like "(x = ...) + ..."
      --
      -- "copy" is like ":=" but without
      -- consuming the values on the evaluation stack

      | Just (o, (lhs, rhs)) <- e ^? hasOp (`elem` [":=", "copy"]) . expr2 = do
          let n    = noOfArgs lhs   -- the # of values to be moved
          let cdup = if o == ":="
                     then mempty
                     else mconcat $ replicate n (gDup $ fromIntegral (n - 1))
          crhs <- go    rhs
          clhs <- store lhs
          return $ crhs <> cdup <> clhs

      -- code for tuple construction, like e1, e2, e3 in Lua or Ruby
      -- or actual param lists in C
      -- or for sequences of expressions
      -- e.g. C statement sequences s1; s2; s3;
      -- or ","-expressions, like e1, e2, e3
      | Just es <- e ^? hasOp (`elem` [",", ";"]) . expr . _2 =
          mconcat <$> traverse go es

      -- code for if-then-else
      | Just (cond, then', else') <- e ^? hasOp (== "if") . expr3 . _2 = do
          l1 <- newLabel
          l2 <- newLabel
          ccond <- branch l1 False cond
          cthen <- go then'
          celse <- go else'

          return $ mconcat
            [ ccond
            , gBrFalse l1
            , cthen
            , gJump    l2
            , gLabel   l1
            , celse
            , gLabel   l2
            ]

      -- code for "while expr do stmt"
      | Just (cond, body) <- e ^? hasOp (== "while") . expr2 . _2 = do
          l1 <- newLabel
          l2 <- newLabel
          ccond <- branch l2 True cond
          cbody <- go body

          return $ mconcat
            [ gJump   l1
            , gLabel  l2
            , cbody
            , gLabel  l1
            , ccond
            , gBrTrue l2
            ]

      -- code for "do stmt while expr"
      | Just (body, cond) <- e ^? hasOp (== "do") . expr2 . _2 = do
          l1 <- newLabel
          cbody <- go body
          ccond <- branch l1 True cond
          return $ mconcat
            [ gLabel l1
            , cbody
            , ccond
            ]

      -- a logical expression to be converted into 0 or 1
      | Just e' <- e ^? hasOp (`elem` ["&&", "||", "!"]) =
          go $
          expr3 # ("if", ( e'
                         , lit . _Bool # True
                         , lit . _Bool # False
                         )
                  )

      -- labels and gotos
      | Just l <- e ^? hasOp (== "label") . expr1 . _2 . label =
          return $ gLabel ("_" <> l)

      | Just l <- e ^? hasOp (== "goto") . expr1 . _2 . label =
          return $ gJump ("_" <> l)

      -- function pointer
      | Just lab <- e ^? label =
          return $ gLoadLab lab

      -- anonymous function as value
      -- generate name and gencode for a function declaration
      | Just e' <- e ^? hasOp (== "lambda") = do
          name <- newLabel
          go $
            expr2 # ("fctdef", (label # name, e'))

      -- function definition
      | Just (name, fct) <- e ^? hasOp (== "fctdef") . expr2 . _2
      , Just lab <- name ^? label =
          fctdef lab fct >> go name

      -- global variable allocation
      | Just nGlobals <- e ^? hasOp (== "globals") . expr1 . _2 . lit . _Word =
          newGlobals nGlobals (_Default # ()) >> return mempty

      | otherwise =
          abortGC $ errGCExpr e


    -- code for storing values
    store e
      -- store under given address
      | Just a <- e ^? address =
          return $ gStore a

      -- an indirect store with computed address
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gStoreInd

      -- discard a value, like in C expr statements, <expr>;
      -- remove the top of eval stack
      | Just _ <- e ^? hasOp (== "void") . expr0 =
          return $ gPop

      -- code for tuple deconstruction
      -- a sequence of stores as in  x,y = ...
      -- the list must be reversed, last value is on top of stack
      | Just as <- e ^? hasOp (== ",") . expr . _2 =
          mconcat <$> traverse store (reverse as)

      | otherwise =
          abortGC $ errGCExpr e

    -- compute the # of values to be stored
    noOfArgs e
      -- tuple
      | Just as <- e ^? hasOp (== ",") . expr . _2 =
          sum $ map noOfArgs as

      -- a single value has to be moved
      | otherwise =
          1

    -- --------------------

    branch lab b e
      -- negation
      | Just e1 <- e ^? hasOp (== "!") . expr1 . _2 =
          branch lab (not b) e1

      -- conjunction
      | Just (e1, e2) <- e ^? hasOp (== "&&") . expr2 . _2 =
          case b of
            True -> do
              lab1 <- newLabel
              ce1 <- branch lab1 (not b) e1
              ce2 <- branch lab       b  e2
              return $ ce1 <> ce2 <> gLabel lab1
            _ -> do
              ce1 <- branch lab b e1
              ce2 <- branch lab b e2
              return $ ce1 <> ce2

      -- disjunction, apply de Morgan
      | Just (e1, e2) <- e ^? hasOp (== "||") . expr2 . _2 =
          branch lab (not b) $
          expr2 # ("&&", ( expr1 # ("!", e1)
                         , expr1 # ("!", e2)
                         )
                  )

      -- negation with logical ops
      | Just e1 <- e ^? hasOp (== "!") . expr1 . _2 =
           branch lab (not b) e1

      -- constant evaluation, maybe a rare case
      | Just v <- e ^? lit
      , Just cond <- v ^? _Bool =
          return $
          if cond == b
          then gJump lab
          else mempty

      | otherwise =
          abortGC $ errGCExpr e

    -- --------------------

    -- function calls
    call fct
      -- normal function call
      | Just lab <- fct ^? label =
          return $ gSRJump lab

      -- lambda application
      | Just e <- fct ^? hasOp (== "lambda") = do
          name <- newLabel
          fctdef name e
          call $ label # name

      | otherwise =
          (<> gSRJumpInd) <$> go fct


    -- code for a function definition
    -- the code is stored in the monad state

    fctdef name fct
      | Just (nParams, nLocals, body) <- fct ^? hasOp (== "lambda") . expr3 . _2
      , Just np <- nParams ^? lit . _Word
      , Just nl <- nLocals ^? lit . _Word = do
          let ra =  AbsA 0
          cbody  <- go body

          addFctCode $ mconcat
            [ gLabel name
            , gEnter (1 + np + nl)
            , gStore ra
            , gLabel ("start" <> name)
            , mconcat $ map (gStore . AbsA) $ reverse [1..np]

            , cbody

            , gLabel ("end" <> name)
            , gLoad  ra
            , gLeave
            ]

      | otherwise =
          abortGC $ errGCExpr fct

-- ----------------------------------------
