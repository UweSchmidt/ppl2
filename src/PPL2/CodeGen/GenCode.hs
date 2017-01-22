{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.GenCode where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types
import PPL2.CodeGen.Builder
import PPL2.CodeGen.Monad

-- ----------------------------------------

genCode :: (CoreValue v) => Mnemonics -> Expr v -> GenCode v Code
genCode mns = go
  where
    go e
      -- load a variable
      | Just a <- e ^? address =
          return $ gLoad a

      -- load an Int literal
      | Just v <- e ^? lit . _Int =
          return $ gLoadInt v

      -- load a literal
      | Just v <- e ^? lit =
          return $ undefined -- gLoadInt v

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

      -- conversion to void, discard the result
      | Just e1 <- e ^? hasOp (== "(void)") . expr1 . _2 =
          go $
          expr2 # ("=", ( expr0 # "pop"
                         , e1
                         )
                  )

      -- code for single and multiple assignments
      | Just (lhs, rhs) <- e ^? hasOp (== "=") . expr2 . _2 = do
          crhs <- go rhs
          clhs <- store lhs
          return $ crhs <> clhs

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

      -- code for tuple deconstruction
      -- a sequence of stores as in  x,y = y,x
      | Just as <- e ^? hasOp (== ",") . expr . _2 =
          mconcat <$> traverse store (reverse as)

      -- an indirect store with computed address
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gStoreInd

      -- discard a value, like in C expr statements, <expr>;
      -- move the result of an expression into a black hole
      -- (convert to void)
      | Just _ <- e ^? hasOp (== "pop") . expr0 =
          return $ gPop

      | otherwise =
          abortGC $ errGCExpr e

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
