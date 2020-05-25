-- stack --system-ghc runghc

{-# LANGUAGE TemplateHaskell #-}
{- |
References:
https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial#Reification
-}

module Template where

import           Control.Monad              (forM, replicateM)
import           Language.Haskell.TH        (Body (..), Clause (..), Con (..),
                                             Dec (..), Exp (..), Info (..),
                                             Name, Pat (..), Q, TyVarBndr (..),
                                             Type (..), appT, appsE, clause,
                                             conE, conP, conT, funD, instanceD,
                                             mkName, newName, normalB, reify,
                                             varE, varP, varT)
import           Language.Haskell.TH.Syntax (BangType, getQ, putQ)

-- | We want a function which can curry n-tuples on demand
curryN :: Int -> Q Exp
curryN n = do

-- | Assign a name to the function
    f <- newName "f" -- ^ Assign a name to the function

-- | Generate n variables, names generated are fresh (only within scope of curryN)
    xs <- replicateM n (newName "x")

-- | Convert the created names to arguments
    let args = map VarP (f:xs)

-- | Convert a list of variables (xs) to n-tuple (Our input)
        ntup = TupE (map VarE xs)

-- | Has form : \x1 -> .. -> xn -> f (x1, .., xn)
    return $ LamE

-- | First, the LamE pattern matches against f and n variables (f:xs)
      args

-- | It then applies f,
      (AppE (VarE f)

 -- | to the n-tuple, derived from the pattern-matched variables
            ntup)


-- | Generates a list of declarations (the curry functions)
genCurries :: Int -> Q [Dec]
genCurries n =
-- | Generate n Curry function declarations
-- curry1 = ..., curry2 = ..., curryN = ...
    forM [1..n] mkCurryDec

-- | Generates a single declaration for curry-n
genCurry :: Int -> Q [Dec]
genCurry n = return <$> mkCurryDec n

-- | Generates a single declaration for curry-n
mkCurryDec :: Int -> Q Dec
mkCurryDec ith = do
-- | Generate the curry declaration body
                curry' <- curryN ith

-- | Generate the curry declaration name
                let name = mkName $ "curry" ++ show ith -- Create name for the function

-- | Return a function declaration, with the specified name { <function name> = <curry definition> }
                return $ FunD name -- ^ { <function name> = ... }

-- | The body of the function,
                              [Clause []               -- ^ No patterns
                                      (NormalB curry') -- ^ The body { = <curry definition> }
                                      []]              -- ^ No further declarations needed

data Result e a = Err e
    | Ok a

data Deriving = Deriving
    { tyCon' :: Name
    , tyVar' :: Name
    }

-- | Given a type, we want to derive a functor instance
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = do

-- | Get the type constructor information by reifying the type
    (TyConI tyCon) <- reify ty

-- | We accept only newtypes and datatypes, not type synonyms
    (tyConName, tyVars, cs) <- case tyCon of

-- | Pattern matching on datatype case
      DataD  _ nm tyVars _ cs _ -> return (nm, tyVars, cs)

-- | Pattern matching on newtype case
      NewtypeD  _ nm tyVars _ c _ -> return (nm, tyVars, [c])

-- | Failing if it is a type synonym
      _ -> fail "deriveFunctor: tyCon may not be a type synonym."

-- | Deconstruct the last type variable
-- For example, for the following
-- data MyType a b c = ...
-- the last type variable would be c
-- assumes that there are type vars
    let (KindedTV tyVar StarT) = last tyVars

-- | Constructs the instance type
        instanceType =

-- | Functor (someType)
-- Apply the Functor Constraint type to some type variable
            appT

-- | Functor constraint
              (conT ''Functor)

-- | This does the following (((f a) b) c) .. to the type variables (except the last one)
              (foldl apply


                (conT tyConName) -- ^ Type constructor name
                (init tyVars))   -- ^ every type variable except the last
                where

-- | t a
                  apply t (PlainTV name)    = appT t (varT name)
-- | t (a :: k)
                  apply t (KindedTV name _) = appT t (varT name)

-- | Replacing the state in the Q Monad
-- Stores type constructor name
-- Stores last the type variable
-- These will be used later in the `newField` function
    putQ $ Deriving tyConName tyVar


-- | Sequence is used to ensure it type checks to Q [Dec]
-- sequence :: [Q Dec] -> Q [Dec]
    sequence [

-- | instance (Functor <typeVar>) where
--       fmap = ...
        instanceD        -- ^ Generates an instance declaration
          (return [])    -- ^ No Context (For example (Show w) => ...)
          instanceType   -- ^ Functor <typeVar>
          [genFmap cs]]  -- ^ fmap = ...
                         -- Note that cs refers to the data constructors of the type

-- | Generates an fMap function declaration after some data constructors are supplied
genFmap :: [Con] -> Q Dec
genFmap cs =

-- | <name> = <body>
    funD 'fmap                  -- ^ name = fmap
         (map genFmapClause cs) -- ^ body = ...
                                -- maps over each data constructor.
                                -- In a sum type, e.g. data A = B | C
                                -- we would be mapping over cs = [B, C]

-- | Takes in a data constructor, returns a clause
genFmapClause :: Con -> Q Clause
genFmapClause (NormalC name fieldTypes)

-- | Generate a new scoped name f, we fmap this f over the type
  = do f          <- newName "f"

-- | Generate scoped name x for all the fields of a data constructor
       fieldNames <- replicateM (length fieldTypes) (newName "x")

-- | Generate patterns

-- | f <data constructor name> <field1> <field2> ...
       let pats =
                   varP f                           -- ^ f
                : [conP name (map varP fieldNames)] -- ^ <data constructor name> <field1> <field2> ...

-- | = <data constructor name>
           body =
               normalB $ appsE     -- ^ [ExpQ] -> ExpQ

                       $ conE name -- ^ <data constructor name>

                       -- | []
                       : zipWith (newField f) fieldNames fieldTypes

       clause pats body []

genFmapClause _ = fail "genFmapClause: not NormalC"

-- |
newField :: Name -> Name -> BangType -> Q Exp

-- | since strictness doesn't affect our deriving of functor instance, we use "_" to ignore the strictness
newField f x (_, fieldType)

-- | Earlier we stored this
  = do Just (Deriving typeCon typeVar) <- getQ

       case fieldType of
-- a; matches the typeVar we want, apply the function
         VarT typeVar' | typeVar' == typeVar ->
           [| $(varE f) $(varE x) |]
-- T a b; matches the typeVar we want
         ty `AppT` VarT typeVar' |
           leftmost ty == ConT typeCon && typeVar' == typeVar ->
             [| fmap $(varE f) $(varE x) |]

-- Doesn't change the other fields
         _ -> [| $(varE x) |]

leftmost :: Type -> Type
leftmost (AppT ty1 _) = leftmost ty1
leftmost ty           = ty
