{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module SplTypeChecker where

import SplAST

import Prelude
import Control.Monad.State
import Control.Monad.Except
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Leans strongly on http://dev.stephendiehl.com/fun/006_hindley_milner.html

type Name = String

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data SplSimpleTypeR = SplTypeConst SplBasicType
                    | SplTypeTupleR SplSimpleTypeR SplSimpleTypeR
                    | SplTypeListR SplSimpleTypeR
                    | SplTypeVar TVar
                    | SplVoid
        deriving (Show, Eq)

data SplTypeR = SplSimple SplSimpleTypeR
              | SplTypeFunction [SplSimpleTypeR] SplSimpleTypeR
        deriving (Show, Eq)

data Scheme = Forall [TVar] SplTypeR

data TypeKind
    = TPV -- TypePlaceholderVariable: like a in "a fiets = 1;"
    | Var -- Variable or function names
    deriving (Eq, Show, Ord)

-- a map from names to schemes
newtype TypeEnv = TypeEnv (Map.Map (TypeKind, Name) Scheme)
    deriving Monoid

data TypeError
    = UnificationFail SplTypeR SplTypeR
    | InfiniteType TVar SplTypeR
    | UnboundVariable Name
    | UnexpectedFunction SplTypeR
    | UnexpectedVoid

data SplEnv = SplEnv { count :: Int, typeEnv :: TypeEnv }

type Infer a = ExceptT TypeError (State SplEnv) a
type Subst = Map.Map TVar SplTypeR


runInfer :: Infer (Subst, SplTypeR) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initEnv of
    Left err -> Left err
    Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar SplTypeR, SplTypeR) -> Scheme
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normtype body)
    where
        ord = zip (nub $ fv body) (fmap TV letters)

        fv (SplSimple (SplTypeVar a)) = [a]
        fv (SplSimple (SplTypeListR a)) = fv (SplSimple a)
        fv (SplSimple (SplTypeTupleR l r)) = fv (SplSimple l) ++ fv (SplSimple r)
        fv (SplTypeFunction argTypes retType) = (concat . (map (fv . SplSimple))) argTypes ++ fv (SplSimple retType)
        fv _ = []

        normtype :: SplTypeR -> SplTypeR
        normtype (SplTypeFunction argTypes retType) = SplTypeFunction (map normtypeS argTypes)  (normtypeS retType)
        normtype (SplSimple t) = SplSimple $ normtypeS t

        normtypeS :: SplSimpleTypeR -> SplSimpleTypeR
        normtypeS a@(SplTypeConst _) = a
        normtypeS (SplTypeTupleR a b) = SplTypeTupleR (normtypeS a) (normtypeS b)
        normtypeS (SplTypeListR a) = SplTypeListR (normtypeS a)
        normtypeS SplVoid = SplVoid
        normtypeS (SplTypeVar a) =
            case lookup a ord of
                Just x -> SplTypeVar x
                Nothing -> error "type variable not in signature"


nullSubst :: Subst
nullSubst = Map.empty

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

initEnv :: SplEnv
initEnv = SplEnv {count = 0, typeEnv = emptyTypeEnv}

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

extend :: ((TypeKind, Name), Scheme) -> SplEnv -> SplEnv
extend (x, scheme) s@(SplEnv {typeEnv=TypeEnv env}) = s{typeEnv = TypeEnv $ Map.insert x scheme env}

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set TVar

instance Substitutable SplTypeR where
    apply s (SplSimple t) = SplSimple $ apply s t
    apply s (SplTypeFunction argTypes retType) = SplTypeFunction (apply s argTypes) (apply s retType)

    ftv (SplTypeFunction argTypes retType) = ftv argTypes `Set.union` ftv retType
    ftv (SplSimple t) = ftv t

instance Substitutable SplSimpleTypeR where
    apply _ t@(SplTypeConst _) = t
    apply s (SplTypeTupleR l r) = SplTypeTupleR (apply s l) (apply s r)
    apply s (SplTypeListR t) = SplTypeListR (apply s t)
    apply s t@(SplTypeVar a) = do
        case Map.findWithDefault (SplSimple t) a s of
            SplSimple r -> r
            _ -> t
    apply _ SplVoid = SplVoid

    ftv SplVoid = Set.empty
    ftv (SplTypeConst _) = Set.empty
    ftv (SplTypeTupleR l r) = ftv l `Set.union` ftv r
    ftv (SplTypeListR t) = ftv t
    ftv (SplTypeVar a) = Set.singleton a

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

instance Substitutable SplEnv where
    apply sub env = env{typeEnv = apply sub $ typeEnv env}
    ftv env = ftv $ typeEnv env

-- ["α", "β", ..., "ω", "αα", ... ]
letters :: [String]
letters = [1..] >>= flip replicateM ['α' .. 'ω']

-- Get fresh type vars
fresh :: Infer SplSimpleTypeR
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ SplTypeVar $ TV (letters !! count s)

-- Check if a type variable exists before we try to replace it
occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- unify
class Unify a where
    unify :: a -> a -> Infer Subst

instance Unify SplTypeR where
    unify (SplSimple t) (SplSimple t') = unify t t'
    unify (SplTypeFunction argTypes retType) (SplTypeFunction argTypes' retType') = do
        s1 <- recApply nullSubst argTypes argTypes'
        s2 <- unify (apply s1 retType) (apply s1 retType')
        return (s2 `compose` s1)
        where
            recApply _ [] [] = return nullSubst
            recApply s [x] [y] = do
                s1 <- unify x y
                return (s1 `compose` s)
            recApply s (x:x':xs) (y:y':ys) = do
                s1 <- unify x y
                recApply (s1 `compose` s) ((apply s1 x'):xs) ((apply s1 y'):ys)
            recApply _ _ _ = fail "Unequal number of arguments"

    unify t1 t2 = throwError $ UnificationFail t1 t2

instance Unify SplSimpleTypeR where
    unify (SplTypeConst a) (SplTypeConst b) | a == b = return nullSubst
    unify (SplTypeVar a) t = bind a t
    unify t (SplTypeVar a) = bind a t
    unify (SplTypeTupleR l r) (SplTypeTupleR l' r') = do
        s1 <- unify l l'
        s2 <- unify (apply s1 r) (apply s1 r')
        return (s2 `compose` s1)
    unify (SplTypeListR a) (SplTypeListR b) = unify a b
    unify t1 t2 = throwError $ UnificationFail (SplSimple t1) (SplSimple t2)

bind :: TVar -> SplSimpleTypeR -> Infer Subst
bind a t
  | t == SplTypeVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a (SplSimple t)
  | otherwise = return $ Map.singleton a (SplSimple t)

instantiate :: Scheme -> Infer SplTypeR
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = (Map.fromList $ zip as (map SplSimple as')) :: Subst
    return $ apply s t

generalize :: TypeEnv -> SplTypeR -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

returnSimple :: (Monad m) => Subst -> SplSimpleTypeR -> m (Subst, SplTypeR)
returnSimple s x = return (s, SplSimple x)

unsimple :: MonadError TypeError m => (s1, SplTypeR) -> m (s1, SplSimpleTypeR)
unsimple (s1, SplSimple s) = return (s1, s)
unsimple (_, t) = throwError $ UnexpectedFunction t

class Inferer a where
    infer :: a -> Infer (Subst, SplTypeR)

lookupEnv :: (TypeKind, Name) -> Infer (Subst, SplTypeR)
lookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

instance Inferer SplType where
    infer (SplType a) = returnSimple nullSubst (SplTypeConst a)
    infer (SplTypeList a) = do
        (s1, t1) <- infer a >>= unsimple
        returnSimple s1 (SplTypeListR t1)
    infer (SplTypeTuple l r) = do
        (s1, t1) <- infer l >>= unsimple
        modify (apply s1)
        (s2, t2) <- infer r >>= unsimple
        returnSimple (s2 `compose` s1) (SplTypeTupleR t1 t2)
    infer (SplTypeUnknown) = do
        var <- fresh
        returnSimple nullSubst var
    infer (SplTypePlaceholder str) = do
        (TypeEnv env) <- gets typeEnv
        case Map.lookup (TPV, str) env of
            Nothing -> do
                n <- SplSimple <$> fresh
                modify (extend ((TPV, str), Forall [] n))
                return (nullSubst, n)
            Just (Forall _ t) -> return (nullSubst, t)

{-

instance Inferer SplVarDecl where
    -- <type> <name> = <expr>;
    infer (SplVarDecl t name expr) = do
        -- get declared type
        (_, t') <- infer env t >>= unsimple
        (s1, et) <- infer env expr >>= unsimple
        let env' = apply s env
        s2 <- unify t' et
        --do something
        Map.insert env name (apply s2 t')

        -- Check expr
        -- unify with t'
        -- insert result for name into env

-}

{-

class SplTypeChecker a where
    typeCheck :: a -> SplTypeCheckResult

unsimple :: SplTypeR -> StateT Environment (Either String) SplSimpleTypeR
unsimple (SplSimple t) = return t
unsimple (SplTypeFunction _ _) = fail "Unexpected function type"

returnSimple :: SplSimpleTypeR -> SplTypeCheckResult
returnSimple x = return (SplSimple x)

disallowVoid :: SplSimpleTypeR -> StateT Environment (Either String) SplSimpleTypeR
disallowVoid (SplVoid) = fail "Void is not allowed"
disallowVoid t = return t

notVoid :: SplSimpleTypeR -> Bool
notVoid SplVoid = False
notVoid _       = True

instance SplTypeChecker SplType where
    typeCheck SplTypeUnknown = fail "Not allowed yet"
    typeCheck (SplTypePlaceholder _) = fail "Not allowed yet"
    typeCheck (SplTypeTuple l r) = do
        lt <- typeCheck l >>= unsimple >>= disallowVoid
        rt <- typeCheck r >>= unsimple >>= disallowVoid
        returnSimple (SplTypeTupleR lt rt)
    typeCheck (SplTypeList a) = do
        at <- typeCheck a >>= unsimple >>= disallowVoid
        returnSimple (SplTypeListR at)
    typeCheck (SplType t) = returnSimple $ SplTypeConst t


instance SplTypeChecker SplDecl where
    typeCheck (SplDeclVar varDecl) = typeCheck varDecl

    typeCheck (SplDeclFun name argNames argTypes retType varDecls stmts) = do
        declared <- gets (Map.member name)
        when declared
            (fail $ "Variable " ++ name ++ " already declared")

        when (nub argNames /= argNames) $
            fail $ "Duplicate argument names in function declaration " ++ name

        checkedArgTypes <- mapM (\x -> typeCheck x >>= unsimple) argTypes
        localState <- get
        globalState <- get

        let nameArgs = zip argNames (map SplSimple checkedArgTypes)
        let localState' = foldr (\(s, n) -> Map.insert s n) localState nameArgs
        put localState'

        mapM_ (\x -> typeCheck x >>= unsimple) varDecls

        retType' <- case retType of
            SplRetType t ->  typeCheck t >>= unsimple
            SplRetVoid -> return SplVoid

        blockType <- typeCheck stmts >>= unsimple

        when (blockType /= retType') $
            fail $ "Cannot unify return type " ++ (show retType) ++
                " with actual type " ++ (show blockType) ++ "."

        let functionType = SplTypeFunction checkedArgTypes retType'
        put (Map.insert name functionType globalState)

        return functionType


instance SplTypeChecker SplVarDecl where
    typeCheck (SplVarDecl varType name expr) = do
        t <- typeCheck varType
        e <- typeCheck expr
        _ <- unsimple e >>= disallowVoid
        if (t == e)
            then (do
                declared <- gets (Map.member name)
                when declared
                    (fail $ "Variable " ++ name ++ " already declared")
                modify (Map.insert name t)
                return t
            )
            else fail "Type mismatch when parsing varDecl"

typeCheckField :: SplSimpleTypeR -> SplField -> SplTypeCheckResult
typeCheckField t SplFieldNone = returnSimple t
typeCheckField (SplTypeListR t) (SplFieldHd f) = typeCheckField t f
typeCheckField t@(SplTypeListR _) (SplFieldTl f) = typeCheckField t f
typeCheckField (SplTypeTupleR l _) (SplFieldFst f) = typeCheckField l f
typeCheckField (SplTypeTupleR _ r) (SplFieldSnd f) = typeCheckField r f
typeCheckField _ (SplFieldHd _) = fail "Cannot take hd of a non-list"
typeCheckField _ (SplFieldTl _) = fail "Cannot take tl of a non-list"
typeCheckField _ (SplFieldFst _) = fail "Cannot take fst of a non-tuple"
typeCheckField _ (SplFieldSnd _) = fail "Cannot take snd of a non-tuple"

instance SplTypeChecker [SplStmt] where
    typeCheck [] = returnSimple SplVoid
    typeCheck (x:xs) = do
        xType <- typeCheck x >>= unsimple
        xsType <- typeCheck xs >>= unsimple
        case xType of
            SplVoid -> returnSimple xsType
            t1 -> case xsType of
                    SplVoid -> returnSimple xType
                    t2 -> if (t1 == t2)
                        then returnSimple t1
                        else fail $ "Different return types: " ++
                            (show t1) ++ " and " ++ (show t2) ++ "."


instance SplTypeChecker SplStmt where
    typeCheck (SplIfStmt cond thenStmts elseStmts) = do
        condType <- typeCheck cond >>= unsimple
        when (condType /= SplTypeConst SplBool) $
            fail $ "Condition needs to be a Bool instead of a " ++ (show condType)
        typeCheck (thenStmts ++ elseStmts)

    typeCheck (SplWhileStmt cond stmts) = do
        condType <- typeCheck cond >>= unsimple
        when (condType /= SplTypeConst SplBool) $
            fail $ "Condition needs to be a Bool instead of a " ++ (show condType)
        typeCheck stmts

    typeCheck (SplAssignmentStmt name field expr) = do
        varType' <- gets (Map.lookup name)
        varType <- case varType' of
            Just t -> unsimple t >>= (flip typeCheckField field) >>= unsimple
            Nothing -> fail $ "Undeclared variable " ++ name
        exprType <- typeCheck expr >>= unsimple >>= disallowVoid
        if (exprType == varType)
            then returnSimple SplVoid
            else fail $ "Cannot assign result of type " ++ (show exprType) ++
                    " to variable of type " ++ (show varType) ++ "."

    typeCheck (SplFuncCallStmt name args) = checkFunctionCall name args

    typeCheck (SplReturnStmt e) =
            typeCheck e >>= unsimple >>= disallowVoid >>= returnSimple

    typeCheck SplReturnVoidStmt = returnSimple SplVoid


instance SplTypeChecker SplExpr where
    typeCheck (SplIntLiteralExpr _) = returnSimple $ SplTypeConst SplInt
    typeCheck (SplBooleanLiteralExpr _) = returnSimple $ SplTypeConst SplBool
    typeCheck (SplCharLiteralExpr _) = returnSimple $ SplTypeConst SplChar
    typeCheck SplEmptyListExpr = fail "empty lists are weird without inference"

    typeCheck (SplTupleExpr l r) = do
            lType <- typeCheck l >>= unsimple >>= disallowVoid
            rType <- typeCheck r >>= unsimple >>= disallowVoid
            -- FIXME functions in tuples
            returnSimple $ SplTypeTupleR lType rType

    typeCheck (SplIdentifierExpr name field) = do
        varType <- gets (Map.lookup name)
        -- todo check if it's a function
        case varType of
            Just t -> unsimple t >>= (flip typeCheckField field)
            Nothing -> fail $ "Undeclared variable " ++ name

    typeCheck (SplUnaryExpr op expr) = do
        exprType <- typeCheck expr >>= unsimple >>= disallowVoid
        case op of
            SplOperatorInvert -> if (exprType == SplTypeConst SplBool)
                then returnSimple exprType
                else fail "Cannot invert a non-Bool"
            SplOperatorNegate -> if (exprType == SplTypeConst SplInt)
                then returnSimple exprType
                else fail "Cannot invert a non-Int"

    typeCheck (SplBinaryExpr SplOperatorCons e1 e2) = do
        e1Type <- typeCheck e1 >>= unsimple >>= disallowVoid
        e2Type <- typeCheck e2 >>= unsimple >>= disallowVoid
        case e2Type of
            SplTypeListR t2 -> if (e1Type == t2)
                then returnSimple e2Type
                else fail $ "Expected list of type " ++ (show e1Type)
            _ -> fail "Cons expects a list as second argument"

    typeCheck (SplBinaryExpr op e1 e2) = do
        e1Type <- typeCheck e1 >>= unsimple >>= disallowVoid
        e2Type <- typeCheck e2 >>= unsimple >>= disallowVoid
        when (e1Type /= e2Type) (
            fail $ "Both operands need to be of the same type\n"
                ++ "Actual: " ++ (show e1Type) ++ " " ++ (show op)
                ++ " " ++ (show e2Type))
        typeCheckBinOp op e1Type

    typeCheck (SplFuncCallExpr name args) = checkFunctionCall name args


checkFunctionCall :: String -> [SplExpr] -> SplTypeCheckResult
checkFunctionCall name args = do
    funcType <- gets (Map.lookup name)
    case funcType of
        Nothing -> fail $ "Undeclared function name " ++ name
        Just (SplSimple _) -> fail "Expected function, got variable"
        Just (SplTypeFunction argTypes retType) -> (do
                when (length args /= length argTypes) (
                        fail ("Got " ++ (show . length) args ++
                              " arguments but expected " ++ (show . length) argTypes))
                argTypeResults <- mapM typeCheck args
                let result = all checkArgType (zip argTypes argTypeResults)
                if result
                    then returnSimple retType
                    else fail ("Error checking arguments for " ++ name ++ ":\n"
                                 ++ "Expected: " ++ (intercalate ", " (map show args)) ++ "\n"
                                 ++ "Got:      " ++ (intercalate ", " (map show argTypes)))

            )
    where
        checkArgType :: (SplSimpleTypeR, SplTypeR) -> Bool
        checkArgType (expected, SplSimple actual) = actual == expected && notVoid actual
        checkArgType _ = False

typeCheckBinOp :: SplBinaryOperator -> SplSimpleTypeR -> SplTypeCheckResult
typeCheckBinOp SplOperatorAdd t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorAdd t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorSubtract t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorSubtract t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorMultiply t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorDivide t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorModulus t@(SplTypeConst SplInt) = returnSimple t

typeCheckBinOp SplOperatorLess t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorLess t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorLessEqual t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorLessEqual t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorEqual t@(SplTypeConst _) = returnSimple t
typeCheckBinOp SplOperatorEqual t@(SplTypeTupleR _ _) = returnSimple t
typeCheckBinOp SplOperatorEqual t@(SplTypeListR _) = returnSimple t

typeCheckBinOp SplOperatorGreaterEqual t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorGreaterEqual t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorGreater t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorGreater t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorNotEqual t@(SplTypeConst _) = returnSimple t
typeCheckBinOp SplOperatorNotEqual t@(SplTypeTupleR _ _) = returnSimple t
typeCheckBinOp SplOperatorNotEqual t@(SplTypeListR _) = returnSimple t

typeCheckBinOp SplOperatorAnd t@(SplTypeConst SplBool) = returnSimple t
typeCheckBinOp SplOperatorOr t@(SplTypeConst SplBool) = returnSimple t

typeCheckBinOp SplOperatorCons _ = fail "Don't use typeCheckBinOp on Cons, dummy"
typeCheckBinOp o t = fail $ "Unsupported operands of type " ++ (show t) ++ " for operator " ++ (show o)
-}
