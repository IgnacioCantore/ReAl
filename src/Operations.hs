module Operations where

import           Monad
import           Common
import           Control.Applicative
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Data.Maybe

-----------------------------------
--- Operaciones
-----------------------------------

-- Selección
sel :: MonadStateError m => Name -> Table -> Cond -> m Table
sel n t@(T as ts _) c = do rs' <- sel' n t c
                           return (T as ts rs')
 where
   sel' :: MonadStateError m => Name -> Table -> Cond -> m (S.Set Row)
   sel' n t@(T _ _ _) (And c1 c2) = do r1 <- sel' n t c1
                                       r2 <- sel' n t c2
                                       return (S.intersection r1 r2)
   sel' n t@(T _ _ _) (Or c1 c2)  = do r1 <- sel' n t c1
                                       r2 <- sel' n t c2
                                       return (S.union r1 r2)
   sel' n t@(T _ _ rs) (Not c)    = do rs' <- sel' n t c
                                       return (S.difference rs rs')
   sel' n           t       c     = do let (op,v1,v2) = getOperatorValues c
                                       filterRows n t op v1 v2

-- Proyección
proj :: MonadStateError m => Name -> Table -> Attributes -> m Table
proj n (T as ts rs) a =
  if repeated n a
  then throw "No puede haber atributos repetidos en la proyección"
  else let indices = map (findIndex n as) a
       in if elem Nothing indices
          then throw "Uno de los atributos dados no existe en la relación"
          else let indices' = map fromJust indices
                   as' = map (as !!) indices'
                   ts' = map (ts !!) indices'
                   rs' = S.map (\row -> map (row !!) indices') rs
               in return (T as' ts' rs')

-- Renombramiento
rename :: MonadStateError m => Name -> Table -> m Table
rename n' (T as ts rs) =
  let as' = map (renameAttr n') as
  in if repeated n' as'
     then throw "No se puede renombrar porque quedan atributos repetidos"
     else return (T as' ts rs)

-- Unión
union :: MonadStateError m => Table -> Table -> m Table
union (T a1 t1 r1) (T _ t2 r2) =
  if t1 == t2
  then return (T a1 t1 (S.union r1 r2))
  else throw "No se puede aplicar la unión porque las relaciones tienen atributos distintos"

-- Diferencia
diff :: MonadStateError m => Table -> Table -> m Table
diff (T a1 t1 r1) (T _ t2 r2) =
  if t1 == t2
  then return (T a1 t1 (S.difference r1 r2))
  else throw "No se puede aplicar la diferencia porque las relaciones tienen atributos distintos"

-- Producto cartesiano
prod :: MonadStateError m => Name -> Name -> Table -> Table -> m Table
prod n1 n2 (T a1 t1 r1) (T a2 t2 r2) =
  if n1 == n2 || repeated n1 a2 || repeated n2 a1
  then throw "No se puede aplicar el producto cartesiano porque quedan atributos repetidos"
  else let as = renameRepeated n1 n2 a1 a2
       in return (T as (t1 ++ t2) (prod' r1 r2))
 where
   -- Producto cartesiano de las filas
   prod' :: S.Set Row -> S.Set Row -> S.Set Row
   prod' r1 r2 = S.fromList $ liftA2 (++) (S.toList r1) (S.toList r2)

-----------------------------------
--- Funciones auxiliares
-----------------------------------

-- Busca el atributo dado dentro de la lista de atributos y, si pertenece, devuelve su índica
findIndex :: Name -> Attributes -> Value -> Maybe Int
findIndex n as at@(AttrVal r a) =
  case L.elemIndex at as of
    Just i  -> Just i
    Nothing -> if n == r then L.elemIndex (AttrVal "" a) as else Nothing

-- Verifica si la lista de atributos dada contiene nombres repetidos
repeated :: Name -> Attributes -> Bool
repeated n as =
  let withRelName = filter (\(AttrVal r a) -> not (null r) || not (elem (AttrVal n a) as)) as
  in length as /= length (L.nub as) || length as /= length withRelName

-- Tipo de los operadores de comparación
type Comp = (Value -> Value -> Bool)

-- Dada una comparación Cond, devuelve una tupla con el operador equivalente y los valores a comparar
getOperatorValues :: Cond -> (Comp, Value, Value)
getOperatorValues (Eq v1 v2) = ((==),v1,v2)
getOperatorValues (NEq v1 v2) = ((/=),v1,v2)
getOperatorValues (Lt v1 v2) = ((<),v1,v2)
getOperatorValues (Gt v1 v2) = ((>),v1,v2)
getOperatorValues (LtE v1 v2) = ((<=),v1,v2)
getOperatorValues (GtE v1 v2) = ((>=),v1,v2)

-- Filtra las filas que cumplan con la condición dada
filterRows :: MonadStateError m => Name -> Table -> Comp -> Value -> Value -> m (S.Set Row)
filterRows n (T as ts rs) op at1@(AttrVal r1 a1) at2@(AttrVal r2 a2) =
  case findIndex n as at1 of
    Nothing -> throw "Uno de los atributos dados no existe en la relación"
    Just i  -> case findIndex n as at2 of
                 Nothing -> throw "Uno de los atributos dados no existe en la relación"
                 Just j  -> if (ts !! i) /= (ts !! j)
                            then throw "Los valores a comparar tienen tipos distintos"
                            else return (S.filter
                                          (\row -> case (row !! i) of
                                             s@(StringVal _) -> op s (row !! j)
                                             num             -> compNum op num (row !! j))
                                          rs)
filterRows n (T as ts rs) op v at@(AttrVal r a) =
  case findIndex n as at of
    Nothing -> throw "Uno de los atributos dados no existe en la relación"
    Just i  -> case v of
                 s@(StringVal _) -> if (ts !! i) /= Str
                                    then throw "Los valores a comparar tienen tipos distintos"
                                    else return (S.filter (\row -> op s (row !! i)) rs)
                 num             -> if (ts !! i) /= Num
                                    then throw "Los valores a comparar tienen tipos distintos"
                                    else return (S.filter (\row -> compNum op num (row !! i)) rs)
filterRows n (T as ts rs) op at@(AttrVal r a) v =
  case findIndex n as at of
    Nothing -> throw "Uno de los atributos dados no existe en la relación"
    Just i  -> case v of
                 s@(StringVal _) -> if (ts !! i) /= Str
                                    then throw "Los valores a comparar tienen tipos distintos"
                                    else return (S.filter (\row -> op (row !! i) s) rs)
                 num             -> if (ts !! i) /= Num
                                    then throw "Los valores a comparar tienen tipos distintos"
                                    else return (S.filter (\row -> compNum op (row !! i) num) rs)
filterRows _ _ _ _ _ =
  throw "Al menos uno de los valores a comparar debe ser un atributo"

-- Permite comparar dos valores númericos aunque sean de distinto tipo
compNum :: Comp -> Value -> Value -> Bool
compNum op n1@(IntVal _)       n2@(IntVal _) = op n1 n2
compNum op (IntVal n1)                    d2 = op (DoubleVal (fromIntegral n1)) d2
compNum op d1@(DoubleVal _) d2@(DoubleVal _) = op d1 d2
compNum op d1                    (IntVal n2) = op d1 (DoubleVal (fromIntegral n2))

-- Cambia los nombres de los atributos para que lleven el nuevo nombre de la relación
renameAttr :: Name -> Value -> Value
renameAttr n attr@(AttrVal r a) = if null r then attr else (AttrVal n a)

-- Si existen nombres de atributos repetidos, les agrega el nombre de su respectiva relación
renameRepeated :: Name -> Name -> Attributes -> Attributes -> Attributes
renameRepeated n1 n2 a1 a2 =
  let a1' = [if elem attr a2 then AttrVal n1 a else attr | attr@(AttrVal _ a) <- a1]
      a2' = [if elem attr a1 then AttrVal n2 a else attr | attr@(AttrVal _ a) <- a2]
  in a1' ++ a2'
