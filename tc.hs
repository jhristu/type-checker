-- file: tc.hs
import Data.Char

type Var = Char
type Loc = Int

data Tm = TmTrue
        | TmFalse
        | TmIf Tm Tm Tm
        | TmZero
        | TmSucc Tm
        | TmPred Tm
        | TmIsZero Tm
        | TmVar Var
        | TmAbs Var Ty Tm
        | TmApp Tm Tm
        | TmFix Tm
        | TmUnit
        | TmRef Tm
        | TmDeref Tm
        | TmAssign Tm Tm
        | TmLoc Loc
          
data Ty = TyBool
        | TyNat
        | TyArr Ty Ty
        | TyUnit
        | TyRef Ty
          deriving Eq

instance Show Ty where
    show (TyBool) = "Bool"
    show (TyNat) = "Nat"
    show (TyArr ty1 ty2) = "(" ++ show ty1 ++ " -> " ++ show ty2 ++ ")"
    show (TyUnit) = "Unit"
    show (TyRef ty) = "(" ++ "Ref " ++ show ty ++ ")"

data Ctx = Ctx [(Var, Ty)]

data Sty = Sty [(Loc, Ty)]

typeof :: Ctx -> Sty -> Tm -> Ty
typeof ctx sty (TmTrue) = TyBool
typeof ctx sty (TmFalse) = TyBool
typeof ctx sty (TmIf t1 t2 t3) =
    if typeof ctx sty t1 == TyBool
    then
        let tyt2 = typeof ctx sty t2 in
        let tyt3 = typeof ctx sty t3 in
        if tyt3 == tyt2
        then
            tyt2
        else
            error "arms of conditional have different types"
    else
        error "guard of conditional is not a boolean"
typeof ctx sty (TmZero) = TyNat
typeof ctx sty (TmSucc t) =
    if typeof ctx sty t == TyNat
    then
        TyNat
    else
        error "can't take succ of non Nat type"
typeof ctx sty (TmPred t) =
    if typeof ctx sty t == TyNat
    then
        TyNat
    else
        error "can't take pred of non Nat type"
typeof ctx sty (TmIsZero t) =
    if typeof ctx sty t == TyNat
    then
        TyBool
    else
        error "can't test iszero on non Nat type"
typeof ctx sty (TmVar x) = gettypefromcontext ctx x
typeof ctx sty (TmAbs x tyt1 t2) =
    let ctx' = addvariablebinding ctx x tyt1 in
    let tyt2 = typeof ctx' sty t2 in
    (TyArr tyt1 tyt2)
typeof ctx sty (TmApp t1 t2) =
    let tyt1 = typeof ctx sty t1 in
    let tyt2 = typeof ctx sty t2 in
    case tyt1 of
      (TyArr tyt11 tyt12)
          ->
            if tyt11 == tyt2
            then
                tyt12
            else
                error "parameter type mismatch"
      _
          ->
            error "can't apply non arrow type"
typeof ctx sty (TmFix t1) =
    let tyt1 = typeof ctx sty t1 in
    case tyt1 of
      (TyArr tyt11 tyt12)
          ->
            if tyt11 == tyt12
            then
                tyt11
            else
                error "arrow type mismatch for fix operator"
      _
          ->
            error "can't fix non arrow type"
typeof ctx sty (TmUnit) = TyUnit
typeof ctx sty (TmRef t) =
    let tyt = typeof ctx sty t in
    (TyRef tyt)
typeof ctx sty (TmDeref t) =
    let tyt = typeof ctx sty t in
    case tyt of
      (TyRef tyt1)
          ->
            tyt1
      _
          ->
            error "can't dereference non reference"
typeof ctx sty (TmAssign t1 t2) =
    let tyt1 = typeof ctx sty t1 in
    let tyt2 = typeof ctx sty t2 in
    case tyt1 of
      (TyRef tyt11)
          ->
            if tyt11 == tyt2
            then
                TyUnit
            else
                error "assignment type mismatch"
      _
          ->
            error "can't assign value to non reference type"
typeof ctx sty (TmLoc l) =
    let tyl = gettypefromstoretypings sty l in
    (TyRef tyl)

gettypefromcontext :: Ctx -> Var -> Ty
gettypefromcontext (Ctx ((v, t):os)) x = 
    if v == x
    then
        t
    else
        gettypefromcontext (Ctx os) x
gettypefromcontext (Ctx []) x = error "variable not in context"

addvariablebinding :: Ctx -> Var -> Ty -> Ctx
addvariablebinding (Ctx os) x ty = (Ctx ((x, ty) : os))

gettypefromstoretypings :: Sty -> Loc -> Ty
gettypefromstoretypings (Sty ((l1, t):os)) l =
    if l1 == l
    then
        t
    else
        gettypefromstoretypings (Sty os) l

addlocationtyping :: Sty -> Loc -> Ty -> Sty
addlocationtyping (Sty os) l ty = (Sty ((l, ty) : os))

nullctx = (Ctx [])
nullsty = (Sty [])
