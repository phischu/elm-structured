import Graphics.Input (Input,input,clickable)

type UID = Int
data ExprF a = PlusF a a | ValueF Int
data Expr = Expr UID (ExprF Expr)
type SelectedExpr = {selecteduid : UID,expr : Expr}

type Fold a = {
    selected : UID -> ExprF a -> a,
    standard : UID -> ExprF a -> a}

runFold : Fold a -> SelectedExpr -> a
runFold fold {selecteduid,expr} = case expr of
    Expr uid exprf ->
        (if selecteduid == uid then fold.selected uid else fold.standard uid) (
            case exprf of
                PlusF lhs rhs ->
                    PlusF
                        (runFold fold {selecteduid = selecteduid,expr = lhs})
                        (runFold fold {selecteduid = selecteduid,expr = rhs})
                ValueF v ->
                    ValueF v)

inputexpr : Input SelectedExpr
inputexpr = input {selecteduid=1,expr=testexpr}

plus : UID -> Expr -> Expr -> Expr
plus uid lhs rhs = Expr uid (PlusF lhs rhs)

value : UID -> Int -> Expr
value uid v = Expr uid (ValueF v)

testexpr : Expr
testexpr = plus 0 (plus 1 (value 2 5) (value 3 4)) (value 4 4)

render : SelectedExpr -> Fold Element
render sexpr = {selected = renderSelected sexpr,standard = renderStandard sexpr}

renderSelected : SelectedExpr -> UID -> ExprF Element -> Element
renderSelected original uid exprf = color lightYellow (renderStandard original uid exprf)

renderStandard : SelectedExpr -> UID -> ExprF Element -> Element
renderStandard original uid exprf = case exprf of
    PlusF lhs rhs -> flow right [
        keyword "(",
        lhs,
        keyword "+",
        rhs,
        keyword ")"]
        |> clickable inputexpr.handle {selecteduid=uid,expr=original.expr}
    (ValueF v) -> leftAligned (toText (show v))
        |> clickable inputexpr.handle {selecteduid=uid,expr=original.expr}

keyword : String -> Element
keyword s = leftAligned (toText s)

type Rewrite a = [([String],a)]

rewriteStandard : UID -> ExprF (Rewrite Expr) -> Rewrite Expr
rewriteStandard uid exprf = case exprf of
    PlusF lhsrewrite rhsrewrite -> bindRewrite lhsrewrite (\lhs ->
        bindRewrite rhsrewrite (\rhs -> returnRewrite (Expr uid (PlusF lhs rhs))))
    ValueF v -> returnRewrite (Expr uid (ValueF v))

returnRewrite : a -> Rewrite a
returnRewrite a = [([],a)]

mapRewrite : (a -> b) -> Rewrite a -> Rewrite b
mapRewrite f = map (\(n,a) -> (n,f a))

joinRewrite : Rewrite (Rewrite a) -> Rewrite a
joinRewrite = concatMap (\(n,r) -> map (\(n',a) -> (n ++ n',a)) r)

bindRewrite : Rewrite a -> (a -> Rewrite b) -> Rewrite b
bindRewrite r f = joinRewrite (mapRewrite f r)

main = lift (\sexpr -> runFold (render sexpr) sexpr) inputexpr.signal