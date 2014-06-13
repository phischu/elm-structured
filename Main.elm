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

main = lift (\sexpr -> runFold (render sexpr) sexpr) inputexpr.signal