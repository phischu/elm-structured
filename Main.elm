import Graphics.Input (Input,input,clickable)

type UID = Int
data Expr = Plus UID Expr Expr | Value UID Int
type SelectedExpr = {selected : UID,expr : Expr}
inputexpr : Input SelectedExpr
inputexpr = input {selected=1,expr=testexpr}

testexpr : Expr
testexpr = Plus 0 (Plus 1 (Value 2 5) (Value 3 4)) (Value 4 4)

render : SelectedExpr -> Element
render sexpr = render' sexpr sexpr.expr

render' : SelectedExpr -> Expr -> Element
render' original expr = case expr of
    (Plus uid lhs rhs) -> flow right [
        keyword "(",
        render' original lhs,
        keyword "+",
        render' original rhs,
        keyword ")"]
        |> color (selectedColor (original.selected==uid))
        |> clickable inputexpr.handle {selected=uid,expr=original.expr}
    (Value uid v) -> leftAligned (toText (show v))
        |> color (selectedColor (original.selected==uid))
        |> clickable inputexpr.handle {selected=uid,expr=original.expr}

keyword : String -> Element
keyword s = leftAligned (toText s)

selectedColor : Bool -> Color
selectedColor selected = if selected then lightYellow else transparent

transparent : Color
transparent = rgba 255 255 255 0

main = lift render inputexpr.signal