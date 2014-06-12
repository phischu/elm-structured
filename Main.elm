import Mouse

type Selected = Bool
data Expr = Plus Selected Expr Expr | Value Selected Int

testexpr : Expr
testexpr = Plus False (Plus True (Value False 5) (Value False 4)) (Value False 4)

render : Expr -> Element
render expr = case expr of
    (Plus selected lhs rhs) -> color (selectedColor selected) (flow right [keyword "(",render lhs,keyword "+",render rhs,keyword ")"])
    (Value selected v) -> color (selectedColor selected) (leftAligned (toText (show v)))

keyword : String -> Element
keyword s = leftAligned (toText s)

selectedColor : Selected -> Color
selectedColor selected = if selected then red else transparent

transparent : Color
transparent = rgba 255 255 255 0

main = constant (render testexpr)