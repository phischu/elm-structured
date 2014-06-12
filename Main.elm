import Graphics.Input (Input,input)

type Selected = Bool
data Expr = Plus Selected Expr Expr | Value Selected Int

exprinput : Input Expr
exprinput = input testexpr

testexpr : Expr
testexpr = Plus False (Plus False (Value False 5) (Value False 4)) (Value True 4)

render : Expr -> Element
render expr = case expr of
    (Plus selected lhs rhs) -> flow right [keyword "(",render lhs,keyword "+",render rhs,keyword ")"]
        |> color (selectedColor selected)
    (Value selected v) -> leftAligned (toText (show v))
        |> color (selectedColor selected)

keyword : String -> Element
keyword s = leftAligned (toText s)

selectedColor : Selected -> Color
selectedColor selected = if selected then lightYellow else transparent

transparent : Color
transparent = rgba 255 255 255 0

main = constant (render testexpr)