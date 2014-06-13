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
testexpr = plus 0 (plus 1 (value 2 5) (value 3 8)) (value 4 4)

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

type Rewrites a = [Rewrite a]
type Rewrite a = ([String],a)

rewrite : Fold (Rewrites Expr)
rewrite = {selected = rewriteSelected,standard = rewriteStandard}

rewriteSelected : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteSelected uid exprf = filter ((\l -> length l > 0) . fst) (commute uid exprf ++ assocl uid exprf ++ assocr uid exprf)

commute : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
commute uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> [(["commute"],Expr uid (PlusF rhs lhs))]))
    ValueF v -> returnRewrites (Expr uid (ValueF v))

assocl : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
assocl uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case rhs of
            Expr rhsuid rhsexprf -> case rhsexprf of
                PlusF rlhs rrhs -> [(["assocl"],Expr uid (PlusF (Expr rhsuid (PlusF lhs rlhs)) rrhs))]
                ValueF v -> returnRewrites (Expr uid (PlusF lhs rhs))))
    ValueF v -> returnRewrites (Expr uid (ValueF v))

assocr : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
assocr uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                PlusF llhs lrhs -> [(["assocr"],Expr uid (PlusF llhs (Expr lhsuid (PlusF lrhs rhs))))]
                ValueF v -> returnRewrites (Expr uid (PlusF lhs rhs))))
    ValueF v -> returnRewrites (Expr uid (ValueF v))

rewriteStandard : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteStandard uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> returnRewrites (Expr uid (PlusF lhs rhs))))
    ValueF v -> returnRewrites (Expr uid (ValueF v))

returnRewrites : a -> Rewrites a
returnRewrites a = [([],a)]

mapRewrites : (a -> b) -> Rewrites a -> Rewrites b
mapRewrites f = map (\(n,a) -> (n,f a))

joinRewrites : Rewrites (Rewrites a) -> Rewrites a
joinRewrites = concatMap (\(n,r) -> map (\(n',a) -> (n ++ n',a)) r)

bindRewrites : Rewrites a -> (a -> Rewrites b) -> Rewrites b
bindRewrites r f = joinRewrites (mapRewrites f r)

renderRewrites : UID -> Rewrites Expr -> Element
renderRewrites selecteduid = flow down . map (renderRewrite selecteduid)

renderRewrite : UID -> Rewrite Expr -> Element
renderRewrite selecteduid (name,expr) = keyword (show name)
    |> clickable inputexpr.handle {selecteduid = selecteduid,expr = expr}

main = lift (\sexpr ->
    runFold (render sexpr) sexpr `beside`
    renderRewrites sexpr.selecteduid (runFold rewrite sexpr)) inputexpr.signal