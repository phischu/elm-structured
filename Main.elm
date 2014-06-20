import Graphics.Input (Input,input,clickable)

type UID = Int
data ExprF a = PlusF a a | ZeroF | VariableF String
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
                ZeroF ->
                    ZeroF
                VariableF name ->
                    VariableF name)

inputexpr : Input SelectedExpr
inputexpr = input {selecteduid=1,expr=testexpr}

plus : UID -> Expr -> Expr -> Expr
plus uid lhs rhs = Expr uid (PlusF lhs rhs)

zero : UID -> Expr
zero uid = Expr uid ZeroF

variable : UID -> String ->Expr
variable uid name = Expr uid (VariableF name)

testexpr : Expr
testexpr = plus 0 (plus 1 (variable 2 "x") (variable 3 "x")) (zero 4)

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
    ZeroF -> leftAligned (toText "0")
        |> clickable inputexpr.handle {selecteduid=uid,expr=original.expr}
    VariableF name -> keyword name
        |> clickable inputexpr.handle {selecteduid=uid,expr=original.expr}

keyword : String -> Element
keyword s = leftAligned (toText s)

type Rewrites a = [Rewrite a]
type Rewrite a = ([String],a)

rewrite : Fold (Rewrites Expr)
rewrite = {selected = rewriteSelected,standard = rewriteStandard}

rewriteSelected : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteSelected uid exprf = filter ((\l -> length l > 0) . fst) (concatMap (\r -> r uid exprf) [
    commute,assocl,assocr,zerol,zeror])

commute : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
commute uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> singleRewrites "commute" (plus uid rhs lhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

assocl : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
assocl uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case rhs of
            Expr rhsuid rhsexprf -> case rhsexprf of
                PlusF rlhs rrhs -> singleRewrites "assocl" (plus uid (plus rhsuid lhs rlhs) rrhs)
                _ -> returnRewrites (plus uid lhs rhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

assocr : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
assocr uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                PlusF llhs lrhs -> singleRewrites "assocr" (plus uid llhs (plus lhsuid lrhs rhs))
                _ -> returnRewrites (plus uid lhs rhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

zerol : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
zerol uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                ZeroF -> singleRewrites "zerol" rhs
                _ -> returnRewrites (plus uid lhs rhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

zeror : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
zeror uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> case rhs of
            Expr rhsuid rhsexprf -> case rhsexprf of
                ZeroF -> singleRewrites "zeror" lhs
                _ -> returnRewrites (plus uid lhs rhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

rewriteStandard : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteStandard uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> returnRewrites (Expr uid (PlusF lhs rhs))))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

singleRewrites : String -> a -> Rewrites a
singleRewrites s a = [([s],a)]

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