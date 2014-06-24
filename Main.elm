import Graphics.Input (Input,input,clickable)

type UID = Int
data ExprF a = PlusF a a | ZeroF | VariableF String
data Expr = Expr UID (ExprF Expr)
type State = {nextuid : UID,selecteduid : UID,expr : Expr}

traverse : (UID -> ExprF a -> a) -> Expr -> a
traverse f expr = case expr of
    Expr uid exprf ->
        f uid (
            case exprf of
                PlusF lhs rhs -> PlusF (traverse f lhs) (traverse f rhs)
                ZeroF -> ZeroF
                VariableF name -> VariableF name)

inputexpr : Input State
inputexpr = input {nextuid=5,selecteduid=1,expr=testexpr}

plus : UID -> Expr -> Expr -> Expr
plus uid lhs rhs = Expr uid (PlusF lhs rhs)

zero : UID -> Expr
zero uid = Expr uid ZeroF

variable : UID -> String ->Expr
variable uid name = Expr uid (VariableF name)

testexpr : Expr
testexpr = plus 0 (plus 1 (variable 2 "x") (variable 3 "x")) (zero 4)

type Render = State -> Element

render : UID -> ExprF Render -> Render
render uid exprf state =
    (if uid == state.selecteduid then color lightYellow else id) (
        clickable inputexpr.handle {state | selecteduid <- uid} (
            case exprf of
                PlusF lhs rhs -> flow right [
                    keyword "(",
                    lhs state,
                    keyword "+",
                    rhs state,
                    keyword ")"]
                ZeroF -> leftAligned (toText "0")
                VariableF name -> keyword name))

keyword : String -> Element
keyword s = leftAligned (toText s)
{-
type Rewrites a = UID -> (UID,[Rewrite a])
type Rewrite a = ([String],a)

rewrite : Fold (Rewrites Expr)
rewrite = {selected = rewriteSelected,standard = rewriteStandard}

rewriteSelected : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteSelected uid exprf = bindRewrites (rewriteStandard uid exprf) (\expr ->
    concatMap (\r -> r expr) [commute,assocl,assocr,zerol,zeror])

commute : Expr -> Rewrites Expr
commute expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> singleRewrites "commute" (plus uid rhs lhs)
        _ -> []

assocl : Expr -> Rewrites Expr
assocl expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case rhs of
                Expr rhsuid rhsexprf -> case rhsexprf of
                    PlusF rlhs rrhs -> singleRewrites "assocl" (plus uid (plus rhsuid lhs rlhs) rrhs)
                    _ -> []
            _ -> []

assocr : Expr -> Rewrites Expr
assocr expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case lhs of
                Expr lhsuid lhsexprf -> case lhsexprf of
                    PlusF llhs lrhs -> singleRewrites "assocr" (plus uid llhs (plus lhsuid lrhs rhs))
                    _ -> []
            _ -> []

zerol : Expr -> Rewrites Expr
zerol expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr _ lhsexprf -> case lhsexprf of
                ZeroF -> singleRewrites "zerol" rhs
                _ -> []
        _ -> []

zeror : Expr -> Rewrites Expr
zeror expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case rhs of
            Expr _ rhsexprf -> case rhsexprf of
                ZeroF -> singleRewrites "zeror" lhs
                _ -> []
        _ -> []

rewriteStandard : UID -> ExprF (Rewrites Expr) -> Rewrites Expr
rewriteStandard uid exprf = case exprf of
    PlusF lhsRewrites rhsRewrites -> bindRewrites lhsRewrites (\lhs ->
        bindRewrites rhsRewrites (\rhs -> returnRewrites (plus uid lhs rhs)))
    ZeroF -> returnRewrites (zero uid)
    VariableF name -> returnRewrites (variable uid name)

singleRewrites : String -> a -> Rewrites a
singleRewrites s a uid = (uid,[([s],a)])

returnRewrites : a -> Rewrites a
returnRewrites a uid = (uid,[([],a)])

mapRewrites : (a -> b) -> Rewrites a -> Rewrites b
mapRewrites f urs uid = case urs uid of
    (newuid,rs) -> (newuid,map (\(n,a) -> (n,f a)) rs)

joinRewrites : Rewrites (Rewrites a) -> Rewrites a
joinRewrites urrs uid = case urrs uid of
    (newuid,rurs) -> go newuid rurs

go : UID -> [([String],UID ->(UID,[([String],s)]))] -> (UID,[([String],a)])
go uid rus = case rus of
    [] -> (uid,[])

bindRewrites : Rewrites a -> (a -> Rewrites b) -> Rewrites b
bindRewrites r f = joinRewrites (mapRewrites f r)

renderRewrites : State -> Rewrites Expr -> Element
renderRewrites state rs = flow down . map (renderRewrite state)

renderRewrite : State -> Rewrite Expr -> Element
renderRewrite state (name,expr) = keyword (show name)
    |> clickable inputexpr.handle {selecteduid = selecteduid,expr = expr}
-}
main = lift (\state -> traverse render state.expr state) inputexpr.signal