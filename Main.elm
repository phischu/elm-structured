import Graphics.Input (Input,input,clickable)

type UID = Int
data ExprF a = PlusF a a | TimesF a a | ZeroF | VariableF String
data Expr = Expr UID (ExprF Expr)
type State = {nextuid : UID,selecteduid : UID,expr : Expr}

traverse : (UID -> ExprF a -> a) -> Expr -> a
traverse f expr = case expr of
    Expr uid exprf ->
        f uid (
            case exprf of
                PlusF lhs rhs -> PlusF (traverse f lhs) (traverse f rhs)
                TimesF lhs rhs -> TimesF (traverse f lhs) (traverse f rhs)
                ZeroF -> ZeroF
                VariableF name -> VariableF name)

inputexpr : Input State
inputexpr = input {nextuid=7,selecteduid=1,expr=testexpr}

plus : UID -> Expr -> Expr -> Expr
plus uid lhs rhs = Expr uid (PlusF lhs rhs)

times : UID -> Expr -> Expr -> Expr
times uid lhs rhs = Expr uid (TimesF lhs rhs)

zero : UID -> Expr
zero uid = Expr uid ZeroF

variable : UID -> String ->Expr
variable uid name = Expr uid (VariableF name)

testexpr : Expr
testexpr = plus 0 (plus 1 (variable 2 "x") (variable 3 "x")) (times 4 (zero 5) (variable 6 "y"))

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
                TimesF lhs rhs -> flow right [
                    keyword "(",
                    lhs state,
                    keyword "*",
                    rhs state,
                    keyword ")"]
                ZeroF -> leftAligned (toText "0")
                VariableF name -> keyword name))

keyword : String -> Element
keyword s = leftAligned (toText s)


type RewriteName = [String]
type Rewrite a = State -> [(State,(RewriteName,a))]

rewrite : UID -> ExprF (Rewrite Expr) -> Rewrite Expr
rewrite uid exprf state =
    if uid == state.selecteduid
        then rewriteSelected uid exprf state
        else rewriteStandard uid exprf state

rewriteSelected : UID -> ExprF (Rewrite Expr) -> Rewrite Expr
rewriteSelected uid exprf = bindRewrite (rewriteStandard uid exprf) (\expr ->
    manyRewrites (map (\r -> r expr) allRewrites))

rewriteStandard : UID -> ExprF (Rewrite Expr) -> Rewrite Expr
rewriteStandard uid exprf = case exprf of
    PlusF lhsRewrite rhsRewrite -> bindRewrite lhsRewrite (\lhs ->
        bindRewrite rhsRewrite (\rhs -> returnRewrite (plus uid lhs rhs)))
    TimesF lhsRewrite rhsRewrite -> bindRewrite lhsRewrite (\lhs ->
        bindRewrite rhsRewrite (\rhs -> returnRewrite (times uid lhs rhs)))
    ZeroF -> returnRewrite (zero uid)
    VariableF name -> returnRewrite (variable uid name)


singleRewrite : String -> a -> Rewrite a
singleRewrite n a s = [(s,([n],a))]

noRewrite : Rewrite a
noRewrite s = []

manyRewrites : [Rewrite a] -> Rewrite a
manyRewrites = foldr (\r1 r2 s -> r1 s ++ r2 s) noRewrite

newuid : Rewrite UID
newuid s = [({s | nextuid <- s.nextuid + 1},([],s.nextuid))]

returnRewrite : a -> Rewrite a
returnRewrite a s = [(s,([],a))]

mapRewrite : (a -> b) -> Rewrite a -> Rewrite b
mapRewrite f smna s = mapRewrite' f (smna s)

mapRewrite' : (a -> b) -> [(State,(RewriteName,a))] -> [(State,(RewriteName,b))]
mapRewrite' f lsna = case lsna of
    [] -> []
    (s',(n,a)) :: rest -> (s',(n,f a)) :: mapRewrite' f rest

bindRewrite : Rewrite a -> (a -> Rewrite b) -> Rewrite b
bindRewrite smna amsnb s = bindRewrite' (smna s) amsnb

bindRewrite' : [(State,(RewriteName,a))] -> (a -> Rewrite b) -> [(State,(RewriteName,b))]
bindRewrite' snas f = case snas of
    [] -> []
    ((s,(n,a)) :: rest) -> map (\(s',(n',b)) -> (s',(n++n',b))) (f a s) ++ bindRewrite' rest f

renderRewrites : State -> Rewrite Expr -> Element
renderRewrites state rs = flow down (map renderRewrite (rs state))

renderRewrite : (State,(RewriteName,Expr)) -> Element
renderRewrite (state,(name,expr)) = keyword (show name)
    |> clickable inputexpr.handle {state | expr <- expr}


commuteplus : Expr -> Rewrite Expr
commuteplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> singleRewrite "commute" (plus uid rhs lhs)
        _ -> noRewrite

assoclplus : Expr -> Rewrite Expr
assoclplus expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case rhs of
                Expr rhsuid rhsexprf -> case rhsexprf of
                    PlusF rlhs rrhs -> singleRewrite "assocl" (plus uid (plus rhsuid lhs rlhs) rrhs)
                    _ -> noRewrite
            _ -> noRewrite

assocrplus : Expr -> Rewrite Expr
assocrplus expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case lhs of
                Expr lhsuid lhsexprf -> case lhsexprf of
                    PlusF llhs lrhs -> singleRewrite "assocr" (plus uid llhs (plus lhsuid lrhs rhs))
                    _ -> noRewrite
            _ -> noRewrite

zerolplus : Expr -> Rewrite Expr
zerolplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr _ lhsexprf -> case lhsexprf of
                ZeroF -> case rhs of
                    Expr _ rhsexprf -> singleRewrite "zerol" (Expr uid rhsexprf)
                _ -> noRewrite
        _ -> noRewrite

zerorplus : Expr -> Rewrite Expr
zerorplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case rhs of
            Expr _ rhsexprf -> case rhsexprf of
                ZeroF -> case lhs of
                    Expr _ lhsexprf -> singleRewrite "zeror" (Expr uid lhsexprf)
                _ -> noRewrite
        _ -> noRewrite

addzerolplus : Expr -> Rewrite Expr
addzerolplus expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "addzerol" (plus uid (zero luid) (Expr ruid exprf))))

addzerorplus : Expr -> Rewrite Expr
addzerorplus expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "addzeror" (plus uid (Expr ruid exprf) (zero luid))))

allRewrites : [Expr -> Rewrite Expr]
allRewrites = [
    commuteplus,assoclplus,assocrplus,
    zerolplus,zerorplus,addzerolplus,addzerorplus]


main = lift (\state ->
    traverse render state.expr state `beside`
    renderRewrites state (traverse rewrite state.expr)) inputexpr.signal