import Graphics.Input (Input,input,clickable)
import Text (leftAligned,toText)
import Text as Text

type UID = Int
data ExprF a = PlusF a a | TimesF a a | ValueF Int | VariableF String
data Expr = Expr UID (ExprF Expr)
type State = {nextuid : UID,selecteduid : UID,expr : Expr,history : [Expr]}

traverse : (UID -> ExprF a -> a) -> Expr -> a
traverse f expr = case expr of
    Expr uid exprf ->
        f uid (
            case exprf of
                PlusF lhs rhs -> PlusF (traverse f lhs) (traverse f rhs)
                TimesF lhs rhs -> TimesF (traverse f lhs) (traverse f rhs)
                ValueF i -> ValueF i
                VariableF name -> VariableF name)

inputexpr : Input State
inputexpr = input {nextuid=7,selecteduid=1,expr=testexpr,history=[]}

plus : UID -> Expr -> Expr -> Expr
plus uid lhs rhs = Expr uid (PlusF lhs rhs)

times : UID -> Expr -> Expr -> Expr
times uid lhs rhs = Expr uid (TimesF lhs rhs)

value : UID -> Int -> Expr
value uid i = Expr uid (ValueF i)

zero : UID -> Expr
zero uid = value uid 0

one : UID -> Expr
one uid = value uid 1

variable : UID -> String ->Expr
variable uid name = Expr uid (VariableF name)

testexpr : Expr
testexpr = plus 0 (plus 1 (variable 2 "x") (variable 3 "x")) (times 4 (zero 5) (variable 6 "y"))

type Render = State -> Element

render : UID -> ExprF Render -> Render
render uid exprf state =
    (if uid == state.selecteduid then color lightYellow else id) (
        clickable inputexpr.handle {state | selecteduid <- uid} (
            renderExprF 60 (applyState state exprf)))

applyState : State -> ExprF Render -> ExprF Element
applyState state exprf =
    case exprf of
        PlusF lhs rhs -> PlusF (lhs state) (rhs state)
        TimesF lhs rhs -> TimesF (lhs state) (rhs state)
        ValueF i -> ValueF i
        VariableF name -> VariableF name

renderHistory : [Expr] -> Element
renderHistory = flow down . map (traverse (\_ -> renderExprF 30))

renderExprF : Float -> ExprF Element -> Element
renderExprF h exprf = case exprf of
    PlusF lhs rhs -> flow right [
        keyword h "(",
        lhs,
        keyword h "+",
        rhs,
        keyword h ")"]
    TimesF lhs rhs -> flow right [
        keyword h "(",
        lhs,
        keyword h "*",
        rhs,
        keyword h ")"]
    ValueF i -> keyword h (show i)
    VariableF name -> keyword h name

renderRewrites : State -> Rewrite Expr -> Element
renderRewrites state rs = flow down (map renderRewrite (rs state))

renderRewrite : (State,(RewriteName,Expr)) -> Element
renderRewrite (state,(name,expr)) = keyword 40 (concat name)
    |> container 390 90 middle
    |> color lightBlue
    |> container 400 100 middle
    |> clickable inputexpr.handle {state | expr <- expr, history <- state.expr :: state.history}

keyword : Float -> String -> Element
keyword h s = Text.leftAligned (Text.height h (toText s))


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
    ValueF i -> returnRewrite (value uid i)
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


equalModuloUIDs : Expr -> Expr -> Bool
equalModuloUIDs expr1 expr2 = case expr1 of
    Expr _ exprf1 -> case expr2 of
        Expr _ exprf2 -> case exprf1 of
            PlusF lhs1 rhs1 -> case exprf2 of
                PlusF lhs2 rhs2 -> equalModuloUIDs lhs1 lhs2 && equalModuloUIDs rhs1 rhs2
                _ -> False
            TimesF lhs1 rhs1 -> case exprf2 of
                TimesF lhs2 rhs2 -> equalModuloUIDs lhs1 lhs2 && equalModuloUIDs rhs1 rhs2
                _ -> False
            ValueF i1 -> case exprf2 of
                ValueF i2 -> i1 == i2
                _ -> False
            VariableF name1 -> case exprf2 of
                VariableF name2 -> name1 == name2
                _ -> False


freshuids : Expr -> Rewrite Expr
freshuids expr = case expr of
    Expr _ exprf -> bindRewrite newuid (\uid ->
        case exprf of
            PlusF lhs rhs -> bindRewrite (freshuids lhs) (\freshlhs ->
                bindRewrite (freshuids rhs) (\freshrhs ->
                    returnRewrite (plus uid freshlhs freshrhs)))
            TimesF lhs rhs -> bindRewrite (freshuids lhs) (\freshlhs ->
                bindRewrite (freshuids rhs) (\freshrhs ->
                    returnRewrite (times uid freshlhs freshrhs)))
            ValueF i ->
                returnRewrite (value uid i)
            VariableF name ->
                returnRewrite (variable uid name))

commuteplus : Expr -> Rewrite Expr
commuteplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> singleRewrite "commute plus" (plus uid rhs lhs)
        _ -> noRewrite

assoclplus : Expr -> Rewrite Expr
assoclplus expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case rhs of
                Expr rhsuid rhsexprf -> case rhsexprf of
                    PlusF rlhs rrhs -> singleRewrite "associate plus left" (plus uid (plus rhsuid lhs rlhs) rrhs)
                    _ -> noRewrite
            _ -> noRewrite

assocrplus : Expr -> Rewrite Expr
assocrplus expr = case expr of
    Expr uid exprf ->
        case exprf of
            PlusF lhs rhs -> case lhs of
                Expr lhsuid lhsexprf -> case lhsexprf of
                    PlusF llhs lrhs -> singleRewrite "associate plus right" (plus uid llhs (plus lhsuid lrhs rhs))
                    _ -> noRewrite
            _ -> noRewrite

zerolplus : Expr -> Rewrite Expr
zerolplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr _ lhsexprf -> case lhsexprf of
                ValueF 0 -> case rhs of
                    Expr _ rhsexprf -> singleRewrite "remove zero left" (Expr uid rhsexprf)
                _ -> noRewrite
        _ -> noRewrite

zerorplus : Expr -> Rewrite Expr
zerorplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case rhs of
            Expr _ rhsexprf -> case rhsexprf of
                ValueF 0 -> case lhs of
                    Expr _ lhsexprf -> singleRewrite "remove zero right" (Expr uid lhsexprf)
                _ -> noRewrite
        _ -> noRewrite

addzerolplus : Expr -> Rewrite Expr
addzerolplus expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "plus zero left" (plus uid (zero luid) (Expr ruid exprf))))

addzerorplus : Expr -> Rewrite Expr
addzerorplus expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "plus zero right" (plus uid (Expr luid exprf) (zero ruid))))

commutetimes : Expr -> Rewrite Expr
commutetimes expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> singleRewrite "commute times" (times uid rhs lhs)
        _ -> noRewrite

assocltimes : Expr -> Rewrite Expr
assocltimes expr = case expr of
    Expr uid exprf ->
        case exprf of
            TimesF lhs rhs -> case rhs of
                Expr rhsuid rhsexprf -> case rhsexprf of
                    TimesF rlhs rrhs -> singleRewrite "associate times left" (times uid (times rhsuid lhs rlhs) rrhs)
                    _ -> noRewrite
            _ -> noRewrite

assocrtimes : Expr -> Rewrite Expr
assocrtimes expr = case expr of
    Expr uid exprf ->
        case exprf of
            TimesF lhs rhs -> case lhs of
                Expr lhsuid lhsexprf -> case lhsexprf of
                    TimesF llhs lrhs -> singleRewrite "associate times right" (times uid llhs (times lhsuid lrhs rhs))
                    _ -> noRewrite
            _ -> noRewrite

oneltimes : Expr -> Rewrite Expr
oneltimes expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case lhs of
            Expr _ lhsexprf -> case lhsexprf of
                ValueF 1 -> case rhs of
                    Expr _ rhsexprf -> singleRewrite "remove one left" (Expr uid rhsexprf)
                _ -> noRewrite
        _ -> noRewrite

onertimes : Expr -> Rewrite Expr
onertimes expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case rhs of
            Expr _ rhsexprf -> case rhsexprf of
                ValueF 1 -> case lhs of
                    Expr _ lhsexprf -> singleRewrite "remove one right" (Expr uid lhsexprf)
                _ -> noRewrite
        _ -> noRewrite

addoneltimes : Expr -> Rewrite Expr
addoneltimes expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "times one left" (times uid (one luid) (Expr ruid exprf))))

addonertimes : Expr -> Rewrite Expr
addonertimes expr = case expr of
    Expr uid exprf ->
        bindRewrite newuid (\luid ->
            bindRewrite newuid (\ruid ->
                singleRewrite "times one right" (times uid (Expr luid exprf) (one ruid))))

distributel : Expr -> Rewrite Expr
distributel expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case rhs of
            Expr rhsuid rhsexprf -> case rhsexprf of
                PlusF rlhs rrhs -> bindRewrite newuid (\lhsuid ->
                    bindRewrite (freshuids lhs) (\freshlhs ->
                        singleRewrite "distribute left" (plus uid (times lhsuid freshlhs rlhs) (times rhsuid lhs rrhs))))
                _ -> noRewrite
        _ -> noRewrite

distributer : Expr -> Rewrite Expr
distributer expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                PlusF llhs lrhs -> bindRewrite newuid (\rhsuid ->
                    bindRewrite (freshuids rhs) (\freshrhs ->
                        singleRewrite "distribute right" (plus uid (times lhsuid llhs freshrhs) (times rhsuid lrhs rhs))))
                _ -> noRewrite
        _ -> noRewrite

factoroutl : Expr -> Rewrite Expr
factoroutl expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                TimesF llhs lrhs -> case rhs of
                    Expr rhsuid rhsexprf -> case rhsexprf of
                        TimesF rlhs rrhs -> if equalModuloUIDs llhs rlhs
                            then singleRewrite "factor out left" (times uid llhs (plus lhsuid lrhs rrhs))
                            else noRewrite
                        _ -> noRewrite
                _ -> noRewrite
        _ -> noRewrite

factoroutr : Expr -> Rewrite Expr
factoroutr expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                TimesF llhs lrhs -> case rhs of
                    Expr rhsuid rhsexprf -> case rhsexprf of
                        TimesF rlhs rrhs -> if equalModuloUIDs lrhs rrhs
                            then singleRewrite "factor out right" (times uid (plus lhsuid llhs rlhs) lrhs)
                            else noRewrite
                        _ -> noRewrite
                _ -> noRewrite
        _ -> noRewrite

splitoneleft : Expr -> Rewrite Expr
splitoneleft expr = case expr of
    Expr uid exprf -> case exprf of
        ValueF i -> bindRewrite newuid (\lhsuid ->
            bindRewrite newuid (\rhsuid -> if i > 1
                then singleRewrite "split one left" (plus uid (one lhsuid) (value rhsuid (i-1)))
                else noRewrite))
        _ -> noRewrite

splitoneright : Expr -> Rewrite Expr
splitoneright expr = case expr of
    Expr uid exprf -> case exprf of
        ValueF i -> bindRewrite newuid (\lhsuid ->
            bindRewrite newuid (\rhsuid -> if i > 1
                then singleRewrite "split one right" (plus uid (value lhsuid (i-1)) (one rhsuid))
                else noRewrite))
        _ -> noRewrite

addvalues : Expr -> Rewrite Expr
addvalues expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                ValueF lhsi -> case rhs of
                    Expr rhsuid rhsexprf -> case rhsexprf of
                        ValueF rhsi -> singleRewrite "add values" (value uid (lhsi + rhsi))
                        _ -> noRewrite
                _ -> noRewrite
        _ -> noRewrite

multiplyvalues : Expr -> Rewrite Expr
multiplyvalues expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                ValueF lhsi -> case rhs of
                    Expr rhsuid rhsexprf -> case rhsexprf of
                        ValueF rhsi -> if lhsi > 1 && rhsi > 1
                            then singleRewrite "multiply values" (value uid (lhsi * rhsi))
                            else noRewrite
                        _ -> noRewrite
                _ -> noRewrite
        _ -> noRewrite

cancelzerol : Expr -> Rewrite Expr
cancelzerol expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case lhs of
            Expr _ lhsexprf -> case lhsexprf of
                ValueF 0 -> singleRewrite "cancel zero left" (zero uid)
                _ -> noRewrite
        _ -> noRewrite

cancelzeror : Expr -> Rewrite Expr
cancelzeror expr = case expr of
    Expr uid exprf -> case exprf of
        TimesF lhs rhs -> case rhs of
            Expr _ rhsexprf -> case rhsexprf of
                ValueF 0 -> singleRewrite "cancel zero right" (zero uid)
                _ -> noRewrite
        _ -> noRewrite

allRewrites : [Expr -> Rewrite Expr]
allRewrites = [
    factoroutl,factoroutr,distributel,distributer,
    zerolplus,zerorplus,
    oneltimes,onertimes,
    cancelzerol,cancelzeror,
    splitoneleft,splitoneright,
    addvalues,multiplyvalues,
    commuteplus,assoclplus,assocrplus,
    commutetimes,assocltimes,assocrtimes,
    addzerolplus,addzerorplus,
    addoneltimes,addonertimes]


main = lift (\state -> let
        renderedExpr = traverse render state.expr state
        renderedHistory = renderHistory state.history
        renderedRewrites = renderRewrites state (traverse rewrite state.expr)
    in
        renderedRewrites `beside` (renderedExpr `above` renderedHistory)) inputexpr.signal