import Graphics.Input (Input,input,clickable,hoverable)
import Text (leftAligned,toText)
import Text as Text
import Window as Window

type UID = Int
data ExprF a = PlusF a a | TimesF a a | NegateF a | ValueF Int | VariableF String
data Expr = Expr UID (ExprF Expr)
type State = {nextuid : UID,selecteduid : UID,expr : Expr,history : [Expr],goal : Expr}

traverse : (UID -> ExprF a -> a) -> Expr -> a
traverse f expr = case expr of
    Expr uid exprf ->
        f uid (
            case exprf of
                PlusF lhs rhs -> PlusF (traverse f lhs) (traverse f rhs)
                TimesF lhs rhs -> TimesF (traverse f lhs) (traverse f rhs)
                NegateF e -> NegateF (traverse f e)
                ValueF i -> ValueF i
                VariableF name -> VariableF name)

inputexpr : Input State
inputexpr = input {nextuid=8,selecteduid=1,expr=testexpr,history=[],goal=goalexpr}

hoverinput : Input UID
hoverinput = input (-1)

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

negate : UID -> Expr -> Expr
negate uid e = Expr uid (NegateF e)

variable : UID -> String ->Expr
variable uid name = Expr uid (VariableF name)

testexpr : Expr
testexpr = plus 0 (plus 1 (variable 2 "a") (variable 3 "a")) (times 4 (value 5 0) (negate 6 (variable 7 "b")))

goalexpr : Expr
goalexpr = times 0 (value 1 2) (variable 2 "a")

type Render = State -> Element

applyState : State -> ExprF Render -> ExprF Element
applyState state exprf =
    case exprf of
        PlusF lhs rhs -> PlusF (lhs state) (rhs state)
        TimesF lhs rhs -> TimesF (lhs state) (rhs state)
        NegateF e -> NegateF (e state)
        ValueF i -> ValueF i
        VariableF name -> VariableF name

colordirection = degrees 230
backgroundcolor = hsl colordirection 0.95 0.95
rewritecolor = hsl colordirection 0.7 0.6
hovercolor = hsl colordirection 0.8 0.8
selectedcolor = hsl colordirection 0.7 0.6
goalcolor = backgroundcolor


main = lift3 renderMain Window.dimensions inputexpr.signal hoverinput.signal

renderMain : (Int,Int) -> State -> UID -> Element
renderMain (w,h) state hovereduid = let
        renderedExpr = traverse (render hovereduid) state.expr state
        renderedHistory = renderHistory state.history
        renderedRewrites = renderRewrites state (traverse rewrite state.expr)
        renderedGoal = keyword 24 "Goal: " `beside` traverse (\_ -> renderExprF (keyword 24)) state.goal
            |> color goalcolor
            |> container 400 60 midLeft
        renderedRightSide = spacer 20 100 `beside` flow down [renderedGoal,renderedExpr,renderedHistory]
        outerContainer = renderedRewrites `beside` renderedRightSide
            |> container w h topLeft
            |> color backgroundcolor
    in
        outerContainer

render : UID -> UID -> ExprF Render -> Render
render hovereduid uid exprf state = exprf
    |> applyState state
    |> renderExprF (\s -> keyword 60 s
        |> clickable inputexpr.handle {state | selecteduid <- uid}
        |> hoverable hoverinput.handle (\b -> if b then uid else -1))
    |> (if uid == state.selecteduid then color selectedcolor else id)
    |> (if uid == hovereduid then color hovercolor else id)

renderHistory : [Expr] -> Element
renderHistory = flow down . map (
    container 800 40 midLeft . (traverse (\_ -> renderExprF (keyword 24))))

renderRewrites : State -> Rewrite Expr -> Element
renderRewrites state rs = flow down (map renderRewrite (rs state))

renderRewrite : (State,(RewriteName,Expr)) -> Element
renderRewrite (state,(name,expr)) = Text.leftAligned (Text.height 24 (toText (concat name)))
    |> container 290 70 middle
    |> color rewritecolor
    |> container 300 80 middle
    |> clickable inputexpr.handle {state | expr <- expr, history <- state.expr :: state.history}

renderExprF : (String -> Element) -> ExprF Element -> Element
renderExprF r exprf = case exprf of
    PlusF lhs rhs -> flow right [
        r "(",
        lhs,
        r "+",
        rhs,
        r ")"]
    TimesF lhs rhs -> flow right [
        r "(",
        lhs,
        r "*",
        rhs,
        r ")"]
    NegateF e -> flow right [
        r "(",
        r "-",
        e,
        r ")"]
    ValueF i -> r (show i)
    VariableF name -> r name

keyword : Float -> String -> Element
keyword h s = Text.leftAligned (Text.monospace (Text.height h (toText s)))


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
    NegateF eRewrite -> bindRewrite eRewrite (\e -> returnRewrite (negate uid e))
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
            NegateF e1 -> case exprf2 of
                NegateF e2 -> equalModuloUIDs e1 e2
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
            NegateF e -> bindRewrite (freshuids e) (\freshe ->
                returnRewrite (negate uid freshe))
            ValueF i ->
                returnRewrite (value uid i)
            VariableF name ->
                returnRewrite (variable uid name))

commuteplus : Expr -> Rewrite Expr
commuteplus expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> singleRewrite "commute plus\nx + y => y + x" (plus uid rhs lhs)
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
                else bindRewrite newuid (\euid ->
                    singleRewrite "split one left" (plus uid (one lhsuid) (negate euid (value rhsuid (1-i)))))))
        _ -> noRewrite

splitoneright : Expr -> Rewrite Expr
splitoneright expr = case expr of
    Expr uid exprf -> case exprf of
        ValueF i -> bindRewrite newuid (\lhsuid ->
            bindRewrite newuid (\rhsuid -> if i > 1
                then singleRewrite "split one right" (plus uid (value lhsuid (i-1)) (one rhsuid))
                else bindRewrite newuid (\euid ->
                    singleRewrite "split one right" (plus uid (negate euid (value lhsuid (1-i))) (one rhsuid)))))
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

cancelnegatedl : Expr -> Rewrite Expr
cancelnegatedl expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case lhs of
            Expr lhsuid lhsexprf -> case lhsexprf of
                NegateF e -> if equalModuloUIDs e rhs
                    then singleRewrite "cancel negated left" (zero uid)
                    else noRewrite
                _ -> noRewrite
        _ -> noRewrite

cancelnegatedr : Expr -> Rewrite Expr
cancelnegatedr expr = case expr of
    Expr uid exprf -> case exprf of
        PlusF lhs rhs -> case rhs of
            Expr rhsuid rhsexprf -> case rhsexprf of
                NegateF e -> if equalModuloUIDs lhs e
                    then singleRewrite "cancel negated right" (zero uid)
                    else noRewrite
                _ -> noRewrite
        _ -> noRewrite

allRewrites : [Expr -> Rewrite Expr]
allRewrites = [
    factoroutl,factoroutr,distributel,distributer,
    zerolplus,zerorplus,
    oneltimes,onertimes,
    cancelnegatedl,cancelnegatedr,
    cancelzerol,cancelzeror,
    splitoneleft,splitoneright,
    addvalues,multiplyvalues,
    commuteplus,assoclplus,assocrplus,
    commutetimes,assocltimes,assocrtimes,
    addzerolplus,addzerorplus,
    addoneltimes,addonertimes]

