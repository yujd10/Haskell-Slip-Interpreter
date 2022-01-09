-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-

---------------------- +
-- Date: Oct 25 2021   +
-- Author : Jiadi Yu   +
--          20189854   +
---------------------- +

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexi-
                                     -- cale).
import Data.Char        -- Conversion de Chars de/vers Int
--import Numeric        -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
--import Data.Maybe     -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                     -- La liste vide
          | Scons Sexp Sexp          -- Une paire
          | Ssym String              -- Un symbole
          | Snum Int                 -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent à la fin de
-- la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }

-- N'importe quelle combinaison d'espaces et de commentaires est considérée co-
-- mme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
            return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes de
-- ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)

data Lexp = Lnum Int               -- Constante entière.
          | Lvar Var               -- Référence à une variable.
          | Lfn Var Lexp           -- Fonction anonyme prenant un argument.
          | Lpipe Lexp Lexp        -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]       -- Constructeur de structure de donnée.
          | Lcase Lexp [(Pat, Lexp)]       -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n -- Un entier
s2l (Ssym s) = Lvar s -- Un symbole
s2l (Scons x Snil) = s2l x
s2l (Scons e1 e2) =
    case e1 of
        -- Case (façon pas smart car on a pas bien compris la structure de Case )
        -- J'ai seulement fait pour les deux cas dans les exemples
        (Ssym "case") ->
            case e2 of
            (Scons cons (Scons(Scons (Scons (Ssym pat1) Snil) exp1)(Scons (Scons (Scons (Ssym pat2) (Scons (Ssym exp3) (Scons (Ssym exp4) Snil))) exp2) Snil)))->Lcase (s2l cons) [(Just(pat1,[]),s2l exp1),(Just (pat2,[exp3,exp4]),s2l exp2)]
            (Scons cons (Scons(Scons (Ssym "_") exp1) (Scons (Scons (Scons (Ssym pat2) (Scons (Ssym exp3) (Scons (Ssym exp4) Snil))) exp2) Snil))) -> Lcase (s2l cons) [(Nothing,s2l exp1),(Just (pat2,[exp3,exp4]),s2l exp2)]
        -- Constructeur
        (Ssym "cons") ->
               case e2 of
                (Scons (Ssym "nil") _) -> Lcons "nil" []
                (Scons (Ssym x) y) -> Lcons x (map s2l (listMaker y))
                _-> error "non valide syntax"
        -- IF
        ((Ssym "if")) ->
            case e2 of
                (Scons x (Scons y z)) -> Lcase (s2l x) [(Just( "true", []), s2l y),(Just( "false", []), s2l z)]
                _-> error "non valide syntax"
        -- Fonction anonyme
        (Ssym "lambda") ->
            case e2 of
                (Scons x y) -> list2lambda (listMaker x) y
                _-> error "non valide syntax"
        -- Let statique
        (Ssym "slet")->
            case e2 of
                (Scons (Scons (Scons (Ssym x) y) Snil) z) -> Llet Lexical x (s2l y) (s2l z)
                (Scons (Scons (Scons (Ssym x) y) z) m) -> Llet Lexical x (s2l y) (s2l (Scons (Ssym "slet") (Scons z m)))
                (Scons (Scons (Scons (Scons (Ssym x) y) z) n) m) ->
                    case n of
                        Snil-> Llet Lexical x (list2lambda (listMaker y) z) (s2l m)
                        _-> Llet Lexical x (list2lambda (listMaker y) z) (s2l (Scons (Ssym "slet") (Scons n m)))
                _->error "non valide syntax"
        -- Let dynamique
        (Ssym "dlet")->
            case e2 of
                (Scons(Scons(Scons (Ssym x) y) Snil) z) -> Llet Dynamic x (s2l y) (s2l z)
                (Scons (Scons (Scons (Ssym x) y) z) m) -> Llet Dynamic x (s2l y) (s2l (Scons (Ssym "dlet") (Scons z m)))
                (Scons (Scons (Scons (Scons (Ssym x) y) z) n) m) ->
                    case n of
                        Snil-> Llet Dynamic x (list2lambda (listMaker y) z) (s2l m)
                        _-> Llet Dynamic x (list2lambda (listMaker y) z) (s2l (Scons (Ssym "dlet") (Scons n m)))
                _ -> error "non valide syntax"
        _-> Lpipe (s2l e1)(s2l e2)

--Mettre les sexp dans une liste
listMaker :: Sexp -> [Sexp]
listMaker e =
    case e of
        Scons m n -> m : listMaker n
        Snil -> []

-- Traiter les element dans la liste pour avoir
-- un syntax pour les fonctions anonyme
list2lambda :: [Sexp] -> Sexp -> Lexp
list2lambda e1 e2 =
    case e1 of
        (Ssym n):m -> Lfn n (list2lambda m e2)
        _ -> s2l e2

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfn (Env -> Value -> Value)
        --    | Vlambda Var Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfn _)
        = showString "<function>"

type Env = [(Var, Value)]


-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> Vnum (x `op` y))))
           mkcmp (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> if x `op` y then true else false)))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n

-- On recherche la variable dans la partie statique (lexical) :
eval ((var, val):restEnvS) envD (Lvar varLook)
    | var == varLook = val
    | otherwise = eval restEnvS envD (Lvar varLook)

-- On recherche la variable dans la partie dynamique:
eval envS ((var, val):restEnvD) (Lvar varLook)
    | var == varLook = val
    | otherwise = eval envS restEnvD (Lvar varLook)

-- Pour un appel d'un constructeur de structure de donnée :
eval _senv _denv (Lcons pat exps) = Vcons pat (map (eval env0 []) exps)

-- Pour une expression conditonnelle (IF):
eval _senv _denv (Lcase cond [(Just ("true",[]),exp1),(Just ("false",[]),exp2)]) =
    case eval _senv _denv cond of
        Vcons "false" [] -> eval _senv _denv exp2
        Vcons "true" [] -> eval _senv _denv exp1
        _ -> error "Invalide."
-- Alternative :
-- eval _senv _denv (Lcase cond (((tag1, list1), body):rest)) =
--  case (eval _senv _denv cond) of
--    _ -> error "Cas non existant"
--    Vcons (tag2 list2) ->  if (tag1 == tag2 || tag1 == "_" ) 
--              then eval _senv ((var list1 list2) ++ _denv) body
--          else eval _senv _denv (Lcase cond rest)

-- Pour le fait que ça soit static ou dynamic :
eval _senv _denv (Llet typ var exp1 exp2)=
    case typ of
        Lexical -> eval ((var,eval _senv _denv exp1):_senv) _denv exp2
        Dynamic -> eval _senv ((var,eval _senv _denv exp1):_denv) exp2
        _ -> error "Pas de type à mettre dans l'envrionnement"

-- Pour appel de fcontion anonyme :
-- Lambda fonction pour recevoir l'environnement et une value de Lpipe x y
eval _senv _denv (Lfn var body) = Vfn (\envD -> (\valuE -> eval ((var,valuE):_senv) ((var,valuE):envD) body))

-- Pour appel de fcontion Lpipe (on évalue) :
eval _senv _denv (Lpipe x y) =
    case eval _senv _denv y of
    -- Pouvoir realiser (\envD -> (\valuE ->eval ((var,(valuE)):_senv) ((var,valuE):envD) body)) _denv (eval _senv _denv x)
        (Vfn f) -> f _denv (eval _senv _denv x)
        Vnum n -> error (show n <> "n'est pas une fonction")

eval _ _ e = error ("Can't eval: " ++ show e)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
