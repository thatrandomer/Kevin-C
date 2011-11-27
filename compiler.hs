import Data.Char
import Numeric
import System

data Token = 
       Num Int
      |Identifier [Char]
      |StringVal String
      |Comment String
      |Math Op
      |Types Type
      |Cmp CmpOp
      |CFun ConFun
      |SChars SChar
      deriving (Show,Eq)

--Comparing operators It may seem counter ituitive that I use the opposite value for show but it's because we only want to branch away if it doesn't equal it
data CmpOp = 
          Greater
         |LessThan
         |Equal
         deriving (Eq,Enum)
instance Show CmpOp where
    show Greater = "LE"
    show LessThan = "GE"
    show Equal = "NE"
instance Read CmpOp where
    readsPrec _ value = 
           tryParse value cmpList
cmpList = [(">", Greater), ("<", LessThan), ("==", Equal)]


--maths stuff
data Op = Plus | Minus | Mult | ReverseSub
  deriving (Eq,Enum)
instance Show Op where
    show Plus       = "ADD"
    show Minus      = "SUB"
    show ReverseSub = "RSB"
    show Mult       = "MUl"

instance Read Op where
	readsPrec _ value = 
           tryParse value opList

opList = [("+", Plus), ("-", Minus), ("*", Mult)]

--control features
data ConFun = If | For | Else | While | ElseIf | Return | Break
  deriving (Eq,Enum)
instance Show ConFun where
    show If     = "if"
    show For    = "for"
    show Else   = "else"
    show While  = "while"
    show ElseIf = "elseif"
    show Return = "return"
    show Break  = "break"
instance Read ConFun where
    readsPrec _ value = 
        tryParse value [(show x, x) |x <- enumFrom If]



data Type = INT | CHARPTR | CHAR 
  deriving (Eq,Enum)
instance Show Type where
    show INT        = "int"
    show CHARPTR    = "char*"
    show CHAR       = "char"
    
instance Read Type where
    readsPrec _ value = 
        tryParse value [(show x, x) |x <- enumFrom INT]
isPointer::Type->Bool
isPointer x = (x == CHARPTR)

--special charaters
data SChar = LeftParn | RightParn | SemiCol | Col | Assignment | LeftBracket | RightBracket | LeftSqrBracket | RightSqrBracket | Comma
  deriving (Eq,Enum)
instance Show SChar where
    show LeftParn         = "("
    show RightParn        = ")"
    show SemiCol          = ";"
    show Col              = ":"
    show Comma            = ","
    show Assignment       = "="
    show LeftBracket      = "{"
    show RightBracket     = "}"
    show LeftSqrBracket   = "["
    show RightSqrBracket  = "]"
instance Read SChar where
    readsPrec _ value = 
        tryParse value [(show x, x) |x <- enumFrom LeftParn]



--main::String->IO()
main = 
	do file <- getArgs
	   a <- readFile (file!!0)
	   putStr  (parseFunctions (tokenise a) "" [])

--This does the work of changing the source code into tokens
tokenise::String->[Token]
tokenise x = lexer (x)


-- better lexer
lexer::[Char]->[Token]
lexer [] = []
lexer (xs)           | null word = []
	             | (xs!!0 == '0') && (xs!!1 == 'x') = [Num hex] ++ lexer str
                       -- char* gets parsed into "char" "*" with lex so you have to do this fuckery
                     | (word  ++ (take 1 rest)) == "char*" = [Types CHARPTR] ++ (lexer (drop 1 rest))
                     | (word!!0) == '"' = [StringVal word] ++ (lexer rest)

                     | word == "/*" = [Comment (unwords comments)] ++ (lexer (unwords (drop 1 afterComments)))
                     | (word!!0 == '\'') && (isAscii (word!!1)) && (word!!2 == '\'') = [Num (ord (word!!1))] ++ (lexer rest)  
                     | all isDigit word = [Num (read word::Int)] ++ (lexer (rest)) 
	                 | tryParse word cmpList /= []
                                     = [Cmp (read word::CmpOp)] ++ (lexer (rest))
                     | tryParse word opList /= []
                                     = [Math (read word::Op)] ++ (lexer (rest))
                     | word `elem` [show x |x <- enumFrom If]
                                     = [CFun (read word::ConFun)] ++ (lexer (rest))
		             | word `elem` [show x |x <- enumFrom INT]
                                     = [Types (read word::Type)] ++ (lexer (rest))

                                      
		             | (isAlpha (word!!0)) && (all (\x -> isAlpha x || isDigit x) word)
                                     =  [Identifier word] ++ (lexer (rest))
                     | word `elem` [show x |x <- enumFrom LeftParn]
                                     = [SChars (read word::SChar)] ++ (lexer (rest))
                     | otherwise = error ("Are you fucking serious -- " ++ word)

                     where [(word , rest)] = lex (xs)  
                           [(hex,str)] = readHex (drop 2 xs)    
                           (comments,afterComments) = break (== "*/") (words rest)

doParse::[Token]->Int
doParse(Num x:Math op:Num y:xs) = doOp op x y

--thats used when two numbers are to be operatered on
doOp::Op->Int->Int->Int
doOp Plus x y = x + y
doOp Minus x y = x - y
doOp Mult x y = x * y





type Reg = (Int,Type,[Char]) 
				--return,break
type ExitVal = (String,String)
              --id,data
type Data = ([Char],[Char])

parseFunctions::[Token]->[Char]->[Data]->[Char]
parseFunctions  [] code d = code ++ "\n\n" ++(printData d)
parseFunctions  (Comment x:xs) code d = parseFunctions xs (code ++ "\n\n;;;;;;;;;" ++ x) d
parseFunctions  toks code d =   parseFunctions toks' code' d'
		   	where (toks',_,code',d') = parseFunction toks code d



-- PLease Fix this awful awfully hacky function
parseFunction::[Token]->[Char]->[Data]->([Token],[[Reg]],[Char],[Data])
--main is very special
parseFunction (Identifier "main":SChars LeftParn:SChars RightParn:SChars LeftBracket:xs) c d =  
            (toks', regs',code' ++  "stop B stop ; program stops forever\n" ++ c,d')
            where  
                (toks',regs',code',d') = parseStmtList (xs) [[]] ( "\n\n") d ("main","") 


parseFunction (Identifier name:SChars LeftParn:xs) c d = 
            (toks', regs', c ++ code' ++ "end" ++ name ++ "\n\tLDMFD lr!,{r4-r11,sp} ; optimise me\n",d')
            where  
                (toks',regs',code',d') = parseStmtList (toks) [regs] ( "\n\n" ++ name ++ "\n\tSTMFD    sp!, {r4-r11,lr} ;optimise me\n" ++ code ++ "\n") d ("end" ++ name,"") 
                (regs,code) = foldl1 (\(regs1,code1) (regs2,code2) -> ((regs2 ++ regs1),(code1 ++ code2))) regss
                (toks,regss) = foldl1 (\(regs1,code1) (regs2,code2) -> ((regs2),(code1 ++ code2))) (parsefuctionRegs' xs 0)
                parsefuctionRegs' (SChars RightParn:SChars LeftBracket:xs) _ = [(xs,[([(3,INT,"neverUse")],"")])] -- this is a hack
                parsefuctionRegs' (Types regType1:Identifier name1:SChars RightParn:SChars LeftBracket:xs) val  = [(xs,[enterReg val regType1 name1])] 
                parsefuctionRegs' (Types regType1:Identifier name1:SChars Comma:xs) val = 
                                                        if val < 3
                                                          then [(xs,[(enterReg val regType1 name1)])] ++  parsefuctionRegs' xs (val+1)  
                                                          else error("more than 4 function parameters thats kind of bad, make a nicer function")       
                enterReg id regtype name4  = ([((id + 4),regtype,name4)],"\tMOV R" ++ (show (id + 4)) ++ ",R" ++ (show id) ++"\n")
                  



parseStmtList::[Token]->[[Reg]]->[Char]->[Data]->ExitVal->([Token],[[Reg]],[Char],[Data])
parseStmtList (SChars RightBracket:xs) b c d e = (xs, b, c,d)
parseStmtList a b c d e = parseStmtList toks regs chars d' e
			where (toks,regs,chars,d') = parseStmt a b c d e



parseStmt::[Token]->[[Reg]]->[Char]->[Data]->ExitVal->([Token],[[Reg]],[Char],[Data])
--end case not sure if this is needed best keep it for now
parseStmt (SChars RightBracket:xs) b c d e = ((SChars RightBracket:xs),b,c,d)
--ignore these
parseStmt (Comment x:xs) b c d e = parseStmt (xs) b (c ++ ";;;" ++ x  ++ "\n") d e
parseStmt (SChars SemiCol:xs) b c d e = parseStmt (xs) b c d e

--assignment functions
parseStmt (Types x:xs) b c d e =  (toks,regs,code,d)
                          where (toks,regs,code)  = parseAssignReg (Types x:xs) b c
parseStmt (Identifier x:SChars LeftSqrBracket:xs) b c d e = parseAssignExpr (Identifier x:SChars LeftSqrBracket:xs) b c d
parseStmt (Identifier x:SChars Assignment:xs) b c d e = parseAssign (Identifier x:SChars Assignment:xs) b c d


--just does the function
parseStmt (Identifier x:SChars LeftParn:xs) b c d e =  (toks,regs,code,d)
                        where (toks,regs,code) = parseDoFunction (Identifier x:SChars LeftParn:xs) b c

--control funtions
parseStmt (CFun If:xs) b c d e = parseIf (xs) b c d e
parseStmt (CFun While:xs) b c d e = parseWhile (xs) b c d e
--break and return functions
parseStmt (CFun Return:SChars SemiCol:xs) b c d e =  (toks,regs,code,d)
                        where (toks,regs,code) =parseReturn (xs) b c e
parseStmt (CFun Return:x:SChars SemiCol:xs) b c d e =  (toks,regs,code,d)
                        where (toks,regs,code) = parseReturn (x:SChars SemiCol:xs) b c e
parseStmt (CFun Return:Math minus:Num x:SChars SemiCol:xs) b c d e =  (toks,regs,code,d)
                        where (toks,regs,code) = parseReturn (Num (-x):SChars SemiCol:xs) b c e
parseStmt (CFun Break:SChars SemiCol:xs) b c d e =  (toks,regs,code,d)
                        where (toks,regs,code) = parseBreak (xs) b c e


--these are for calling a function
parseDoFunction::[Token]->[[Reg]]->[Char]->([Token],[[Reg]],[Char])
parseDoFunction (Identifier x:SChars LeftParn:SChars RightParn:xs) b c = (xs,b,c ++ "\tBL " ++ x ++ "\n") 
parseDoFunction (Identifier x:SChars LeftParn:xs) b c = (toks,b,c ++ code ++  "\tBL " ++ x ++ "\n")
                 where (toks,code) = parseDoFuntion' xs b "" 0
                       parseDoFuntion' (Identifier name:SChars RightParn:SChars SemiCol:xs) b c arg = (xs,(c ++ "\tMOV R" ++(show arg) ++ ",R" ++ (show (getRegVal b name)) ++ "\n") )
                       parseDoFuntion' (Num num:SChars RightParn:SChars SemiCol:xs) b c arg = (xs,(c ++ "\tMOV R" ++(show arg) ++ ",#" ++ (show  num) ++ "\n") )
                       parseDoFuntion' (Math minus:Num num:xs) b c arg = parseDoFuntion' (Num (-num):xs) b c arg
                       
                       parseDoFuntion' (Num num:SChars Comma:xs) b c arg =  if arg < 3
                                                    then parseDoFuntion' xs b (c ++ "\tMOV R" ++ (show arg) ++ ",#" ++ (show num) ++ "\n") (arg +1)
                                                    else error("woah your gone a bit mad with the function paraters: " ++ x)

                       parseDoFuntion' (Identifier name:SChars Comma:xs) b c arg =  if arg < 3
                                                    then parseDoFuntion' xs b (c ++ "\tMOV R" ++ (show arg) ++ ",R" ++ (show (getRegVal b name)) ++ "\n") (arg +1)
                                                    else error("woah your gone a bit mad with the function paraters: " ++ x)


--this is for unknown assignments
parseAssign::[Token]->[[Reg]]->[Char]->[Data]->([Token],[[Reg]],[Char],[Data])
parseAssign (Identifier x:SChars Assignment:Identifier fun:SChars LeftParn:xs) b c d = 
                                                        (toks,b,c ++ code ++ "\tMOV R" ++ (show regVal) ++",R0\n",d)
                                                    where
                                                        (toks,_,code) = parseDoFunction (Identifier fun:SChars LeftParn:xs) b ""
                                                        regVal = getRegVal b x
parseAssign (Identifier x:SChars Assignment:xs) b c d
                                                     | isPointer regT = parseArrayExpr xs b c d (regVal,regT,regN) 
                                                     | otherwise = (toks,regs,code,d) 
                                                     where (regVal,regT,regN) = head [ (regInt,regType,regName) | (regInt,regType,regName) <- (concat b), regName == x]		
                                                           (toks,regs,code) = parseExpr xs b c (regVal,regT,regN)


--this is for poiter assignments
parseAssignExpr::[Token]->[[Reg]]->[Char]->[Data]->([Token],[[Reg]],[Char],[Data])
parseAssignExpr (Identifier x:SChars LeftSqrBracket:Identifier ind:SChars RightSqrBracket:SChars Assignment:Identifier ws:SChars SemiCol:xs) b c d
                                                     | isPointer regT  = (xs ,b,c ++ "\tSTRB R" ++ (show value) ++ ", [R" ++  (show regVal) ++ ", R" ++ (show indexReg)  ++"]\n",d)
                                                     | otherwise =  error("you cant do shit like that with a char or int :" ++ x)
                                                     where (regVal,regT,regN) = head [ (regInt,regType,regName) | (regInt,regType,regName) <- (concat b), regName == x]			
                                                           indexReg = getRegVal b ind	
                                                           value =  getRegVal b ws
parseAssignExpr (Identifier x:SChars LeftSqrBracket:Num ind:SChars RightSqrBracket:SChars Assignment:Identifier ws:SChars SemiCol:xs) b c d
                                                     | isPointer regT = 
                                                           (xs ,b,c ++   "\tSTRB R" ++ (show val) ++ ", [R" ++  (show regVal) ++ ", #" ++ (show ind)  ++"]\n",d)
                                                     | otherwise =  error("you cant do shit like that with a char or int :" ++ x)
                                                     where (regVal,regT,regN) = head [ (regInt,regType,regName) | (regInt,regType,regName) <- (concat b), regName == x]			
                                                           --(tmp,tmpCode) = putValInTemp b ind 
                                                           val =  getRegVal b ws

parseAssignExpr (Identifier x:SChars LeftSqrBracket:Identifier ind:SChars RightSqrBracket:SChars Assignment:Num val:SChars SemiCol:xs) b c d
                                                    | isPointer regT = (xs ,b,c ++ tmpCode ++ "\tSTRB R" ++ (show tmp) ++ ", [R" ++  (show regVal) ++ ", R" ++ (show indexReg)  ++"\n",d)
                                                    | otherwise =  error("you cant do shit like that with a char or int :" ++ x)
                                                    where (regVal,regT,regN) = head [ (regInt,regType,regName) | (regInt,regType,regName) <- (concat b), regName == x]			
                                                          indexReg = getRegVal b ind
                                                          (tmp,tmpCode) = putValInTemp b val 	
                                                          
parseAssignExpr (Identifier x:SChars LeftSqrBracket:Num ind:SChars RightSqrBracket:SChars Assignment:Num val:SChars SemiCol:xs) b c d
                                                     | isPointer regT  = 
                                                           (xs ,b,c ++  tmpCode ++ "\tSTRB R" ++ (show tmp) ++ ", [R" ++  (show regVal) ++ ", #" ++ (show ind)  ++"]\n",d)
                                                     | otherwise =  error("you cant do shit like that with a char or int :" ++ x)
                                                     where (regVal,regT,regN) = head [ (regInt,regType,regName) | (regInt,regType,regName) <- (concat b), regName == x]			
                                                           (tmp,tmpCode) = putValInTemp b val 

                                      



--this is for assignment to a pointer 
parseArrayExpr::[Token]->[[Reg]]->[Char]->[Data]->Reg->([Token],[[Reg]],[Char],[Data])
parseArrayExpr (Identifier var:SChars LeftSqrBracket:Identifier ind:SChars RightSqrBracket:SChars SemiCol:xs) b c d (resultReg,_,_) =
                    if isPointer regLType 
				         then (xs ,b,c ++ "\tADD R" ++ (show resultReg)++ ", R" ++ (show regLVal)  ++ " ,R" ++ (show index) ++"\n",d)
					 else  error (var ++ " is not a char pointer") 
					where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == var]			
						index = getRegVal b ind
parseArrayExpr (Identifier var:SChars LeftSqrBracket:Num ind:SChars RightSqrBracket:SChars SemiCol:xs) b c d (resultReg,_,_) =
                    if isPointer regLType 
				         then (xs ,b,c ++ "\tADD R" ++ (show resultReg)++ ", R" ++ (show regLVal)  ++ " ,#" ++ (show ind) ++"\n",d)
					 else  error (var ++ " is not a char pointer") 
					where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == var]			
						
parseArrayExpr (Identifier var:SChars SemiCol:xs) b c d (resultReg,_,_) =
                    if isPointer regLType
				         then (xs ,b,c ++ "\tMOV R" ++ (show resultReg) ++ ", R" ++  (show regLVal) ++ " " ++"\n",d)
					 else  (xs ,b,c ++ "\tSTRB R" ++ (show regLVal) ++ ", [R" ++  (show resultReg)  ++"]\n",d)
					where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == var]	

parseArrayExpr (Num var:SChars SemiCol:xs) b c d (resultReg,_,_) =
					 (xs ,b,c ++ code ++ "\tSTRB R" ++ (show regId) ++ ", [R" ++  (show resultReg)  ++"]\n",d)
					where   	
                            (regId,code) = putValInTemp b var
parseArrayExpr (Identifier var:SChars Col:StringVal x:SChars SemiCol:xs) b c d (resultReg,_,_) =
            ( xs ,b,c ++ "\tMOV R" ++ (show resultReg) ++ ",=" ++ var  ++"\n",([(var,x)] ++ d))
parseArrayExpr (Identifier var:SChars Col:Num x:SChars SemiCol:xs) b c d (resultReg,_,_) =
            ( xs ,b,c ++ "\tMOV R" ++ (show resultReg) ++ ",=" ++ var  ++"\n",([(var,"space " ++ (show x))] ++ d))

parseArrayExpr (x:xs) _ c _ _ = error(c ++ "\n\nerror what are you trying to do with " ++ (show x))      						   





-----------This is for assignment to a non pointer
--pass it all the information we normally use with the register we want to store the store the result in
parseExpr::[Token]->[[Reg]]->[Char]->Reg->([Token],[[Reg]],[Char])
--this changes numbers into valid form 
parseExpr (Num l:Math op:Num r:xs) b c d = parseExpr (Num (doOp op l r):xs) b c d
parseExpr (Math Minus:Num l:xs) b c d =  parseExpr (Num (-l):xs) b c d 
parseExpr (x:y:Num l:Math op:Num r:xs) b c d = parseExpr (x:y:Num (doOp op l r):xs) b c d
parseExpr (x:y:Math Minus:Num l:xs) b c d =  parseExpr (x:y:Num (-l):xs) b c d 
--use for arrays
parseExpr (Identifier var:SChars LeftSqrBracket:Identifier ind:SChars RightSqrBracket:SChars SemiCol:xs) b c (resultReg,_,_) = 
					if regLType == CHARPTR 
				         then (xs ,b,c ++ "\tLDRB R" ++ (show resultReg) ++ ", [R" ++ (show regLVal) ++ " ,R" ++ (show index) ++"]\n")
					 else error (var ++ " is not char pointer") 
					where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == var]			
						index = getRegVal b ind

parseExpr (Identifier var:SChars LeftSqrBracket:Num index:SChars RightSqrBracket:SChars SemiCol:xs) b c (resultReg,_,_) = 
					if regLType == CHARPTR 
				         then (xs ,b,c ++ "\tLDRB R" ++ (show resultReg) ++ ", [R" ++ (show regLVal) ++ " ,#" ++ (show index) ++"]\n")
					 else error (var ++ " is not char pointer") 
					where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == var]			
						

parseExpr (Identifier l:Math op:Identifier r:SChars SemiCol:xs) b c (resultReg,_,_) = (xs,b, c ++ "\t" ++ (show op) ++ " R" ++ (show resultReg) ++ ", R" ++ (show regL) ++ ", R"
							++ (show regR) ++ "\n")
					where   regL = head [ regInt | (regInt,regType,regName) <- (concat b), regName == l]			
						regR = head [ regInt | (regInt,regType,regName) <- (concat b), regName == r]		

--we cant do a mul directly so we must store it in a temp reg
parseExpr (Identifier l:Math Mult:Num r:SChars SemiCol:xs) b c (resultReg,_,_) = (xs,b,c ++ 
							tmpCode ++ 
							"\n\tMUL R" ++ (show resultReg) ++ ", R" ++ (show regL) ++ ",R" ++ (show tmp) ++ "\n" )
                    where   regL = head [ regInt | (regInt,regType,regName) <- (concat b), regName == l]		
                            (tmp,tmpCode) = putValInTemp b r 

parseExpr (Identifier l:Math op:Num r:SChars SemiCol:xs) b c (resultReg,_,_) = (xs,b,c ++ "\t" ++ (show op) ++ " R" ++ (show resultReg) ++ ", R" ++ (show regL) ++ ",#"
							++ (show r) ++ "\n" )
					where   regL = head [ regInt | (regInt,regType,regName) <- (concat b), regName == l]			
							

parseExpr (Num l:Math op:Identifier r:SChars SemiCol:xs) b c d = if op == Minus then parseExpr (Identifier r:Math ReverseSub:Num l:SChars SemiCol:xs) b c d
																			    else parseExpr (Identifier r:Math op:Num l:SChars SemiCol:xs) b c d
																		
parseExpr (Identifier l:SChars SemiCol:xs) b c (resultReg,_,_)  =
					if regLType == CHARPTR 
				    then (xs ,b,c ++ "\tLDRB R" ++ (show resultReg) ++ ", [R" ++ (show regLVal) ++ "]\n")
					else (xs ,b,c ++ "\tMOV R" ++ (show resultReg) ++ ", R" ++ (show regLVal) ++ "\n") 
					 where   (regLVal,regLType) = head [ (regInt,regType) | (regInt,regType,regName) <- (concat b), regName == l]	

parseExpr (Num l:SChars SemiCol:xs) b c (resultReg,_,_) = (xs ,b,c ++ "\tMOV R" ++ (show resultReg) ++ ",#" ++ (show l) ++ "\n" ) 

parseExpr (Identifier fun:SChars LeftParn:xs) b c (resultReg,_,_) = 
                                                        (toks,b,c ++ code ++ "\tMOV R" ++ (show resultReg) ++",R0\n")
                                                    where
                                                        (toks,_,code) = parseDoFunction (Identifier fun:SChars LeftParn:xs) b ""
                                                        

parseExpr (x:xs) _ c _ = error(c ++ "WOAH WOAH MASSIVE ERROR after assignment of " ++ (show x))















--this is used for assigning registers to variables
parseAssignReg::[Token]->[[Reg]]->[Char]->([Token],[[Reg]],[Char])
parseAssignReg (Types x:Identifier y:SChars SemiCol:xs) [[]] c =  (xs, [[(4,x,y)]], c) 
parseAssignReg (Types x:Identifier y:SChars SemiCol:xs) (b:bs) c = 
								if regPos < 12  
								    then (xs, [[(regPos + 1,x,y)] ++ b] ++ bs, c) 
						            else error ("ran ot of registers") 
								 where
									((regPos,regType,regID):regs) = concat (b:bs)
parseAssignReg (Types x:Identifier y:SChars Assignment:xs) [[]] c = ((Identifier y:SChars Assignment:xs), [[(4,x,y)]], c) 

parseAssignReg (Types x:Identifier y:SChars Assignment:xs) (b:bs) c = 
								if regPos < 12  
								    then ((Identifier y:SChars Assignment:xs), [[(regPos + 1,x,y)] ++ b] ++ bs, c) 
						                else error ("ran ot of registers") 
								 where
									((regPos,regType,regID):regs) = concat (b:bs)
								






----------------------------------------control functions
parseIf::[Token]->[[Reg]]->[Char]->[Data]->ExitVal->([Token],[[Reg]],[Char],[Data])
parseIf (SChars Col:Identifier x :SChars LeftParn:xs) b c d e = (stmtxs, stmtsRegs,  stmtsChars ++ x ++"\n", stmtData)
								where 
									  (stmtxs , stmtsRegs , stmtsChars, stmtData) = parseStmtList condxs ([] ++ b) (conChars ++ x ++"\n") d e
										 where (SChars LeftBracket:condxs , _ , conChars) = parseCondition xs b c
								



parseWhile::[Token]->[[Reg]]->[Char]->[Data]->ExitVal->([Token],[[Reg]],[Char],[Data])
parseWhile (SChars Col:Identifier x :SChars LeftParn:xs) b c d (returnVal,_) = (stmtxs, stmtsRegs,  stmtsChars ++ "\tB " ++ x ++ "\n" ++ "end" ++ x ++"\n", stmtData)
								where 
									  (stmtxs , stmtsRegs , stmtsChars, stmtData) = parseStmtList condxs ([] ++ b) (conChars ++ "end" ++ x ++"\n") d (returnVal,"end" ++ x)
										 where (SChars LeftBracket:condxs , _ , conChars) = parseCondition xs b (c ++ x ++ "\n ")


--end statements
parseBreak::[Token]->[[Reg]]->[Char]->ExitVal->([Token],[[Reg]],[Char])
parseBreak a b c (_,breakName) | breakName /= [] = (a,b,c ++ "\tB " ++ breakName ++"\n")
                               | otherwise       = error("One of your break statements doesn't work")


parseReturn::[Token]->[[Reg]]->[Char]->ExitVal->([Token],[[Reg]],[Char])
parseReturn (Identifier x:SChars SemiCol:xs) b c d = parseReturn xs b (c ++ "\tMOV R0,R" ++ (show id) ++ "\n") d
                                                where id = getRegVal b x 
parseReturn (Num x:SChars SemiCol:xs) b c d = parseReturn xs b (c ++ "\tMOV R0,#" ++ (show x) ++ "\n" ) d
parseReturn a b c (returnName,_) | returnName == "main" = (a,b,c ++ "\tB stop\n")
                                 | otherwise       =  (a,b,c ++ "\tB " ++ returnName ++"\n")




---needs to be expanded
parseCondition::[Token]->[[Reg]]->[Char]->([Token],[[Reg]],[Char])
parseCondition (Identifier x:Cmp y:Identifier z:SChars RightParn :xs ) b c = (xs, b ,(c ++ "\tCMP R" ++ (show (getRegVal b x)) ++ ", R" ++ (show (getRegVal b z)) ++ " \n\tB" ++       
                             (show y) ++ " "))
parseCondition (Identifier x:Cmp y:Num z: SChars RightParn:xs ) b c = (xs, b, (c ++ "\tCMP R" ++ (show (getRegVal b x)) ++ ", #" ++ (show z) ++ " \n\tB" ++ (show y) ++ " "))





printData::[Data]->[Char]
printData [] = []
printData ((name,val):xs) = (name ++ "  DCB  " ++ val ++ "\n") ++ (printData xs)


-- this is for registers
getFreeTempReg::[[Reg]]->Int
getFreeTempReg regs = if regVal < 11 then (regVal + 1) else error "ran out of registers"
					where (regVal,_,_) = head (concat regs)

getRegVal::[[Reg]]->[Char]->Int
getRegVal regs name = head [ regInt | (regInt,regType,regName) <- (concat regs), regName == name]


putValInTemp::[[Reg]]->Int->(Int,[Char])
putValInTemp regs val = (x,"\tMOV R" ++ (show x) ++ ",#" ++ (show val) ++ "\n")
                          where x = getFreeTempReg regs 



--to shorten the read function
tryParse val [] = []    
tryParse val ((attempt, result):xs) =
       if (take (length attempt) val) == attempt
          then [(result, drop (length attempt) val)]
          else tryParse val xs
