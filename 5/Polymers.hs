import Data.Char (ord)
import Control.Monad 
import Data.Bool (bool)
main = do{let{f(a:b:x)=bool(a:f(b:x))(b:x)(a-b==32);f x=x;s=length.(until=<<((==)=<<))f};x<-liftM(map ord)$readFile "input";print.s$x;print$minimum[s$filter(`notElem`[c,c+32])x|c<-[65..90]]
