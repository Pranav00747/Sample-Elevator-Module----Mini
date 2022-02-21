import Control.Monad (unless)
import System.GPIO

initPinsEval :: Int :: Int :: Int  :: Int :: Int :: Int :: Int :: Int :: Int -> Boolean

initPinsEval b0 b1 b2 b3 b4 b5 fnb rt fn = do

  p0 <- openPin(Pin b0)

  p1 <- openPin(Pin b1)

  p2 <- openPin(Pin b2)

  p3 <- openPin(Pin b3)

  p4 <- openPin(Pin b4)

  p5 <- openPin(Pin b5)

  fnb0 <- openPin(Pin fnb)

  r_m <- openPin(Pin rt)

  fn <- openPin(Pin fn)

  setPinInputMode p0 DefaultInput

  setPinInputMode p1 DefaultInput

  setPinInputMode p3 DefaultInput

  setPinInputMode p4 DefaultInput

  setPinInputMode p5 DefaultInput

  setPintInputMode fnb0 DefaultInput

  setPinOutputMode r_m DefaultOutput High

  setPinOutputMode fn DefaultOutput High
  True

user_mod :: Int :: Int :: Int :: Int

user_mod x b cp r = do
   y = 0

  if b==High
      if x /= cp then
        if x  > cp then
          unless ( cp==x ) $ do
               unless(y<=250) $ do
                  writePin r High
                     y = y+25
            y=0
            cp = cp+1

      else if x < cp then
          unless(cp==x) $ do
             unless(y<=250) $ do
               writePin r High
               y = y+25
          y=0
          cp = cp - 1
 

          
    
main :: IO ()

main  = do

btn0 = 0
btn1 = 1
btn2 = 2
btn3 = 3
btn4 = 4
btn5 = 5
fn_b = 6
rot = 9
fan = 8
curr_position = 1
x = False

is_t = initPinsEval btn0 btn1 btn2 btn3 btn4 btn5 fn_b rot fan 

if is_t /= False then
 b1 <- readPin btn0

 b2 <- readPin btn1

 b3 <- readPin btn3

 b4 <- readPin btn4

 b5 <- readPin btn5

 fb <- readPin fn_b

 if b1==High then

   user_mod 1 b1 curr_position rot

 else if b2==High then

   user_mod 2 b2 curr_position rot

 else if b3==High then

   user_modbtn 3 b3 curr_position rot

 else if b4==High then

   user_mod 4 b4 curr_position rot

 else if b5==High then

   user_mod 5 b5 curr_position rot

 else if fb==High then

      if x==False then
         writePin fan High
         x=True
    else if x==True then
        writePin fan Low
         x= False

   
  






