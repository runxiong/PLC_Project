module Test

open Absyn

let cases = 
  (
    let s = "0"
    let e = ConI 0
    (s, e)
  ) :: (
   let s = "5+3*4"  
   let e = Prim2 ("+",ConI 5,Prim2 ("*",ConI 3,ConI 4))
   (s, e) 
  ) :: ( 
   let s = "-3 < 4" 
   let e = Prim2 ("<",Prim1 ("-",ConI 3),ConI 4)
   (s, e)
  ) :: (
    let s = "!(3 = 4)" 
    let e = Prim1 ("!",Prim2 ("=",ConI 3,ConI 4))
    (s, e)
  ) :: (
    let s = "3+1 = 4 && 4 <= 3"
    let e = Prim2 ("&&",Prim2 ("=",Prim2 ("+",ConI 3,ConI 1),ConI 4), Prim2 ("<=",ConI 4,ConI 3))
    (s, e)
  ) :: (
    let s = "if 3 = 2 then 0 else 1 + 4"
    let e = If (Prim2 ("=",ConI 3,ConI 2),ConI 0,Prim2 ("+",ConI 1,ConI 4))
    (s, e)
  ) :: (
    let s = "3 + if 3 = 2 then 0 else 1"
    let e = Prim2 ("+",ConI 3,If (Prim2 ("=",ConI 3,ConI 2),ConI 0,ConI 1))
    (s, e)
  ) :: (
    let s = "4; true"
    let e = Prim2 (";",ConI 4,ConB true)
    (s, e)
  ) :: (
    let s = "4 * (true; 6)"
    let e = Prim2 ("*",ConI 4,Prim2 (";",ConB true,ConI 6))
    (s, e)
  ) :: (
    let s = "( )"
    let e = Tuple []
    (s, e)
  ) :: (
    let s = "(1,false,())"
    let e = Tuple [ConI 1; ConB false; Tuple []]
    (s, e)
  ) :: (
    let s = "(1,(2,3),4)"
    let e = Tuple [ConI 1; Tuple [ConI 2; ConI 3]; ConI 4]
    (s, e)
  ) :: (
    let s = "(true,false)#1"
    let e = Sel (Tuple [ConB true; ConB false],1)
    (s, e)
  ) :: (
    let s = "((5,6),false)#1#2"
    let e = Sel (Sel (Tuple [Tuple [ConI 5; ConI 6]; ConB false],1),2)
    (s, e)
  ) :: (
    let s = "1 + {3}"
    let e = Prim2 ("+",ConI 1,ConI 3)
    (s, e)
  ) :: (
    let s = "print false"
    let e = Prim1 ("print",ConB false)
    (s, e)
  ) :: (
    let s = "print (1 - 3)"
    let e = Prim1 ("print",Prim2 ("-",ConI 1,ConI 3))
    (s, e)
  ) :: (
    let s = "([]: List[Int])"
    let e = EList (LisT IntT)
    (s, e)
  ) :: (
    let s = "([]: List[Int])"
    let e = EList (LisT IntT)
    (s, e)
  ) :: (
    let s = "([]: List[Bool])"
    let e = EList (LisT BooT)
    (s, e)
  ) :: (
    let s = "([]: List[Unit])"
    let e = EList (LisT (TupT []))
    (s, e)
  ) :: (
    let s = "([]: List[List[Int]])"
    let e = EList (LisT (LisT IntT))
    (s, e)
  ) :: (
    let s = "([]: List[Int -> Unit])"
    let e = EList (LisT (FunT (IntT,TupT [])))
    (s, e)
  ) :: (
    let s = "
    ([]: List[Int -> Int -> Bool])"
    let e = EList (LisT (FunT (IntT,FunT (IntT,BooT))))
    (s, e)
  ) :: (
    let s = "([]: List[Tuple[Unit, Int, Bool]])"
    let e = EList (LisT (TupT [TupT []; IntT; BooT]))
    (s, e)
  ) :: (
    let s = "1 :: ([]: List[Int])"
    let e = Prim2 ("::",ConI 1,EList (LisT IntT))
    (s, e)
  ) :: (
    let s = "1 :: 2 :: ([]: List[Int])"
    let e = Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))
    (s, e)
  ) :: (
    let s = "(1,2) :: (3,4) :: ([]: List[Tuple[Int,Int]])"
    let e = Prim2 ("::",Tuple [ConI 1; ConI 2], Prim2 ("::",Tuple [ConI 3; ConI 4],EList (LisT (TupT [IntT; IntT]))))
    (s, e)
  ) :: (
    let s = "hd (1 :: 2 :: ([]: List[Int]))"
    let e = Prim1 ("hd",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT))))
    (s, e)
  ) :: (
    let s = "tl (1 :: 2 :: ([]: List[Int]))"
    let e = Prim1 ("tl",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT))))
    (s, e)
  ) :: (
    let s = "ise([]: List[Int])"
    let e = Prim1 ("ise",EList (LisT IntT))
    (s, e)
  ) :: (
    let s = "ise(true::([]: List[Bool]))"
    let e = Prim1 ("ise",Prim2 ("::",ConB true,EList (LisT BooT)))
    (s, e)
  ) :: (
    let s = "var x = 4; x+1"
    let e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    (s, e)
  ) :: (
    let s = "{var x = 4; x+1}"
    let e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    (s, e)
  ) :: (
    let s = "
    var x = 4; 
    var y = 6;
    x + y"
    let e = Let ("x",ConI 4,Let ("y",ConI 6,Prim2 ("+",Var "x",Var "y")))
    (s, e)
  ) :: (
    let s = "
    var x = 4; 
    print x;
    {var y = 6;
     print y
    }"
    let e = Let ("x",ConI 4,Prim2 (";",Prim1 ("print",Var "x"),Let ("y",ConI 6,Prim1 ("print",Var "y"))))
    (s, e)
  ) :: (
    let s = "1 + {var tmp = 9; x + x}"
    let e = Prim2 ("+",ConI 1,Let ("tmp",ConI 9,Prim2 ("+",Var "x",Var "x")))
    (s, e)
  ) :: (
    let s = "
    var a = (3,4);
    a#1 < a#2 
    "
    let e = Let ("a",Tuple [ConI 3; ConI 4],Prim2 ("<",Sel (Var "a",1),Sel (Var "a",2)))
    (s, e)
  ) :: (
    let s = "
    var e = ([]:List[Bool]);
    true::false::e 
    "
    let e = Let ("e",EList (LisT BooT),Prim2 ("::",ConB true,Prim2 ("::",ConB false,Var "e")))
    (s, e)
  ) :: (
    let s = "fn (x:Int) => x end"
    let e = Anon ("x",IntT,Var "x")
    (s, e)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    (s, e)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    (s, e)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f(10)"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Call (Var "f",ConI 10))
    (s, e)
  ) :: (
    let s = "fun f (x:Int) = x; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    (s, e)
  ) :: (
    let s = "fun f (x:Int) = {fun g(y:Int) = x+y; g}; f(3)(4)"
    let e = Let ("f", Anon ("x",IntT,Let ("g",Anon ("y",IntT,Prim2 ("+",Var "x",Var "y")),Var "g")),Call (Call (Var "f",ConI 3),ConI 4))
    (s, e)
  ) :: (
    let s = "fun f (x:Int) = fn (y:Int) => x+y end; f(3)(4)"
    let e = Let ("f",Anon ("x",IntT,Anon ("y",IntT,Prim2 ("+",Var "x",Var "y"))), Call (Call (Var "f",ConI 3),ConI 4))
    (s, e)
  ) :: (
    let s = "
    fun f (g: Int -> Bool) = if g(1) then 10 else 11; 
    fun h (x: Int) = 0 < x;
    f(h)
    "
    let e = Let ("f",Anon ("g",FunT (IntT,BooT),If (Call (Var "g",ConI 1),ConI 10,ConI 11)), Let ("h",Anon ("x",IntT,Prim2 ("<",ConI 0,Var "x")),Call (Var "f",Var "h")))
    (s, e)
  ) :: (
    let s = "fun rec f(x:Int):Int = if x <= 0 then 1 else x + f(x-1); f(5)"
    let e = Letrec ("f","x",IntT, If (Prim2 ("<=",Var "x",ConI 0),ConI 1, Prim2 ("+",Var "x",Call (Var "f",Prim2 ("-",Var "x",ConI 1)))),IntT, Call (Var "f",ConI 5))
    (s, e)
  ) :: (
    let s = "
    fun rec pr(x:Int): Unit = 
      if x <= 0 then 
        print(0)
      else { 
        print(x);
        pr(x-1)
      };
    pr(5)"
    let e = Letrec ("pr","x",IntT, If (Prim2 ("<=",Var "x",ConI 0),Prim1 ("print",ConI 0), Prim2 (";",Prim1 ("print",Var "x"),Call (Var "pr",Prim2 ("-",Var "x",ConI 1)))), TupT [],Call (Var "pr",ConI 5))
    (s, e)
  ) :: (
    let s = "
    fun rec len(l : List[Int]): Int = if ise(l) then 0 else 1 + len(tl(l)); 
    len(1::2::([]:List[Int]))"
    let e = Letrec ("len","l",LisT IntT, If (Prim1 ("ise",Var "l"),ConI 0, Prim2 ("+",ConI 1,Call (Var "len",Prim1 ("tl",Var "l")))),IntT, Call (Var "len",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))))
    (s, e)
  ) :: (
    let s = "fn (x:Int, y:Int) => x - y end"
    let e = Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y"))))
    (s, e)
  ) :: (
    let s = "fun f(x:Int, y:Int) = x - y; f(5,4)"
    let e = Let ("f", Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Tuple [ConI 5; ConI 4]))
    (s, e)
  ) :: (
    let s = "
    var p = (1,3);
    fun f(x:Int, y:Int) = x - y; 
    f(p)"
    let e = Let ("p",Tuple [ConI 1; ConI 3], Let ("f", Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Var "p")))
    (s, e)
  ) :: (
    let s = "fun f(x:Int, y:Int, z: Int) = x - y * z ; f(5,4,2)"
    let e = Let ("f", Anon ("$tuple",TupT [IntT; IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2), Let ("z",Sel (Var "$tuple",3), Prim2 ("-",Var "x",Prim2 ("*",Var "y",Var "z")))))), Call (Var "f",Tuple [ConI 5; ConI 4; ConI 2]))
    (s, e)
  ) :: (
    let s = "
    fun rec mem(x: Int, l : List[Int]): Bool = 
      if ise(l) then false 
      else if x = hd(l) then true else mem(x, tl(l)); 
    mem(2, 1::2::([]:List[Int]))"
    let e = Letrec ("mem","$tuple",TupT [IntT; LisT IntT],Let ("x",Sel (Var "$tuple",1), Let ("l",Sel (Var "$tuple",2),If (Prim1 ("ise",Var "l"),ConB false,If (Prim2 ("=",Var "x",Prim1 ("hd",Var "l")),ConB true, Call (Var "mem",Tuple [Var "x"; Prim1 ("tl",Var "l")]))))),BooT, Call (Var "mem", Tuple [ConI 2; Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))]))
    (s, e)
  ) :: (
    let s = "
    var E = ([] : List[Int]);
    fun reverse (l : List[Int]) = {
      fun rec rev (l1 : List[Int], l2 : List[Int]): List[Int] =
        if ise(l1) then
          l2 
        else
          rev(tl(l1), hd(l1)::l2);
      rev(l, E)
    };
    reverse (1::2::3::E)"
    let e = Let ("E",EList (LisT IntT),Let ("reverse",Anon ("l",LisT IntT,Letrec ("rev","$tuple",TupT [LisT IntT; LisT IntT],Let ("l1",Sel (Var "$tuple",1),Let ("l2",Sel (Var "$tuple",2), If (Prim1 ("ise",Var "l1"),Var "l2",Call (Var "rev",Tuple [Prim1 ("tl",Var "l1");Prim2 ("::",Prim1 ("hd",Var "l1"),Var "l2")])))),LisT IntT,Call (Var "rev",Tuple [Var "l"; Var "E"]))),Call (Var "reverse",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,Prim2 ("::",ConI 3,Var "E"))))))
    (s, e)
  ) :: (
    let s = "
    fun rec map (f : Int -> Int) : (List[Int] -> List[Int]) =
      fn (l: List[Int]) =>
        if ise(l) then l else f(hd(l)) :: map(f)(tl(l))
      end ;
    map (fn (x:Int) => 2*x end) (10::20::30::([] : List[Int]))"
    let e = Letrec ("map","f",FunT (IntT,IntT),Anon("l",LisT IntT,If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),FunT (LisT IntT,LisT IntT),Call(Call (Var "map",Anon ("x",IntT,Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,EList (LisT IntT))))))
    (s, e)
  )
  :: []
