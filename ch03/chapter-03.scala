//Exc. 3.2
def tail: List[A] = this match {
    case Cons(_, xs) => xs
    case Nil => throw new IllegalStateException("Tail on empty list.")
}

//Exc. 3.3
def setHead[A](l: List[A], y: A): List[A] = l match {
    case Cons(_, xs) => Cons(y, xs)
    case Nil => throw new IllegalStateException("SetHead on empty list.")
}

//Exc. 3.4
def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
        case Cons(_, t) => drop(t, n-1)
        case Nil throw new IllegalStateException("Drop on empty list.")
    }
}

//Exc. 3.5
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if f(x) dropWhile(xs, f)
    case _ => l
}

//Exc. 3.6
def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalStateException("Init on empty list.")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Case(x, init(xs))
}

//Exc. 3.9
def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
}

//Exc. 3.10
@annotation.tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
}

//Exc. 3.11
def sum(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
}

def product(l: List[Double]): Int = {
    foldLeft(l, 1.0)(_ * _)
}

def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
}

//Exc. 3.12
def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((a, b) => Cons(b, a))
}

//Exc. 3.14
def append[A](l: List[A], m: List[A]): List[A] = {
    foldRight(l, r)(Cons(_,_))
}

//Exc. 3.15
def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
}

//Exc. 3.16, 3.17
def transform1(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[A])((x, xs) => Cons(x+1, t))
}

def transform2(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[A])((x, xs) => Cons(x.toString(), t))
}

//Exc. 3.18
def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    foldRight(l, Nil:List[A])((x, xs) => Cons(f(x), t))
}

//Exc. 3.19
def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((x, xs) => if f(x) Cons(x, t) else t)
}

//Exc. 3.20
def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
}

//Exc. 3.21
def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if f(a) List(a) else Nil)
}

//Exc. 3.22
def pair(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, pair(t1,t2))
}

//Exc. 3.23
def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
}

//Exc. 3.25
def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
}

//Exc. 3.26
def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
}

//Exc. 3.27
def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
}

//Exc. 3.28
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
}

//Exc. 3.29
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)(1 + _ + _)
}

def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
}

def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
}
