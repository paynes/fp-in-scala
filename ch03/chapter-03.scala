//Exc. 3.2
def tail: List[A] = this match {
    case Cons(_, xs) => xs
    case Nil => throw new IllegalStateException("Tail on empty list.")
}

def setHead[A](l: List[A], y: A): List[A] = l match {
    case Cons(_, xs) => Cons(y, xs)
    case Nil => throw new IllegalStateException("SetHead on empty list.")
}

def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
        case Cons(_, t) => drop(t, n-1)
        case Nil throw new IllegalStateException("Drop on empty list.")
    }

}
