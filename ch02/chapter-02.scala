object Chapter2 {
    def abs(n: Int): Int = {
        if (n > 0) n
        else n
    }

    //Exc. 2.1
    def fib(n: Int): Int = {

        @annotation.tailrec
        def go(prev: Int, next: Int, acc: Int): Int = {
            if (acc == 0) prev
            else go(next, next + prev, acc - 1)
        }

        go(0, 1, n - 1)
    }

    private def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }

    //Exc. 2.2
    def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {

        @annotation.tailrec
        def loop(i: Int, j: Int): Boolean = {
            if (j >= as.length) true
            else if (f(as(i), as(j))) loop(j, j + 1)
            else false
        }

        loop(0, 1)
    }

    //Exc. 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        (a: A) => (b: B) => f(a, b)
    }

    //Exc. 2.4
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        (a: A, b: B) => f(a)(b)
    }

    //Exc. 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        (a: A) => f(g(a))
    }

    def main(args: Array[String]): Unit = {
        println(fib(1))
        println(fib(2))
        println(fib(3))
        println(fib(4))

        println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
        println(isSorted(Array(1,3,2), (a: Int, b: Int) => a < b))
        println(isSorted(Array(), (a: Int, b: Int) => a < b))

        val c = curry((x: Int, y: Int) => x + y)
        println(c(5)(6))

        val u = uncurry((a: Int) => (b: Int) => a + b)
        println(u(5, 6))

        val com = compose((x: Int) => x * 2, (y: Int) => y + 5)
        println(com(1))

        val x = List(1, 2, 3, 4, 5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
        }
    }
}
