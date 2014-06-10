
object Main {

  class RationalNumber(val n: Int, val d: Int) {

    def this(n: Int) = this(n, 1)

    def +(that: RationalNumber) =
      new RationalNumber(n * that.d + that.n + d, d * that.d)
  }

  object General {

    def sum(xs: List[RationalNumber]) = xs.foldLeft(new RationalNumber(0)) { (a, b) => a + b }

    def sum(xs: Array[RationalNumber]) = xs.foldLeft(new RationalNumber(0)) { (a, b) => a + b }

    def sum(m: Map[RationalNumber, RationalNumber]) = m.foldLeft(new RationalNumber(0)) { (a, b) => a + b._2 }

    def sum(xs: List[Int]) = xs.foldLeft(0) { (a, b) => a + b }

    def sum(m: Map[Int, Int]) = m.foldLeft(0) { (a, b) => a + b._2 }
  }

  import Step1.Monoid
  import Step2.FoldLeft

  def main(args: Array[String]) {
    val intList = List(1, 2, 3, 4, 5)
    val rnList = List(new RationalNumber(1,2), new RationalNumber(2,3), new RationalNumber(3,4))

    println(General.sum(intList))
        
    implicit val intMonoid = new  Monoid[Int] {
      def mappend(a: Int, b: Int):Int = a + b
      def mzero = 0
    }

    implicit val rnMonoid = new  Monoid[RationalNumber] {
      def mappend(a: RationalNumber, b: RationalNumber) = a + b
      def mzero = new RationalNumber(0)
    }

    println(Step1.sum(intList))
    println(Step1.sum(rnList))

    implicit val listFoldLeft = new  FoldLeft[List] {
      def foldLeft[A, B](xs: List[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }

    implicit val ArrayFoldLeft = new  FoldLeft[Array] {
      def foldLeft[A, B](xs: Array[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }

  //  println(Step2.sum(intList))
//    println(Step2.sum(rnList))


  }

  object Step1 {
  
    trait Monoid[A] {
      def mappend(a: A, b: A): A
      def mzero: A
    }

    def sum[A](xs: List[A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }

    def sum1[A: Monoid](xs:List[A]) = { 
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }
    }

    def sum[A](xs: Map[_, A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b._2) }

  }

  object Step2 {

    trait FoldLeft[M[_]] {
      def foldLeft[A, B](xs: M[B])(a: A)(f: (A, B) => A): A
    }

    trait FoldLeftMap[M[_, _]] {
      def foldLeft[A, B, C](xs: M[B, C])(a: A)(f: (A, (B, C)) => A): A
    }

    class ListFoldLeft extends FoldLeft[List] {
      def foldLeft[A, B](xs: List[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }

    class ArrayFoldLeft extends FoldLeft[Array] {
      def foldLeft[A, B](xs: Array[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }

    def sum[M[A], A](xs: M[A])(fl: FoldLeft[M])(implicit m: Monoid[A]) = fl.foldLeft(xs)(m.mzero) { (a, b) => m.mappend(a, b) }
  }

}

