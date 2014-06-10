
object Main {

  class RationalNumber(val n: Int, val d: Int) {

    def this(n: Int) = this(n, 1)

    def +(that: RationalNumber) =
      new RationalNumber(n * that.d + that.n + d, d * that.d)

    override def toString = s"$n/$d"
  }

  object General {

    //sum1
    def sum(xs: List[RationalNumber]) = xs.foldLeft(new RationalNumber(0)) { (a, b) => a + b }
    
    //sum2
    def sum(xs: Array[RationalNumber]) = xs.foldLeft(new RationalNumber(0)) { (a, b) => a + b }

    //sum3
    def sum(xs: List[Int]) = xs.foldLeft(0) { (a, b) => a + b }
    
    //sum4
    def sum(xs: Array[Int]) = xs.foldLeft(0) { (a, b) => a + b }

    //sum4
   
    // def sum(m: Map[Int, Int]) = m.foldLeft(0) { (a, b) => a + b._2 }

    //def sum(m: Map[RationalNumber, RationalNumber]) = m.foldLeft(new RationalNumber(0)) { (a, b) => a + b._2 }

  }

  import Step1.Monoid
  import Step2.FoldLeft

  def main(args: Array[String]) {
    val intList = List(1, 2, 3, 4, 5)
    val rnList = List(new RationalNumber(1,2), new RationalNumber(2,3), new RationalNumber(3,4))
    val intArray = Array(1,2,3,4,5)
    val rnArray = List(new RationalNumber(1,2), new RationalNumber(2,3), new RationalNumber(3,4))

    println(s"Initial List Int: ${General.sum(intList)}") //sum1
    println(s"Initial List Rational Number: ${General.sum(rnList)}") //sum2
    println(s"Initial Array Int: ${General.sum(intArray)}") //sum3
    println(s"Initial Array Rational Number: ${General.sum(rnArray)}") //sum4
        
    implicit val intMonoid = new  Monoid[Int] {
      def mappend(a: Int, b: Int):Int = a + b
      def mzero = 0
    }

    implicit val rnMonoid = new  Monoid[RationalNumber] {
      def mappend(a: RationalNumber, b: RationalNumber) = a + b
      def mzero = new RationalNumber(0)
    }

    println(s"Step1 List Int: ${Step1.sum(intList)}") //sum5
    println(s"Step1 List RationalNumber: ${Step1.sum(rnList)}") //sum6
    println(s"Step1 Array RationalNumber: ${Step1.sum(intArray)}") //sum5
    println(s"Step1 Array RationalNumber: ${Step1.sum(rnArray)}") //sum6

    implicit val listFoldLeft = new  FoldLeft[List] {
      def foldLeft[A, B](xs: List[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
   }

   implicit val ArrayFoldLeft = new  FoldLeft[Array] {
     def foldLeft[A, B](xs: Array[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
   }

    println(s"Step2 List Int: ${Step2.sum(intList)}") //sum7
    println(s"Step2 List RationalNumber: ${Step2.sum(rnList)}") //sum7
    println(s"Step2 Array RationalNumber: ${Step2.sum(intArray)}") //sum7
    println(s"Step2 Array RationalNumber: ${Step2.sum(rnArray)}") //sum7
  }

  object Step1 {
  
    trait Monoid[A] {
      def mappend(a: A, b: A): A
      def mzero: A
    }

    //def sum[A](xs: List[A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }

    //def sum[A](xs: Array[A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }

    //def sum[A](xs: Map[_, A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b._2) }
    
    //sum5
    def sum[A: Monoid](xs:List[A]){ 
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }
    }
    
    //sum6
    def sum[A: Monoid](xs:Array[A]){ 
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero) { (a, b) => m.mappend(a, b) }
    }

  }

  object Step2 {

    trait FoldLeft[M[_]] {
      def foldLeft[A, B](xs: M[B])(a: A)(f: (A, B) => A): A
    }

    /*trait FoldLeftMap[M[_, _]] {
      def foldLeft[A, B, C](xs: M[B, C])(a: A)(f: (A, (B, C)) => A): A
    }

    class ListFoldLeft extends FoldLeft[List] {
      def foldLeft[A, B](xs: List[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }

    class ArrayFoldLeft extends FoldLeft[Array] {
      def foldLeft[A, B](xs: Array[B])(a: A)(f: (A, B) => A): A = xs.foldLeft(a)(f)
    }*/

    //def sum[M[A], A](xs: M[A])(fl: FoldLeft[M])(implicit m: Monoid[A]) = fl.foldLeft(xs)(m.mzero) { (a, b) => m.mappend(a, b) }
    
    //sum7
    def sum[M[A]: FoldLeft, A:Monoid](xs: M[A]) = { 
      val fl = implicitly[FoldLeft[M]]
      val m = implicitly[Monoid[A]]
      fl.foldLeft(xs)(m.mzero) { (a, b) => m.mappend(a, b) }			 
    }
  }

}

