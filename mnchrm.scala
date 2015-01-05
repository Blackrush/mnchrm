/**
 * Par Antoine CHAUVIN INFOB1
 */
object App {
  import scala.util.Random
  import scala.collection.parallel.ParSeq

  /**
   * Vec represente un vecteur (x, y)
   */
  case class Vec(x: Int, y: Int)

  /**
   * Vec contient des fonctions auxiliaires en rapport aux vecteurs
   */
  object Vec {
    /**
     * Genere aleatoirement un vecteur de translation unite
     */
    def sample(implicit rand: Random) =
      Vec(rand.nextInt(3) - 1, rand.nextInt(3) - 1)
  }

  /**
   * Mat represente une matrice (M, N)
   */
  class Mat(elems: Seq[Seq[Int]]) { mat =>
    val rows: Int = elems.length
    val cols: Int = elems(0).length

    elems forall {_.length == mat.cols}

    /**
     * Loc represente un point precis (a, b) de la matrice
     */
    case class Loc(row: Int, col: Int) {
      /**
       * Recupere la valeur du point de la matrice
       */
      def get(): Int = mat.get(row, col)

      // de grands pouvoirs impliquent de grandes responsabilites
      def apply(): Int = get()
      def unary_~ = get()

      /**
       * Dirige ce point grace a un vecteur
       */
      def map(vec: Vec): mat.Loc = mat.locate(row + vec.x, col + vec.y)
    }

    /**
     * Recupere une point percis de la matrice
     * Respecte le principe de toricite
     */
    def get(i: Int, j: Int): Int =
      if (i < 0)                     get(elems.length + i, j)
      else if (i >= elems.length)    get(i % elems.length, j)
      else if (j < 0)                get(i, elems(i).length + j)
      else if (j >= elems(i).length) get(i, j % elems(i).length)
      else                           elems(i)(j)

    def apply(i: Int, j: Int): Int = get(i, j)

    /**
     * Localise et isole une point precis de la matrice
     */
    def locate(i: Int, j: Int): this.Loc = new Loc(i, j)

    /**
     * Transforme cette matrice par de nouvelles valeurs
     */
    def map(fn: this.Loc => Int): Mat =
      new Mat(for (i <- 0 until elems.length) yield
                    for (j <- 0 until elems(i).length) yield
                        fn(locate(i, j)))

    /**
     * Transforme cette matrice par de nouvelles valeurs deja presentes dans la matrice
     */
    def flatMap(fn: this.Loc => this.Loc): Mat = this map { fn(_).get() }

    /**
     * Meme but que Mat#map mais autorise des optimisations
     */
    def map2(fn: (Int, Int) => Int): Mat =
      new Mat(for (i <- 0 until elems.length) yield
                   for (j <- 0 until elems(i).length) yield
                     fn(i, j))

    /**
     * Analyse la matrice
     * Renvoit {@code false} des que le parametre "fn" renvoit {@code false}
     */
    def forall(fn: Int => Boolean): Boolean = elems forall { _ forall fn }

    /**
     * Retourne {@code true} si la matrice est constante
     * Une matrice est constante si toutes ses valeurs sont egales
     */
    def isConst(): Boolean = {
      val root = get(0, 0)
      this forall { _ == root }
    }

    /**
     * Multiplie cette matrice
     */
    def multiply(x: Int) = this map { loc => ~loc * x }
    def *(x: Int) = multiply(x)

    def row(i: Int): Stream[Int] = elems(i).to[Stream]
    def col(j: Int): Stream[Int] =
      Stream.iterate[Int](0){_+1}
            .takeWhile{_<elems.length}
            .map{x => elems(x)(j)}

    /**
     * Multiplie deux matrices et retourne le resultat
     */
    def multiply(that: Mat) =
      if (this.cols != that.rows || this.rows != that.cols) {
        throw new IllegalArgumentException("those matrices are not multipliable")
      } else {
        val newElems: Seq[Seq[Int]] =
          for (i <- 0 until this.rows) yield
          for (j <- 0 until that.cols) yield
            (this.row(i), that.col(j)).zipped map {_*_} reduce {_+_}

        new Mat(newElems)
      }

    def *(that: Mat) = multiply(that)

    /**
     * Genere une representation humaine de la matrice
     */
    override def toString: String =
      elems map { _.mkString("[", " ", "]") } mkString("[", "\n ", "]")
  }

  /**
   * Mat contient des fonctions auxiliaires en rapport aux matrices
   */
  object Mat {
    /**
     * Genere une matrice vide de taille (M, N)
     */
    def apply(rows: Int, cols: Int): Mat =
      new Mat(Seq.fill[Int](rows, cols)(0))

    /**
     * Genere une matrice vide carre de taille N
     */
    def square(len: Int): Mat = apply(len, len)

    /**
     * Genere une matrice unitaire de taille N
     */
    def unit(len: Int): Mat = {

      /**
       * Genere une ligne de la matrice
       */
      def line(i: Int): Seq[Int] =
        List.fill(i)(0) ++
        List(1) ++
        List.fill(len - i - 1)(0)

      // genere les colonnes de la matrice
      val elems: Seq[Seq[Int]] = for (i <- 0 until len) yield line(i)

      new Mat(elems)
    }

    /**
     * Genere une matrice aleatoire de taille (M, N)
     */
    def sample(rows: Int, cols: Int)(implicit rand: Random): Mat =
      new Mat(for (i <- 0 until rows) yield
                      for (j <- 0 until cols) yield
                        rand.nextInt())

  }

  /**
   * Fais converger aleatoirement une matrice
   */
  def converge[A](mat: Mat)(implicit rand: Random): Mat =
    // mat flatMap { _ map Vec.sample }
    mat map2 { (x, y) =>
      val vec = Vec.sample
      mat(x + vec.x, y + vec.y)
    }

  /**
   * Fais converger une matrice et retourne la matrice convergeante ainsi que sa convergeance
   * La convergeance d'une matrice correspond au nombre d'iterations a converger pour obtenir une matrice convergeante
   */
  def convergeance(seed: Mat)(implicit rand: Random): Int =
    Stream.iterate(seed) { converge(_) } // cree un flux de matrice convergeant indefiniment
          .zipWithIndex // on y associe un indice d'iteration
          .dropWhile{ !_._1.isConst() } // on epuise le flux tant que la matrice n'a pas converge
          .head._2 // et retourne l'indice de la premiere matrice convergeante

  /**
   * Retourne un echantillon de multiple convergeances
   */
  def convergeances(taille: Int, tailleMatrice: Int)(implicit rand: Random): Seq[Int] =
    // genere parallelement plusieurs echantillons
    (0 until taille).par
          .map{ _ => convergeance(Mat.sample(tailleMatrice, tailleMatrice)) }
          .seq

  /**
   * Calcule la moyenne empirique d'un echantillon
   * http://fr.wikipedia.org/wiki/Intervalle_de_confiance#Exemple_I_:_Estimation_d.27une_moyenne
   */
  def moyenne(xs: Seq[Int]): Double = xs.reduce{ _ + _ }.toDouble / xs.length

  /**
   * Calcule la variance sans biais d'un echantillon
   * http://fr.wikipedia.org/wiki/Estimateur_(statistique)#Estimateurs_classiques
   */
  def variance(xs: Seq[Int], mean: Double): Double =
    xs.map{ _.toDouble - mean }
      .map{ x => x*x }
      .reduce{_+_} / (xs.length - 1)

  /**
   * Calcule l'ecart-type d'un echantillon a partir de sa variance sans biais
   */
  def ecartType(xs: Seq[Int], mean: Double): Double =
    math.sqrt(variance(xs, mean))

  /**
   * Calcule l'intervalle de confiance a partir d'une moyenne, d'un ecart-type, de la taille de l'echantillon
   * et enfin de z (TCL tqvu)
   * http://fr.wikipedia.org/wiki/Estimateur_(statistique)#Estimateurs_classiques
   */
  def intervalleConfiance(mean: Double, sigma: Double, n: Int, z: Double): (Double, Double) = {
    val delta = z * sigma / math.sqrt(n)
    (mean - delta, mean + delta)
  }

  /**
   * Point d'entree du programme
   */
  def main(args: Array[String]): Unit = {

    /**
     * Utilise une instance implicite de Random
     */
    implicit val rand = new Random

    /**
     * Comment se servir d'une matrice
     */
    val u = Mat.unit(3)

    /*
     * [[2 0 0]
     *  [0 2 0]
     *  [0 0 2]]
     */
    println(u * 2)
    /**
     * [[1 0 0]
     *  [0 1 0]
     *  [0 0 1]]
     */
    println(u * u)

    /**
     * TP à rendre 5/1/15
     */
    // on definis quelques constantes
    val precision = 1000
    val taille    = 10
    val z         = 1.96
    println(s"precision=$precision")
    println(s"taille=$taille")
    println(s"z=$z")

    // on genere un echantillon
    val echantillon = convergeances(precision, taille)

    // et on fait toutes les stats!
    // merci wikipedia :o)
    val moy = moyenne(echantillon)
    val sig = ecartType(echantillon, moy)
    val (lo, hi) = intervalleConfiance(moy, sig, precision, z)
    println(s"moyenne=$moy")
    println(s"ecart-type=$sig")
    println(s"intervalle=[$lo,$hi]")
  }

}
