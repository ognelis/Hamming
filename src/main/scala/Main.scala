import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

/**
  * Created by Admin on 22.01.2017.
  */
object Main extends App {

  object HMatrix {
    private val nrow = 8
    private val ncol = 4

    private val h = {
      val vec1 = Array[Int](0, 1, 1, 1)
      val vec2 = Array[Int](1, 0, 1, 1)
      val vec3 = Array[Int](1, 1, 0, 1)
      val vec4 = Array[Int](1, 1, 1, 0)
      val vec5 = Array[Int](1, 0, 0, 0)
      val vec6 = Array[Int](0, 1, 0, 0)
      val vec7 = Array[Int](0, 0, 1, 0)
      val vec8 = Array[Int](0, 0, 0, 1)
      Array(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8)
    }

    def recover(vectorToRecover: Array[Int], sindrom: Array[Int]): Array[Int] = {
      def findPositionOfError : Int = {
        val index = h.indexWhere(x => x sameElements sindrom)
        if (index == -1) throw new Exception("Double error") else index
      }

      val index = findPositionOfError
      val elem =  vectorToRecover(index)

      val newArray = new Array[Int](vectorToRecover.length)
      Array.copy(vectorToRecover, 0, newArray, 0, vectorToRecover.length)

      if (elem == 1) newArray.update(index, 0) else newArray.update(index, 1)
      newArray
    }

    def hasError(vector: Array[Int]) = !(vector sameElements Array[Int](0,0,0,0))

    def multiply(vector: Array[Int]) = {
      (0 until ncol).map(j => (0 until nrow).map { i =>
        vector(i) * h(i)(j)
      }.sum % 2).toArray
    }
  }

  object GMatrix {
    private val nrow = 4
    private val ncol = 8

    private val g = {
      val vec1 = Array[Int](1, 0, 0, 0, 0, 1, 1, 1)
      val vec2 = Array[Int](0, 1, 0, 0, 1, 0, 1, 1)
      val vec3 = Array[Int](0, 0, 1, 0, 1, 1, 0, 1)
      val vec4 = Array[Int](0, 0, 0, 1, 1, 1, 1, 0)
      Array(vec1, vec2, vec3, vec4)
    }

    def multiply(vector: Array[Int]) = {
      (0 until ncol).map(j => (0 until nrow).map { i =>
        vector(i) * g(i)(j)
      }.sum % 2).toArray
    }

    def getWord(vector: Array[Int]) = vector.splitAt(4)._1
  }

  object Util {
    val rand = scala.util.Random

    def addBytesToArray(binary: String): String = {
      val bytesToAdd = binary.length % 4
      if (bytesToAdd == 0) "0001" ++ binary
      else {
        if (bytesToAdd == 1) '1' +: binary
        else Array.fill(bytesToAdd - 1)('0').mkString ++ ('1' +: binary)
      }
    }

    def getBytesFromArray(binary: String) = {
      binary.splitAt(binary.indexWhere(char => char == '1') + 1)._2
    }

    def doubleError(vector: Array[Int]): Array[Int] = {
      val index1 = rand.nextInt(vector.length)
      val elem1 = vector(index1)
      val elemUpdate1 = if (vector(index1) == 1) 0 else 1

      val index2 = rand.nextInt(vector.length)
      val elem2 = vector(index2)
      val elemUpdate2 = if (vector(index2) == 1) 0 else 1

      val newArray = new Array[Int](vector.length)
      Array.copy(vector, 0, newArray, 0, vector.size)

      newArray.update(index1, elemUpdate1)
      newArray.update(index2, elemUpdate2)

      newArray
    }

    def addError(vector: Array[Int]): Array[Int] = {
      val index = rand.nextInt(vector.length)
      val elem = vector(index)
      val elemUpdate = if (vector(index) == 1) 0 else 1
      val newArray = new Array[Int](vector.length)
      Array.copy(vector, 0, newArray, 0, vector.size)
      newArray.update(index, elemUpdate)
      newArray
    }
  }

  val from = scala.io.Source.fromFile("text.txt").mkString
  val text = from.getBytes.map(byte => String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0')).mkString

  val res = Util.addBytesToArray(text).sliding(4,4) map  { x =>
    val word = x.map(_.toInt).toArray
    val encode = Util.addError(GMatrix.multiply(word))
    val decode = HMatrix.multiply(encode)
    if (HMatrix.hasError(decode))
      GMatrix.getWord(HMatrix.recover(encode, decode)).mkString
    else
      GMatrix.getWord(encode).mkString
  }

  val toOutput = Util.getBytesFromArray(res.mkString)
  val result = toOutput.sliding(8,8).map { x =>
    java.lang.Integer.parseUnsignedInt(x.mkString, 2).toByte
  }.toArray

  println(new String(result))
}
