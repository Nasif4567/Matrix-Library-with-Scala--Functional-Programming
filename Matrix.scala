/*
 * An implementation of a matrix of Int values.
 * Author: David Smallwood.
 * Date: 17 November 2023.
 */

package lib.matrix
import scala.annotation.tailrec

import lib.matrix
import lib.picture.*
import lib.picture.Picture.*

import scala.collection.immutable.Seq

/** A class for immutable matrices containing Int values.
  *
  * @param rows The data for the matrix arranged by row.
  */
class Matrix(val rows: Seq[Seq[Int]]) {

  /* Invariant condition:
   * There must be at least one row, and
   * each row must be the same length.
   *
   * This property must hold initially, and all operations
   * must maintain it.
   */
  require(rows.nonEmpty && rows.head.nonEmpty, "empty rows")

  val row1Len: Int = rows.head.length
  require(rows.tail.map(_.length).forall(_ == row1Len), "ragged rows")

  /*
   * The internal representation of a matrix uses a vector of
   * vectors. The vector data structure provides good performance
   * for random access. Due to the very wide/flat balanced tree
   * used to implement a vector, the access time is considered
   * to be "effectively constant".
   */
  val elements: Vector[Vector[Int]] = rows.map(_.toVector).toVector

  /** The number of rows in this matrix. */
  val numRows: Int = elements.length

  /** The number of columns in this matrix. */
  val numCols: Int = elements(0).length

  /** The order of this matrix: (number of rows, number of cols) */
  val order: (Int, Int) = (numRows, numCols)

  /** Is the matrix square? */
  val isSquare: Boolean = this.numCols == this.numRows

  private def canEqual(a: Any): Boolean = a.isInstanceOf[Matrix]

  /** Tests whether two matrices are equal. */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Matrix =>
        that.canEqual(this) &&
          this.order == that.order &&
          this.elements == that.elements
      case _ => false
    }
  }

  /** A hash function for matrices. */
  override def hashCode: Int =
    val somePrime = 31
    somePrime * elements.hashCode()

  /** Create a Picture representation of this matrix. */
  def toPicture: Picture = {
    val cells: Seq[Seq[Picture]] =
      elements.map(row =>
        row.map(col =>
          Picture(col.toString).borderL()
        )
      ).normalise(RGT)
    cells.map(_.spread()).stack().borderR().frameLR
  }

  override def toString: String = this.toPicture.toString

  /** Return a value in the matrix using row/column indexes. */
  def apply(row: Int, col: Int): Int =
    require(row >= 1 && row <= numRows && col >= 1 && col <= numCols, "Out of bounds")
    elements(row - 1)(col - 1)

  /** Transpose this matrix. */
  def transpose: Matrix = Matrix(elements.transpose)

  /** Apply a function to every element in the matrix. */
  def map(f: Int => Int): Matrix = Matrix(this.elements.map(_.map(f)))

  /** Subtract another matrix from this one. */
  def -(that: Matrix): Matrix = this + that.map(0 - _)

  /** Reduce all the rows to a single row by applying an operator to corresponding elements in each column */
  def reduceRows(op: (Int, Int) => Int): Matrix =
    this.transpose.reduceCols(op).transpose

  /** Apply an operator throughout all elements in the matrix */
  def reduce(op: (Int, Int) => Int): Matrix =
    this.reduceRows(op).reduceCols(op)

  /** Join two matrices with the same number of rows side by side */
  def beside(that: Matrix): Matrix =
    require(this.numRows == that.numRows, "Different numbers of rows")
    Matrix(this.elements zip that.elements map (_ ++ _))

  /** COURSEWORK METHOD */

  /**
   * Adds another matrix to this one.
   *
   * @param that The matrix to be added.
   * @return The resulting matrix after addition.
   * @throws IllegalArgumentException if the dimensions of the matrices are not compatible for addition.
   */
  def +(that: Matrix): Matrix = (this, that) match {
    // Check if matrices have compatible dimensions
    case _ if this.numCols != that.numCols || this.numRows != that.numRows =>
      throw new IllegalArgumentException("Matrices must have the same dimensions for addition.")
    // If dimensions are compatible, perform element-wise addition
    case _ =>
      // Zip the elements of corresponding rows and perform addition
      val addedElements = this.elements.zip(that.elements).map { case (row1, row2) =>
        row1.zip(row2).map { case (elem1, elem2) =>
          elem1 + elem2
        }
      }
      // Create a new matrix from the added elements and return
      Matrix(addedElements)
  }


  /** COURSEWORK METHOD
   * Multiplies this matrix by another matrix.
   *
   * @param that The matrix to be multiplied with.
   * @return The resulting matrix after multiplication.
   * @throws IllegalArgumentException if the matrices are empty or not compatible for multiplication.
   */
  def *(that: Matrix): Matrix = {
    // Check if matrices are non-empty and have compatible dimensions for multiplication
    require(rows.nonEmpty && that.rows.nonEmpty, "Matrices cannot be empty")
    require(rows.head.size == that.rows.size, "Matrices are not compatible for multiplication")

    // Get the dimensions of matrices A and B
    val numRowsA = rows.size
    val numColsA = rows.head.size
    val numColsB = that.rows.head.size

    // Calculate the elements of the resulting matrix
    val resultRows = for (i <- 0 until numRowsA) yield {
      for (j <- 0 until numColsB) yield {
        // Perform dot product for each element of the resulting matrix
        (for (k <- 0 until numColsA) yield rows(i)(k) * that.rows(k)(j)).sum
      }
    }.toVector

    // Create a new matrix from the resulting rows and return
    Matrix(resultRows)
  }


  /** COURSEWORK METHOD
   * Raises this matrix to the power of n.
   *
   * @param n The exponent to which the matrix is raised.
   * @return The resulting matrix after raising to the power.
   * @throws IllegalArgumentException if n is negative.
   */
  def power(n: Int): Matrix = {
    // Check if n is non-negative
    require(n >= 0, "Sorry, the number cannot be negative")

    // Base case: if n is 0, return the identity matrix of the same size
    if (n == 0) {
      Matrix.id(this.numRows)
    } else {
      // Recursive case: multiply the matrix by itself (n-1) times
      this * power(n - 1)
    }
  }



  /** COURSEWORK METHOD
   * Joins this matrix with another matrix by stacking them vertically.
   *
   * @param that The matrix to be stacked above this matrix.
   * @return The resulting matrix after stacking.
   * @throws IllegalArgumentException if the matrices have different numbers of columns.
   */
  def above(that: Matrix): Matrix = {
    // Check if both matrices have the same number of columns
    require(this.rows.head.size == that.rows.head.size, "Different numbers of columns")

    // Combine the rows of both matrices while maintaining the order
    val combinedRows = this.rows ++ that.rows

    // Create a new matrix from the combined rows and return
    Matrix(combinedRows)
  }


  /** COURSEWORK METHOD
   * Reduces all the columns of this matrix to a single column by applying an operator to corresponding elements in each row.
   *
   * @param op The operator function to be applied to corresponding elements.
   * @return The resulting matrix after reducing columns.
   * @throws IllegalArgumentException if the matrix is empty.
   */
  def reduceCols(op: (Int, Int) => Int): Matrix = {
    // Check if the matrix is non-empty
    require(rows.nonEmpty, "Matrix cannot be empty")

    // Apply the reduction operation to corresponding elements in each row and wrap the result in a single-column vector
    val reducedRows = rows.map(row => Vector(row.reduce(op)))

    // Create a new matrix from the reduced rows and return
    Matrix(reducedRows)
  }


} //class Matrix

object Matrix {
  /** A constructor for matrices */
  def apply(rows: Seq[Seq[Int]]) = new Matrix(rows)

  /** COURSEWORK METHOD
   * Generates an identity matrix of size n x n.
   *
   * @param n The dimension of the identity matrix.
   * @return An identity matrix of size n x n.
   * @throws IllegalArgumentException if n is not greater than 0.
   */
  def id(n: Int): Matrix = {
    // Ensure that n is greater than 0, otherwise, throw an IllegalArgumentException
    require(n > 0, "Dimension must be greater than 0")

    // Generate the rows of the identity matrix
    val identityRows = (1 to n).map { i =>
      // For each row, generate the elements
      (1 to n).map { j =>
        // If the current row index (i) is equal to the current column index (j),
        // set the element to 1 (indicating the diagonal), otherwise set it to 0.
        if (i == j) 1 else 0
      }.toVector // Convert the elements to a vector and return
    }.toVector // Convert the rows to a vector and return

    // Create a Matrix object from the generated rows and return it
    Matrix(identityRows)
  }


}