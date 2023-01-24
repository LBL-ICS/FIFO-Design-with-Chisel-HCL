
import scala.io.Source
import scala.math.sqrt
import java.io.PrintWriter
import scala.math.sqrt

class GoldenModel_QR_HouseHolder(row: 3, col: 3) {
  // defines the array, and the parameters
  var a, b, d = 0.0 //a for col, b for rows, d for dot product
  var n = col //col
  var m = row //row
  var p = 2 // processor?
  var myMatrix = Array.ofDim[Double](m, n)
  var holder = Array.ofDim[Double](m)
  var reflector = Array.ofDim[Double](m)
  var trailing = Array.ofDim[Double](m)
  var final_Trailing = Array.ofDim[Double](m)
  var tk = 0.0
  var kr, jr= 0


  // all labels with TEST_MATRIX have nothing to do with the algorithm and should be ignored, they are only meant for testing

  var d1, d2, d3, d4, d5 = 0.0

  // sets up the array for testing
  for (a <- 0 until m) {
    for (b <- 0 until n) {
      if(b == a){
        myMatrix(a)(b) = 4;
      }
      else {
        myMatrix(a)(b) = 1;
      }
    }
  }





  for (a <- 0 until m) {
   for (b <- 0 until n) {
     print(myMatrix(a)(b) + " ")
    }
    println(" ")
  }
  println("________________________________________________")
  for (k <- 1 until n) {
    //hqr1 get the next section x
    kr = k - 1




    for (b <- 0 until m) {
      holder(b) = myMatrix(b)(kr)
      //(holder(b))
    }
    b = 0;
    //hqr2 ddot product

    //for (d <- kr until m) {
      for (b <- kr until m) {
        d1 = holder(b) * holder(b) + d1
        //println(holder(b))
      }
   // }
    d = 0
    //print(d1 + " ")
    //hqr3
    d2 = sqrt(d1)
    //print(d2 + " ")
    //hqr4
    holder.copyToArray(reflector)
    //hqr5
    if (holder(kr) < 0) {
      reflector(kr) = holder(kr) + -d2
    }
    else {
      reflector(kr) = holder(kr) + d2
    }
    //print(reflector(0))

    //hqr6
    //for (d <- kr until m) {
      for (b <- kr until m) {
        d3 = reflector(b) * reflector(b) + d3
      }
    //}

    d = 0
    //hqr7
    if( d3 == 0){
      tk = 0
    }
    else {
      tk = -2 / d3
    }



    for (j <- k until n + 1 ) {
      jr = j - 1
      //hqr8
      for (b <- 0 until m) {
        trailing(b) = myMatrix(b)(jr)
        //print(trailing(b) + " ")
      }


      //hqr9
      //for (d <- jr until m) {
        for (b <- kr until m) {
          //yT times x
          d4 = trailing(b) * reflector(b) + d4
          //print(reflector(b) + " ")
        }
      //}
      d = 0
      //print(d4)
      //hqr10
      d5 = tk * d4
      //hqr11
      for (b <- kr until m) {
        final_Trailing(b) = d5 * reflector(b) + trailing(b)
        //print(final_Trailing(b) + " ")
      }
      //hqr12
      for (b <- kr until m) {
        myMatrix(b)(jr) = final_Trailing(b)
      }

    //reset all hold values
    d3 = 0
      d4 = 0


    }
    d1 = 0
    for (a <- 0 until m) {
      for (b <- 0 until n) {
        print(BigDecimal(myMatrix(a)(b)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble + " ")
      }
      println(" ")
    }

    println("________________________________________________")

  }

  for (a <- 0 until m) {
   for (b <- 0 until col) {
     print(BigDecimal(myMatrix(a)(b)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble + " ")
    //print(myMatrix(a)(b) + " ")
   }
   println("")
  }

}
