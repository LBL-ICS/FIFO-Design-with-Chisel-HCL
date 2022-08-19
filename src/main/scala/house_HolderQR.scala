import BasicDesigns.Arithmetic.{full_adder, full_subber, leadingOneDetector, shifter, twoscomplement}
import chisel3._
import chisel3.util._
import IEEEConversions.FPConvert._
import Chisel.{MuxLookup, log2Ceil, resetToBool}
import FloatingPointDesigns.FPArithmetic.FP_multiplier
import FloatingPointDesigns.FPArithmetic.FP_adder
import FloatingPointDesigns.FPArithmetic.FP_square_root
import FloatingPointDesigns.FPArithmetic.FP_subber
import FloatingPointDesigns.FPArithmetic.FP_divider
import chisel3._
import chisel3.aop.Select.When
import chisel3.tester._
import chisel3.tester.RawTester.test
import firrtl._




import scala.collection._

class house_HolderQR(row: Int,col: Int,bitwidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in_a = Input(Vec((row * col), UInt(bitwidth.W)))
    val out = Output(Vec((row), UInt(bitwidth.W)))
    val out_test = Output(UInt(bitwidth.W))
    //val in_enable = Input(Bool())
    //val out_enable = Output(Bool())
  })
  // val checker = RegInit(Vec((row * col) , UInt(bitwidth.W)))
  val holder = Reg(Vec((row), UInt(bitwidth.W)))
  val reflector = Reg(Vec((row), UInt(bitwidth.W)))
  val trailing = Reg(Vec((row), UInt(bitwidth.W)))
  //val final_trailing = Reg(Vec((row), UInt(bitwidth.W)))
  val in_a_prime = Reg(Vec((row * col), UInt(bitwidth.W)))
  //val test_1 = Reg(Uint(4.W))
  var n = col
  var m = row
  var kr, jr, k_sig, j_sig = 0
  var count = 0
  var d1 = RegInit(0.U(bitwidth.W))
  var d2 = RegInit(0.U(bitwidth.W))
  var d3 = RegInit(0.U(bitwidth.W))
  var tk = RegInit(0.U(bitwidth.W))
  var d4 = RegInit(0.U(bitwidth.W))
  var d5 = RegInit(0.U(bitwidth.W))
  //var s1 = Reg(UInt(bitwidth.W))
  //val multiplier1 = Module(new FP_multiplier(bitwidth))
  val multiplier2 = Module(new FP_multiplier(bitwidth))
  //val multiplier3 = Module(new FP_multiplier(bitwidth))
  //val multiplier4 = Module(new FP_multiplier(bitwidth))
  //val multiplier5 = Module(new FP_multiplier(bitwidth))
  //val adder1 = Module(new FP_adder(bitwidth))
  val adder2 = Module(new FP_adder(bitwidth))
  val adder3 = Module(new FP_adder(bitwidth))
  //val adder4 = Module(new FP_adder(bitwidth))
  //val adder5 = Module(new FP_adder(bitwidth))
  //val sqrt1 = Module(new FP_square_root(bitwidth))
  //val divider1 = Module(new FP_divider(bitwidth))


  in_a_prime := io.in_a


  for (k <- 1 until n) {
    kr = k - 1
    if (k == 1) {
      k_sig = k - 1
    }
    else {
      k_sig = col + k - 1
    }


    //hqr1
    for (b <- kr until m) {
      holder(count) := in_a_prime(k_sig)
      count = count + 1
      k_sig = k_sig + col
    }
    count = 0
    if (k == 1) {
      k_sig = k - 1
    }
    else {
      k_sig = col + k - 1
    }

    /*

    //hqr2
  for (b <- kr until m) {
    //d1 = holder(b) * holder(b) + d1
    multiplier1.io.in_a := holder(b)
    multiplier1.io.in_b := holder(b)
    //printf(p"holder = $holder(b)")
    adder1.io.in_a := multiplier1.io.out_s
    adder1.io.in_b := d1
    d1 := adder1.io.out_s
  }


    //hqr3
      sqrt1.io.in_a := d1
      d2 := sqrt1.io.out_s



*/
   // d2 = "b00111111100100100010110100001110".U
    //hqr4
    reflector := holder
    //hqr5
        adder2.io.in_a := holder(kr)
        adder2.io.in_b := d2
        reflector(kr) := adder2.io.out_s



    // hqr6

    for (b <- kr until m) {
      //d1 = holder(b) * holder(b) + d1
      multiplier2.io.in_a := reflector(b)
      multiplier2.io.in_b := reflector(b)
      //printf(p"holder = $holder(b)")
      adder3.io.in_a := multiplier2.io.out_s
      adder3.io.in_b := d3
      d3 := adder3.io.out_s
    }

/*
    //hqr7
    d3 = "b01000000110110101000001001000001".U
    divider1.io.in_a :="b11000000000000000000000000000000".U
    divider1.io.in_b := d3
    tk :=  divider1.io.out_s


    for (j <- k until n + 1) {
      jr = j - 1

      if (j == 1) {
        j_sig = j - 1
      }
      else {
        j_sig = col + j - 1
      }

      //hqr8
      for (b <- jr to m by n) {
        trailing(count) := in_a_prime(j_sig)
        count = count + 1
        j_sig = j_sig + col
      }

      if (j == 1) {
        j_sig = j - 1
      }
      else {
        j_sig = col + j - 1
      }
      count = 0

        //hqr9
        for (b <- 0 until m) {
          //d1 = holder(b) * holder(b) + d1
          multiplier3.io.in_a := trailing(b)
          multiplier3.io.in_b := reflector(b)
          adder4.io.in_a := multiplier3.io.out_s
          adder4.io.in_b := d4
          d4 := adder4.io.out_s
        }
        //hqr10
        multiplier4.io.in_a := d4
        multiplier4.io.in_b := tk
        d5 := multiplier4.io.out_s

        //hqr11
        for (b <- jr until m) {
          multiplier5.io.in_a := d5
          multiplier5.io.in_b := reflector(b)
          adder4.io.in_a := multiplier5.io.out_s
          adder4.io.in_b := trailing(b)
          final_trailing(b) := adder4.io.out_s
        }

        //hqr12
        for (b <- jr to m) {
          in_a_prime(j_sig) := final_trailing(count)
          count = count + 1
        }
        count = 0
     // }

    }



 */


    io.out := holder
    io.out_test := tk

  }
}

  object tester_1 {
    var row = 1;
    var col = 1;

    def main(args: Array[String]): Unit = {
      test(new house_HolderQR(2, 2, 32)) { c =>
        c.io.in_a(0).poke("b00111111100000000000000000000000".U)
        c.io.in_a(1).poke("b00111111100000000000000000000000".U)
        c.io.in_a(2).poke("b01000000100000000000000000000000".U)
        c.io.in_a(3).poke("b01000000100000000000000000000000".U)
        c.clock.step(2)
        c.io.out(0).expect("b00111111100000000000000000000000".U)
        c.io.out(1).expect("b01000000100000000000000000000000".U)
        //c.io.out_test.expect("b01000000100000000000000000000000".U)


      }
      println("SUCCESS!!")


    }
  }




