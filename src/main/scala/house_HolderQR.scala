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
import FloatingPointDesigns.FPArithmetic.FP_dDot_2
import chisel3._
import chisel3.aop.Select.When
import chisel3.tester._
import chisel3.tester.RawTester.test
import firrtl._




import scala.collection._

class house_HolderQR(row: Int,col: Int,bitwidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in_a = Input(Vec((row * col), UInt(bitwidth.W)))
    val out = Output(Vec((row * col), UInt(bitwidth.W)))
    val out_test = Output(UInt(bitwidth.W))
    val out_test2 = Output(UInt(bitwidth.W))
    val out_test3 = Output(UInt(bitwidth.W))
    val out_test4 = Output(UInt(bitwidth.W))
    val out_test5 = Output(UInt(bitwidth.W))
    val out_test6 = Output(UInt(bitwidth.W))
    val out_test7 = Output(UInt(bitwidth.W))
    val out_test8 = Output(UInt(bitwidth.W))
    val out_test9 = Output(UInt(bitwidth.W))
    val out_test10 = Output(UInt(bitwidth.W))
    val out_test11 = Output(UInt(bitwidth.W))
    //val in_enable = Input(Bool())
    //val out_enable = Output(Bool())
  })
  // val checker = RegInit(Vec((row * col) , UInt(bitwidth.W)))
  val holder = Reg(Vec((row), UInt(bitwidth.W)))
  val reflector = Reg(Vec((row), UInt(bitwidth.W)))
  val trailing = Reg(Vec((row), UInt(bitwidth.W)))
  val final_trailing = Reg(Vec((row), UInt(bitwidth.W)))
  val in_a_prime = Reg(Vec((row * col), UInt(bitwidth.W)))
  val in_b_prime = Reg(Vec((row * col), UInt(bitwidth.W)))
  //val test_1 = Reg(Uint(4.W))
  var n = col
  var m = row
  var kr, jr, k_sig, j_sig = 0
  var set = 0
  var count = 0
  var d1 = RegInit(0.U(bitwidth.W))
  var d1_prime = RegInit(0.U(bitwidth.W))
  var d2 = RegInit(0.U(bitwidth.W))
  var d3 = RegInit(0.U(bitwidth.W))
  var tk = RegInit(0.U(bitwidth.W))
  var d4 = RegInit(0.U(bitwidth.W))
  var d5 = RegInit(0.U(bitwidth.W))
  //var s1 = Reg(UInt(bitwidth.W))
  //val multiplier1 = Module(new FP_multiplier(bitwidth))
  val multiplier2 = Module(new FP_multiplier(bitwidth))
  val multiplier3 = Module(new FP_multiplier(bitwidth))
  val multiplier4 = Module(new FP_multiplier(bitwidth))
  val multiplier5 = Module(new FP_multiplier(bitwidth))
  //val adder1 = Module(new FP_adder(bitwidth))
  val adder2 = Module(new FP_adder(bitwidth))
  val adder3 = Module(new FP_adder(bitwidth))
  val adder4 = Module(new FP_adder(bitwidth))
  val dDot1 = Module(new FP_dDot_2(bitwidth,row))
  //val adder5 = Module(new FP_adder(bitwidth))
  val sqrt1 = Module(new FP_square_root(bitwidth))
  val divider1 = Module(new FP_divider(bitwidth))


  in_a_prime := io.in_a

  for (k <- 1 until n) {
    kr = k - 1
    k_sig = kr

    //hqr1
    for (b <- kr until m) {
      holder(count) := in_a_prime(k_sig)
      count = count + 1
      k_sig = k_sig + col
    }
    count = 0
    io.out_test := holder(0)
    //printf(p"d1 = $holder(b)")


    //hqr2
    for(b <- kr until m) {

      dDot1.io.in_a := holder(kr)
      dDot1.io.in_b := holder(kr)
    }
    d1 := dDot1.io.out_s
    //printf(p"d1 = $d1")

    /*
    for (b <- kr until m) {
      //d1 = holder(b) * holder(b) + d1
      multiplier1.io.in_a := holder(0)
      multiplier1.io.in_b := holder(0)
      //printf(p"holder = $holder(b)")
      adder1.io.in_a := multiplier1.io.out_s
      adder1.io.in_b := d1
      d1 := adder1.io.out_s
     //printf(p"d1 = $d1(b)")
    }



     */


    io.out_test2 := d1

    //hqr3
    sqrt1.io.in_a := d1
    d2 := sqrt1.io.out_s
    io.out_test3 := d2
    //printf(p"d2 = $d2(b)")

    //hqr4
    for (b <- kr + 1 until m) {
      reflector := holder
    }

    //hqr5
    adder2.io.in_a := holder(kr)
    adder2.io.in_b := d2
    reflector(kr) := adder2.io.out_s
    io.out_test4 := reflector(kr)
    //printf(p"d2 = $reflector(b)")

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
    io.out_test5 := d3


    //hqr7
    divider1.io.in_a := "b11000000000000000000000000000000".U
    divider1.io.in_b := d3
    tk := divider1.io.out_s


    io.out_test6 := tk

    //for (j <- k until n + 1) {
    var j = 1
    jr = j - 1
    j_sig = jr

    //hqr8

      for (b <- 0 until m) {
        trailing(b) := in_a_prime(j_sig)
        j_sig = j_sig + col
      }

    io.out_test7 := trailing(1)

    //hqr9
    for (b <- kr until m) {
      //d1 = holder(b) * holder(b) + d1
      multiplier3.io.in_a := trailing(b)
      multiplier3.io.in_b := reflector(b)
      adder4.io.in_a := multiplier3.io.out_s
      adder4.io.in_b := d4
      d4 := adder4.io.out_s
      //printf(p"trailing = $trailing(b)")
     // println(" ")
      //printf(p"reflector = $reflector(b)")

      //println(" ")
      //printf(p"holder = $d4(b)")
    }
    io.out_test8 := d4


    //hqr10
    multiplier4.io.in_a := d4
    multiplier4.io.in_b := tk
    d5 := multiplier4.io.out_s

    io.out_test9 := d5

    //hqr11
    for (b <- kr until m) {

      multiplier5.io.in_a := d5
      multiplier5.io.in_b := reflector(b)
      adder4.io.in_a := multiplier5.io.out_s
      adder4.io.in_b := trailing(b)
      final_trailing(b) := adder4.io.out_s
    }
    io.out_test10 := final_trailing(0)


    //hqr12
    j_sig = (col * (jr)) - 1
    if (j_sig == -1) {
      j_sig = 0
    }
    for (b <- kr until m) {
      //println(b)
      in_b_prime(j_sig) := final_trailing(b)
      j_sig = j_sig + col
    }
    count = 0
    io.out_test11 := in_b_prime(1)



  }




  io.out := in_b_prime
  //io.out_test := holder(0)
  }


class FP_dDot_2(bw:Int,level:Int) extends Module{
  val io = IO{new Bundle() {
    val in_a = Input(Vec((level), UInt(bw.W)))
    val in_b = Input(Vec((level), UInt(bw.W)))
    val out_s = Output(UInt(bw.W))
    val out_test = Output(UInt(bw.W))
  }}
  var d1 = RegInit(0.U(bw.W))
  var adder_math = level
  var adder_creator = 0
  var stages = 0
  var stages_2 = level
  var stage_pointer = 0
  var adder_count = 0
  var odd_or_even = false

  if(stages_2 % 2 != 0) {
    odd_or_even = true
  }

  // set up adders
  while (adder_math != 1){
    if(adder_math % 2 == 0){
      adder_math = adder_math /2

    }
    else if(adder_math == 1){

    }
    else{
      adder_math = adder_math/2 + 1

    }
    stages = stages + 1
    adder_creator = adder_math + adder_creator
    //println(adder_creator)
  }


// gets mulit and adders
  val multiply_layer = for(i <- 0 until level)yield{
    val multiply = Module(new FP_multiplier(bw)).io
    multiply
  }
  val adder_sequence = for (i <- 0 until adder_creator) yield {
    val adder = Module(new FP_adder(bw)).io
    adder
  }


//multplies the numbers
  for(i <- 0 until level){
    multiply_layer(i).in_a := io.in_a(i)
    multiply_layer(i).in_b := io.in_b(i)
    d1 := multiply_layer(i).out_s
  }
    io.out_test := d1


  if (odd_or_even){
    stages_2 = stages_2 - 1
  }

  //adders first level
  for(i <- 0 until stages_2 by 2) {
    adder_sequence(adder_count).in_a := multiply_layer(i).out_s
    adder_sequence(adder_count).in_b := multiply_layer(i+1).out_s
    adder_count = adder_count + 1
  }
  println(adder_count)
  if(odd_or_even){
    stages_2 = stages_2 + 1
    adder_sequence(adder_count).in_a := multiply_layer(stages_2-1).out_s
    adder_sequence(adder_count).in_b := 0.U
    adder_count = adder_count + 1
  }
if(adder_count > 1) {
  adder_sequence(adder_count).in_a := adder_sequence(adder_count - 1).out_s
  adder_sequence(adder_count).in_b := adder_sequence(adder_count - 2).out_s
  adder_count = adder_count+1
}
else{

}


println(adder_count)
    /*

  for(r <- 1 until stages) {


    if(stages_2 % 2 == 0) {
      stages_2 = stages_2 / 2
      odd_or_even = false
    }
    else if(stages_2 == 1) {
      stages_2 = 1
      odd_or_even = false
    }
    else {
      stages_2 = stages_2/2 + 1
      odd_or_even = true
    }




    if(odd_or_even){
    for(i <- 0 until stages_2 by 2){



    }


    }
    else{
      if(stages_2 == 1){


      }
      else{


      }
    }
  }




     */








  io.out_s := adder_sequence(adder_count-1).out_s
}


  object tester_1 {

    def main(args: Array[String]): Unit = {
      //test(new house_HolderQR(2, 2, 32)) { c =>
        //c.io.in_a(0).poke("b01000000100000000000000000000000".U)
        //c.io.in_a(1).poke("b00111111100000000000000000000000".U)
        //c.io.in_a(2).poke("b00111111100000000000000000000000".U)
        //c.io.in_a(3).poke("b01000000100000000000000000000000".U)
        //c.clock.step(12)
        //c.clock.step(2)
        //c.io.out(2).expect("b00000000000000000000000000000000".U)
        //c.io.out(1).expect("b01000000100000000000000000000000".U)
       // c.io.out_test.expect("b00111111100000000000000000000000".U)
        //c.clock.step(5)
        //c.io.out_test2.expect("b01000000000000000000000000000000".U)
        //c.clock.step(11)
        //c.io.out_test3.expect("b00111111101101010000010010000001".U)
        //c.clock.step(2)
        //c.io.out_test4.expect("b01000000000110101000001001000001".U)
        //c.clock.step(1)
        //c.io.out_test5.expect("b01000000110110101000001001000001".U)
        //c.io.out_test6.expect("b10111110100101011111011001000000".U)
        //c.clock.step(1)
        //c.io.out_test7.expect("b00111111100000000000000000000000".U)
        //c.io.out_test8.expect("b01000000010110101000001001000001".U)
        //c.clock.step(1)
        //c.io.out_test9.expect("b00111111100000000000000000000000".U)
        //c.clock.step(4)
        //c.io.out_test10.expect("b11000000011000100100010110100010".U)
        //c.clock.step(5)
        //c.io.out_test11.expect("b11000000011000100100010110100010".U)


        //NEXT TEST
        //c.clock.step(38)
        //c.io.out_test2.expect("b01000001100010000000000000000000".U)
        //c.clock.step(12)
        //c.io.out_test3.expect("b01000000100000111111000001101111".U)
        //c.io.out_test4.expect("b01000000101000111111000001101111".U)


      test(new FP_dDot_2(32, 2)) { c =>
        c.io.in_a(0).poke("b01000000100000000000000000000000".U)
        c.io.in_b(0).poke("b00111111100000000000000000000000".U)
        c.io.in_a(1).poke("b00111111100000000000000000000000".U)
        c.io.in_b(1).poke("b01000000100000000000000000000000".U)
        //c.io.in_a(2).poke("b00111111100000000000000000000000".U)
        //c.io.in_b(2).poke("b01000000100000000000000000000000".U)
        c.clock.step(3)
        c.io.out_s.expect("b01000001000000000000000000000000".U)
      }
      println("SUCCESS!!")


    }
  }




