import BasicDesigns.Arithmetic.{full_adder, full_subber, leadingOneDetector, shifter, twoscomplement}
import chisel3._
import chisel3.util._
import IEEEConversions.FPConvert._
import Chisel.{MuxLookup, log2Ceil, resetToBool}
import FloatingPointDesigns.FPArithmetic
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
import firrtl.Utils.True
import firrtl._

import scala.collection._

class house_HolderQR(row: Int, bitwidth: Int) extends Module {
  /*
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

   */


  val io = IO(new Bundle() {
    val col_1 = Input(Vec((row), UInt(bitwidth.W)))
    val reg_array = Input(Vec((row), UInt(bitwidth.W)))
    val reg_arrays = Input(Vec((row), UInt(bitwidth.W)))
    val Tk = Input(Bool())
    val Vk = Input(Bool())
    val Tr = Input(Bool())
    val h_count = Input(UInt(bitwidth.W))
    val out_test = Output(UInt(bitwidth.W))
    val out_s = Output(Vec((row), UInt(bitwidth.W)))
  })
  val reg_array_h = Reg(Vec((row), UInt(bitwidth.W)))
  val reg_arrays_h = Reg(Vec((row), UInt(bitwidth.W)))
  val reg_arrays_h_b = Reg(Vec((row), UInt(bitwidth.W)))
  val reg_1 = Reg(UInt(bitwidth.W))
  val reg_2 = Reg(Vec((row), UInt(bitwidth.W)))
  val col_1 = Reg(Vec((row), UInt(bitwidth.W)))
  val tk = Reg(UInt(bitwidth.W))
  var level = 0
  var loop = 0
  //val reg_arrays_h = Reg(Vec((row), UInt(bitwidth.W)))
  val w0 = Wire(Vec((row), UInt(bitwidth.W)))
  val w1 = Wire(Vec((row), UInt(bitwidth.W)))
  //val d1 = Wire(UInt(bitwidth.W))
  //val d2 = Wire(UInt(bitwidth.W))
  val dDot = Module(new FP_dDot_2(bitwidth, row))
  val test = Reg(UInt(bitwidth.W))
  //val sqrt = Module(new FP_square_root(bitwidth))
  //val sign = Module(new sign_Calculator(bitwidth))

  reg_array_h := io.reg_array
  reg_arrays_h := io.reg_arrays




  //Mux((reg_h === 0.U),(loop = 0),(loop = 1))


    //Mux 1 for ddot
    when(io.Tk === true.B) {
      for (i <- 0 until row) {
        //Starts testing the clock
        when(i.U < io.h_count) {
          w0(i) := 0.U(i)
          w1(i) := 0.U(i)
        }.elsewhen(i.U > io.h_count) {
          w0(i) := io.col_1(i)
          w1(i) := io.col_1(i)
        }.otherwise {
          w0(i) := io.col_1(i)
          w1(i) := io.col_1(i)
        }

      }
      // TK flag
    }.elsewhen(io.Vk === true.B) {
      //VK flag
      for (i <- 0 until row) {
      when(i.U < io.h_count) {
        w0(i) := 0.U(i)
        w1(i) := 0.U(i)
      }.elsewhen(i.U > io.h_count) {
        w0(i) := reg_array_h(i)
        w1(i) := reg_array_h(i)
      }.otherwise {
        w0(i) := reg_array_h(i)
        w1(i) := reg_array_h(i)
      }
      }
    }
      .elsewhen(io.Tr === true.B) {
        //tr flag
        for (i <- 0 until row) {
          when(i.U < io.h_count) {
            reg_2 (i) := 0.U(i)
            reg_2 (i) := 0.U(i)
          }.elsewhen(i.U > io.h_count) {
            reg_2 (i) := reg_array_h(i)
            reg_2 (i) := reg_array_h(i)
          }.otherwise {
            reg_2 (i) := reg_array_h(i)
            reg_2 (i) := reg_array_h(i)
          }
        }
        //zeros vk
        for (i <- 0 until row) {
          when(i.U < io.h_count) {
            w0(i) := 0.U(i)
            w1(i) := 0.U(i)
          }.elsewhen(i.U > io.h_count) {
            w0(i) := reg_array_h(i)
            w1(i) := reg_arrays_h(i)
          }.otherwise {
            w0(i) := reg_array_h(i)
            w1(i) := reg_arrays_h(i)
          }
        }
        //sets the reg_arrays_col
      }
      .otherwise {
        for (i <- 0 until row) {
          w0(i) := 0.U
          w1(i) := 0.U
        }
      }

  //ddot

  dDot.io.in_a := w0
  dDot.io.in_b := w1

  //mux 2 after ddot
  when(io.Tk === true.B) {
    // defines first part
    val d1 = Wire(UInt(bitwidth.W))
    val d2 = Wire(UInt(bitwidth.W))
    val sqrt = Module(new FP_square_root(bitwidth))
    val sign = Module(new sign_Calculator(bitwidth))
    d1 := dDot.io.out_s
    //square root
    sqrt.io.in_a := d1
    d2 := sqrt.io.out_s
    //sign
    //gets the correct Vk value
    //test := reg_1
    sign.io.in_a := reg_array_h(io.h_count)
    sign.io.in_b := d2
    reg_1 := sign.io.out_s
  }.elsewhen(io.Vk === true.B) {
    val divide = Module(new FP_divider(bitwidth))
    divide.io.in_a := "b11000000000000000000000000000000".U
    divide.io.in_b := dDot.io.out_s
    tk := divide.io.out_s
  }.elsewhen(io.Tr === true.B) {
    val reg_array = Module(new reg_arrays(bitwidth, row))
    reg_array.io.in_a := reg_arrays_h
    // TESTER
    val d5_M = Module(new FP_multiplier(bitwidth))
    val axpy = Module(new axpy(bitwidth, row))
    test := reg_arrays_h_b(2)

    d5_M.io.in_a := tk
    d5_M.io.in_b := dDot.io.out_s
    axpy.io.in_a := d5_M.io.out_s
    axpy.io.in_b := reg_2
    axpy.io.in_c := reg_array.io.out_s
    reg_arrays_h_b := axpy.io.out_s
  }


  when(io.Vk === true.B) {
    reg_array_h(io.h_count) := reg_1
  }
  when(io.Tr === true.B) {
    reg_array_h(io.h_count) := reg_1
  }
  io.out_test := test
  io.out_s := w0
}


class FP_dDot_2(bw:Int,level:Int) extends Module{
  val io = IO{new Bundle() {
    val in_a = Input(Vec((level), UInt(bw.W)))
    val in_b = Input(Vec((level), UInt(bw.W)))
    val out_s = Output(UInt(bw.W))
  }}
  var adder_math = level
  var adder_creator = 0
  var stages = 0
  var stages_2 = level
  var adder_count = 0
  var odd_or_even = false
  var total_adders = 0
  var p_adders = 0

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
    //println(adder_math + "Math")
    stages = stages + 1
    adder_creator = adder_math + adder_creator
  }
//println(adder_creator + "CURRENT ADDERS")

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
  }


  //println(stages_2)
  if (odd_or_even){
    stages_2 = stages_2 - 1
  }

  //adders first level
  for(i <- 0 until stages_2 by 2) {
    adder_sequence(adder_count).in_a := multiply_layer(i).out_s
    adder_sequence(adder_count).in_b := multiply_layer(i+1).out_s
    adder_count = adder_count + 1
  }
  //println(adder_count)
  if(odd_or_even){
    adder_sequence(adder_count).in_a := multiply_layer(stages_2).out_s
    adder_sequence(adder_count).in_b := 0.U
    adder_count = adder_count + 1
    stages_2 = stages_2 + 2
  }
  //println(adder_count + " Input Adder")
  //println(stages_2 + " Input stage")
  //println(stages + " Input loop")


  for(r <- 1 until stages) {
    if (stages_2 % 2 == 0) {
      stages_2 = stages_2 / 2
      odd_or_even = false
    }
    else if (stages_2 == 1 ) {
      stages_2 = 0
      odd_or_even = false
    }

    else if(stages_2/2 == 1){
      if(r+1 == stages) {
        stages_2 = 1
        odd_or_even = false
      }
      else {
        stages_2 = stages_2 / 2
        odd_or_even = true
      }
    }
    else {
      stages_2 = stages_2 / 2
      odd_or_even = true
    }
    //println(stages_2 + " Recur Stage")
    total_adders = stages_2 + total_adders
    //println(odd_or_even)
    //println(stages_2)
    //println(total_adders)
    //println(p_adders +"P")
    //println(total_adders + "T")
    //println(total_adders)

    if (odd_or_even){
      total_adders= total_adders - 1
    }

    //println(total_adders + "T")
      for (i <- p_adders until total_adders by 2){
       // println(adder_count)
        //println(i + "P")
        adder_sequence(adder_count).in_a := adder_sequence(i).out_s
        if(i + 1 >= total_adders){
          if(stages_2 != 1){
            adder_sequence(adder_count).in_b := 0.U
            stages_2 = stages_2 + 1
          }
          else{
            adder_sequence(adder_count).in_b := adder_sequence(i + 1).out_s
          }
        }else {
          adder_sequence(adder_count).in_b := adder_sequence(i + 1).out_s
        }
        adder_count = adder_count + 1
    }
    //println(adder_count)
    if(odd_or_even){
      adder_sequence(adder_count).in_a := adder_sequence(total_adders).out_s
      adder_sequence(adder_count).in_b := 0.U
      adder_count = adder_count + 1
      total_adders = total_adders + 2
      //stages_2 = stages_2 + 1
    }
    p_adders = total_adders
    //println(stages_2 + "After Stage")
    //println("________________________________")
  }
//adder - 1 for overflow
  io.out_s := adder_sequence(adder_count-1).out_s
}
class sign_Calculator(bw:Int)extends Module{
  val io = IO{new Bundle() {
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val out_s = Output(UInt(bw.W))
  }}
  val adder = Module(new FP_adder(bw)).io
  val subber = Module(new FP_subber(bw)).io
  // loads up both hardware

  adder.in_a := io.in_a
  adder.in_b := io.in_b
  subber.in_a := io.in_a
  subber.in_b := io.in_b
// checks sign bit
  when(io.in_a >= "b10000000000000000000000000000000".U){
    io.out_s := subber.out_s
  }
    .otherwise {
      io.out_s := adder.out_s
    }




}
class house_Divider(bw:Int)extends Module {
  val io = IO {
    new Bundle() {
      val in_a = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    }
  }
  val divide = Module(new FP_divider(bw)).io
  divide.in_a := "b11000000000000000000000000000000".U
  divide.in_b := io.in_a
  io.out_s := divide.out_s
}
class axpy(bw:Int,level:Int)extends Module {
  val io = IO {
    new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(Vec((level), UInt(bw.W)))
      val in_c = Input(Vec((level), UInt(bw.W)))
      val out_s = Output(Vec((level), UInt(bw.W)))
    }
  }

  val multiply_layer = for(i <- 0 until level)yield{
    val multiply = Module(new FP_multiplier(bw)).io
    multiply
  }
  val adder_layer = for (i <- 0 until level) yield {
    val adder = Module(new FP_adder(bw)).io
    adder
  }

  for(i <- 0 until level){
    multiply_layer(i).in_a := io.in_a
    multiply_layer(i).in_b := io.in_b(i)
  }
  for(i <- 0 until level){
    adder_layer(i).in_a :=  multiply_layer(i).out_s
    adder_layer(i).in_b := io.in_c(i)
  }


  for(i <- 0 until level){
io.out_s(i) := adder_layer(i).out_s
  }
}

class reg_arrays(bw:Int,level:Int)extends Module {
  val io = IO {
    new Bundle() {
      val in_a = Input(Vec((level), UInt(bw.W)))
      val out_s = Output(Vec((level), UInt(bw.W)))
    }
  }
  val reg_array_h = Reg(Vec(14, UInt(bw.W)))


  reg_array_h(0) := io.in_a(0)
  reg_array_h(1) := reg_array_h(0)
  reg_array_h(2) := reg_array_h(1)
  reg_array_h(3) := reg_array_h(2)
  reg_array_h(4) := reg_array_h(3)
  reg_array_h(5) := reg_array_h(4)
  reg_array_h(7) := reg_array_h(6)
  reg_array_h(8) := reg_array_h(7)
  reg_array_h(9) := reg_array_h(8)
  reg_array_h(10) := reg_array_h(9)
  reg_array_h(11) := reg_array_h(10)
  reg_array_h(12) := reg_array_h(11)
  io.out_s(0) := reg_array_h(5)
for(i <- 1 until level) {
  io.out_s(i) := io.in_a(i)
}


}
/*
class TEST(bw:Int,level:Int)extends Module {
  val io = IO {
    new Bundle() {
      val in_a = Input(Vec((level), UInt(bw.W)))
      val h_count = Input(UInt(bw.W))
      val out_s = Output(Vec((level), UInt(bw.W)))
    }
  }

  val reg_2 = Reg(Vec((level), UInt(bw.W)))
    for(i <- 0 until 4){
      when(io.h_count === i.U){
          for(r <- 0 until i){
            reg_2(r) := 0.U
          }
      }
        .otherwise{
          reg_2(i) := io.in_a(i)
        }

    }

io.out_s := reg_2



}



 */

  object tester_1 extends App {


    test(new reg_arrays(32, 2)) { c =>
      c.io.in_a(0).poke("b00111111100000000000000000000000".U)
      c.io.in_a(1).poke("b00111111100000001000000000000000".U)
      c.clock.step(12)
      c.io.out_s(0).expect("b00111111100000000000000000000000".U)

    //println(new(chisel3.stage.ChiselStage).emitVerilog(new sign_Calculator(32)))
    //def main(args: Array[String]): Unit = {
/*
        test(new house_HolderQR(3, 32)) { c =>
          c.io.col_1(0).poke("b00111111100000000000000000000000".U)
          c.io.col_1(1).poke("b00111111100000000000000000000000".U)
          c.io.col_1(2).poke("b00111111100000000000000000000000".U)
          c.io.h_count.poke(0.U)
          c.io.reg_array(0).poke("b00111111100000000000000000000000".U)
          c.io.reg_array(1).poke("b00111111100000000000000000000000".U)
          c.io.reg_array(2).poke("b00111111100000000000000000000000".U)
          c.io.Tk.poke(true.B)
          c.io.Vk.poke(false.B)
          c.io.Tr.poke(false.B)
          c.clock.step(14)
          //c.io.out_test.expect("b00111111100000000000000000000000".U)


c.io.Tk.poke(false.B)
c.io.Vk.poke(true.B)
c.io.Tr.poke(false.B)
c.clock.step(12)
           //c.io.out_test.expect("b00111111100000000000000000000000".U)

c.io.Tk.poke(false.B)
c.io.Vk.poke(false.B)
c.io.Tr.poke(true.B)
c.clock.step(1)
c.io.reg_arrays(0).poke("b00111111100000000000000000000000".U)
c.io.reg_arrays(1).poke("b00111111100000000000000000000000".U)
c.io.reg_arrays(2).poke("b00111111100000000000000000000000".U)
c.clock.step(1)
c.io.reg_arrays(0).poke("b00111111100000000000000000000000".U)
c.io.reg_arrays(1).poke("b01000000100000000000000000000000".U)
c.io.reg_arrays(2).poke("b00111111100000000000000000000000".U)
c.clock.step(8)
// c.io.out_s(0).expect("b01000000100000000000000000000000".U)
//c.io.out_test.expect("b00111111100000000000000000000000".U)
//c.io.in_a(2).poke("b00111111100000000000000000000000".U)
//c.io.in_a(3).poke("b01000000100000000000000000000000".U)




 */


//test(new FP_dDot_2(32, 2)) { c =>
//c.io.in_a(0).poke("b00111111100000000000000000000000".U)
// c.io.in_b(0).poke("b01000000100000000000000000000000".U)
/*
c.io.in_a(1).poke("b00111111100000000000000000000000".U)
c.io.in_b(1).poke("b01000000100000000000000000000000".U)

c.io.in_a(2).poke("b01000000100000000000000000000000".U)
c.io.in_b(2).poke("b00111111100000000000000000000000".U)

c.io.in_a(3).poke("b00111111100000000000000000000000".U)
c.io.in_b(3).poke("b01000000100000000000000000000000".U)

c.io.in_a(4).poke("b00111111100000000000000000000000".U)
c.io.in_b(4).poke("b01000000100000000000000000000000".U)

c.io.in_a(5).poke("b00111111100000000000000000000000".U)
c.io.in_b(5).poke("b01000000100000000000000000000000".U)

c.io.in_a(6).poke("b00111111100000000000000000000000".U)
c.io.in_b(6).poke("b01000000100000000000000000000000".U)

c.io.in_a(7).poke("b00111111100000000000000000000000".U)
c.io.in_b(7).poke("b01000000100000000000000000000000".U)

c.io.in_b(8).poke("b01000000100000000000000000000000".U)
c.io.in_a(8).poke("b00111111100000000000000000000000".U)

c.io.in_b(9).poke("b01000000100000000000000000000000".U)
c.io.in_a(9).poke("b00111111100000000000000000000000".U)

c.io.in_b(10).poke("b01000000100000000000000000000000".U)
c.io.in_a(10).poke("b00111111100000000000000000000000".U)

c.io.in_b(11).poke("b01000000100000000000000000000000".U)
c.io.in_a(11).poke("b00111111100000000000000000000000".U)


c.io.in_a(12).poke("b00111111100000000000000000000000".U)
c.io.in_b(12).poke("b01000000100000000000000000000000".U)

//c.io.in_a(13).poke("b01000000100000000000000000000000".U)
//c.io.in_b(13).poke("b00111111100000000000000000000000".U)

//c.io.in_a(14).poke("b00111111100000000000000000000000".U)
//c.io.in_b(14).poke("b01000000100000000000000000000000".U)

//c.io.in_a(15).poke("b01000000100000000000000000000000".U)
//c.io.in_b(15).poke("b00111111100000000000000000000000".U)

//c.io.in_a(16).poke("b00111111100000000000000000000000".U)
//c.io.in_b(16).poke("b01000000100000000000000000000000".U)

//c.io.in_a(17).poke("b00111111100000000000000000000000".U)
//c.io.in_b(17).poke("b01000000100000000000000000000000".U)

//c.io.in_a(18).poke("b00111111100000000000000000000000".U)
//c.io.in_b(18).poke("b01000000100000000000000000000000".U)

//c.io.in_a(19).poke("b00111111100000000000000000000000".U)
//c.io.in_b(19).poke("b01000000100000000000000000000000".U)
//c.io.in_a(20).poke("b00111111100000000000000000000000".U)
//c.io.in_b(20).poke("b01000000100000000000000000000000".U)
//c.io.in_b(21).poke("b01000000100000000000000000000000".U)
//c.io.in_a(21).poke("b00111111100000000000000000000000".U)
//c.io.in_b(22).poke("b01000000100000000000000000000000".U)
//c.io.in_a(22).poke("b00111111100000000000000000000000".U)
//c.io.in_b(23).poke("b01000000100000000000000000000000".U)
//c.io.in_a(23).poke("b00111111100000000000000000000000".U)


*/
//  c.clock.step(10)
// c.io.out_s.expect("b01000001000000000000000000000000".U)
//}

// test(new house_Divider(32)) { c =>
// c.io.in_a.poke("b01000001100101111000110111101101".U)
//  c.clock.step(6)
// c.io.out_s.expect("b01000001000000000000000000000000".U)
//}
/*
test(new axpy(32,2)) { c =>
c.io.in_a.poke("b01000001100101111000110111101101".U)
c.io.in_b(0).poke("b11000001100101111000110111101101".U)
c.io.in_b(1).poke("b01000001000100000000000000000000".U)
c.io.in_c(0).poke("b01000001100101111000110111101101".U)
c.io.in_c(1).poke("b01000001100101111000110111101101".U)



c.clock.step(2)

c.io.out_s(0).expect("b01000001000000000000000000000000".U)
*/

//}
println("SUCCESS!!")


//}
}
}




