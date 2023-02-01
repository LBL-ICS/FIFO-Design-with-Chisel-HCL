
import chisel3._
import chisel3.util._
import Chisel.{MuxLookup, log2Ceil, resetToBool}
import chisel3._
import chisel3.aop.Select.When
import chisel3.tester._
import chisel3.tester.RawTester.test
import firrtl.Utils.True
import firrtl._

import scala.collection._


class first_In_First_Out(Size_of_Fifo:Int, Depth:Int, bw:Int)extends Module{
  val io = IO{new Bundle() {
    val write_in = Input(Vec((Depth), UInt(bw.W)))
    val read_out = Output(Vec((Depth), UInt(bw.W)))
    //val test = Output(UInt(bw.W))
    //val test_1 = Output(Bool())
    val read_enable = Input(Bool())
    val write_enable = Input(Bool())
  }}
  val max = Size_of_Fifo
  val total = Size_of_Fifo * Depth
  val reg_array_Fifo = Reg(Vec((total), UInt(bw.W)))
  //gives 3 counters, each with a max bitwidth of the size of the Fifo
  val H_count = RegInit(0.U(bw.W))
  val T_count = RegInit(0.U(bw.W))
  val F_count = RegInit(0.U((max).W))
  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)
  when(F_count === 0.U){
    emptyReg := true.B
  }.otherwise{
    emptyReg := false.B
  }
  when(F_count === (max).U){
    fullReg := true.B
  }.otherwise{
    fullReg := false.B
  }


  when(io.write_enable && (fullReg =/= true.B) && !io.read_enable) {
  F_count := F_count + 1.U
      H_count := H_count + 1.U
    for(i <- 0 to (Depth - 1)){
      reg_array_Fifo((H_count% max.U)*Depth.U+i.U) := io.write_in(i)
      //io.read_out(i) := io.read_out(i)
    }
    for(i <- 0 to (Depth - 1)){
      io.read_out(i) := 0.U
    }
  }.elsewhen(io.read_enable && (emptyReg =/= true.B) && !io.write_enable) {
    F_count := F_count - 1.U

      T_count := T_count + 1.U
    for(i <- 0 to (Depth - 1)){
      io.read_out(i) := reg_array_Fifo(((T_count-1.U) % max.U)*Depth.U+i.U)
    }
  }.elsewhen(io.read_enable && (emptyReg =/= true.B) && io.write_enable && (fullReg =/= true.B)) {
    T_count := T_count + 1.U
    H_count := H_count + 1.U
    for(i <- 0 to (Depth - 1)){
      reg_array_Fifo((H_count% max.U)*Depth.U+i.U) := io.write_in(i)
      //io.read_out(i) := io.read_out(i)
    }
    for(i <- 0 to (Depth - 1)){
      io.read_out(i) := reg_array_Fifo(((T_count-1.U) % max.U)*Depth.U+i.U)
    }

  }.otherwise {
    for(i <- 0 to (Depth - 1)){
      io.read_out(i) := 0.U
    }
  }
    //io.test := reg_array_Fifo(0)
    //io.test_1 := fullReg



    }





  object tester_1 extends App {


    test(new first_In_First_Out(5,5,32)) { c =>
      c.io.write_in(0).poke(1.U)
      c.io.write_in(1).poke(1.U)
      c.io.write_in(2).poke(1.U)
      c.io.write_in(3).poke(1.U)
      c.io.write_in(4).poke(1.U)

      c.io.write_enable.poke(true.asBool)
      c.clock.step(1)

      c.io.write_in(0).poke(2.U)
      c.io.write_in(1).poke(2.U)
      c.io.write_in(2).poke(2.U)
      c.io.write_in(3).poke(2.U)
      c.io.write_in(4).poke(2.U)
      c.clock.step(1)
      c.io.write_in(0).poke(3.U)
      c.io.write_in(1).poke(3.U)
      c.io.write_in(2).poke(3.U)
      c.io.write_in(3).poke(3.U)
      c.io.write_in(4).poke(3.U)
      c.clock.step(1)
      c.io.write_in(0).poke(4.U)
      c.io.write_in(1).poke(4.U)
      c.io.write_in(2).poke(4.U)
      c.io.write_in(3).poke(4.U)
      c.io.write_in(4).poke(4.U)
      c.clock.step(1)


      c.io.read_enable.poke(true.asBool)
      c.clock.step(1)
      c.io.read_out(0).expect(1.U)
      c.clock.step(1)
      c.io.read_out(0).expect(2.U)
      c.clock.step(1)
      c.io.read_out(0).expect(3.U)
      c.clock.step(1)
      c.io.read_out(0).expect(4.U)




    }
    println("SUCCESS!!")
  //println(new(chisel3.stage.ChiselStage).emitVerilog(new axpy(32,256)))

//}

}




