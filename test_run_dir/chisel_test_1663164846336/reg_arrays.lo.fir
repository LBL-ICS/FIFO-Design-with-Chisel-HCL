circuit reg_arrays :
  module reg_arrays :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a_0 : UInt<32>
    input io_in_a_1 : UInt<32>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    reg reg_array_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 571:24]
    reg reg_array_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 571:24]
    reg reg_array_h_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_2) @[house_HolderQR.scala 571:24]
    reg reg_array_h_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_3) @[house_HolderQR.scala 571:24]
    reg reg_array_h_4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_4) @[house_HolderQR.scala 571:24]
    reg reg_array_h_5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_5) @[house_HolderQR.scala 571:24]
    reg reg_array_h_6 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_6) @[house_HolderQR.scala 571:24]
    reg reg_array_h_7 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_7) @[house_HolderQR.scala 571:24]
    reg reg_array_h_8 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_8) @[house_HolderQR.scala 571:24]
    io_out_s_0 <= reg_array_h_3 @[house_HolderQR.scala 576:15]
    io_out_s_1 <= reg_array_h_7 @[house_HolderQR.scala 581:15]
    reg_array_h_0 <= io_in_a_0 @[house_HolderQR.scala 572:18]
    reg_array_h_1 <= reg_array_h_0 @[house_HolderQR.scala 573:18]
    reg_array_h_2 <= reg_array_h_1 @[house_HolderQR.scala 574:18]
    reg_array_h_3 <= reg_array_h_2 @[house_HolderQR.scala 575:18]
    reg_array_h_4 <= io_in_a_1 @[house_HolderQR.scala 577:18]
    reg_array_h_5 <= reg_array_h_4 @[house_HolderQR.scala 578:18]
    reg_array_h_6 <= reg_array_h_5 @[house_HolderQR.scala 579:18]
    reg_array_h_7 <= reg_array_h_6 @[house_HolderQR.scala 580:18]
    reg_array_h_8 <= reg_array_h_8 @[house_HolderQR.scala 571:24]
