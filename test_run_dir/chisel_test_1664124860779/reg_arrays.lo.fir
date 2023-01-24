circuit reg_arrays :
  module reg_arrays :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a_0 : UInt<32>
    input io_in_a_1 : UInt<32>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    reg reg_array_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 630:24]
    reg reg_array_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 630:24]
    reg reg_array_h_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_2) @[house_HolderQR.scala 630:24]
    reg reg_array_h_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_3) @[house_HolderQR.scala 630:24]
    reg reg_array_h_4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_4) @[house_HolderQR.scala 630:24]
    reg reg_array_h_5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_5) @[house_HolderQR.scala 630:24]
    reg reg_array_h_6 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_6) @[house_HolderQR.scala 630:24]
    reg reg_array_h_7 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_7) @[house_HolderQR.scala 630:24]
    reg reg_array_h_8 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_8) @[house_HolderQR.scala 630:24]
    reg reg_array_h_9 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_9) @[house_HolderQR.scala 630:24]
    reg reg_array_h_10 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_10) @[house_HolderQR.scala 630:24]
    reg reg_array_h_11 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_11) @[house_HolderQR.scala 630:24]
    reg reg_array_h_12 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_12) @[house_HolderQR.scala 630:24]
    reg reg_array_h_13 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_13) @[house_HolderQR.scala 630:24]
    io_out_s_0 <= reg_array_h_1 @[house_HolderQR.scala 645:15]
    io_out_s_1 <= io_in_a_1 @[house_HolderQR.scala 647:15]
    reg_array_h_0 <= io_in_a_0 @[house_HolderQR.scala 633:18]
    reg_array_h_1 <= reg_array_h_0 @[house_HolderQR.scala 634:18]
    reg_array_h_2 <= reg_array_h_1 @[house_HolderQR.scala 635:18]
    reg_array_h_3 <= reg_array_h_2 @[house_HolderQR.scala 636:18]
    reg_array_h_4 <= reg_array_h_3 @[house_HolderQR.scala 637:18]
    reg_array_h_5 <= reg_array_h_4 @[house_HolderQR.scala 638:18]
    reg_array_h_6 <= reg_array_h_6 @[house_HolderQR.scala 630:24]
    reg_array_h_7 <= reg_array_h_6 @[house_HolderQR.scala 639:18]
    reg_array_h_8 <= reg_array_h_7 @[house_HolderQR.scala 640:18]
    reg_array_h_9 <= reg_array_h_8 @[house_HolderQR.scala 641:18]
    reg_array_h_10 <= reg_array_h_9 @[house_HolderQR.scala 642:19]
    reg_array_h_11 <= reg_array_h_10 @[house_HolderQR.scala 643:19]
    reg_array_h_12 <= reg_array_h_11 @[house_HolderQR.scala 644:19]
    reg_array_h_13 <= reg_array_h_13 @[house_HolderQR.scala 630:24]
