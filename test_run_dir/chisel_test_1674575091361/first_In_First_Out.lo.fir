circuit first_In_First_Out :
  module first_In_First_Out :
    input clock : Clock
    input reset : UInt<1>
    input io_write_in_0 : UInt<2>
    input io_write_in_1 : UInt<2>
    output io_test : UInt<2>
    input io_read_enable : UInt<1>
    input io_write_enable : UInt<1>

    reg reg_array_h_0 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 703:24]
    reg reg_array_h_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 703:24]
    reg reg_array_h_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_2) @[house_HolderQR.scala 703:24]
    reg reg_array_h_3 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_3) @[house_HolderQR.scala 703:24]
    reg H_count : UInt<2>, clock with :
      reset => (UInt<1>("h0"), H_count) @[house_HolderQR.scala 705:24]
    reg T_count : UInt<2>, clock with :
      reset => (UInt<1>("h0"), T_count) @[house_HolderQR.scala 706:24]
    reg F_count : UInt<2>, clock with :
      reset => (UInt<1>("h0"), F_count) @[house_HolderQR.scala 707:24]
    node _H_count_T = add(H_count, UInt<1>("h1")) @[house_HolderQR.scala 711:22]
    node _H_count_T_1 = tail(_H_count_T, 1) @[house_HolderQR.scala 711:22]
    node _GEN_0 = mux(io_write_enable, _H_count_T_1, H_count) @[house_HolderQR.scala 710:25 711:11 705:24]
    io_test <= H_count @[house_HolderQR.scala 720:13]
    reg_array_h_0 <= reg_array_h_0 @[house_HolderQR.scala 703:24]
    reg_array_h_1 <= reg_array_h_1 @[house_HolderQR.scala 703:24]
    reg_array_h_2 <= reg_array_h_2 @[house_HolderQR.scala 703:24]
    reg_array_h_3 <= reg_array_h_3 @[house_HolderQR.scala 703:24]
    H_count <= mux(reset, UInt<2>("h0"), _GEN_0) @[house_HolderQR.scala 705:{24,24}]
    T_count <= mux(reset, UInt<2>("h0"), T_count) @[house_HolderQR.scala 706:{24,24,24}]
    F_count <= mux(reset, UInt<2>("h0"), F_count) @[house_HolderQR.scala 707:{24,24,24}]
