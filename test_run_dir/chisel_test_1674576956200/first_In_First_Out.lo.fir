circuit first_In_First_Out :
  module first_In_First_Out :
    input clock : Clock
    input reset : UInt<1>
    input io_write_in_0 : UInt<2>
    input io_write_in_1 : UInt<2>
    output io_test : UInt<2>
    output io_test_1 : UInt<1>
    input io_read_enable : UInt<1>
    input io_write_enable : UInt<1>

    reg reg_array_h_0 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 704:24]
    reg reg_array_h_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 704:24]
    reg reg_array_h_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_2) @[house_HolderQR.scala 704:24]
    reg reg_array_h_3 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_3) @[house_HolderQR.scala 704:24]
    reg H_count : UInt<1>, clock with :
      reset => (UInt<1>("h0"), H_count) @[house_HolderQR.scala 706:24]
    reg T_count : UInt<1>, clock with :
      reset => (UInt<1>("h0"), T_count) @[house_HolderQR.scala 707:24]
    reg F_count : UInt<2>, clock with :
      reset => (UInt<1>("h0"), F_count) @[house_HolderQR.scala 708:24]
    reg emptyReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), emptyReg) @[house_HolderQR.scala 709:25]
    reg fullReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), fullReg) @[house_HolderQR.scala 710:24]
    node _T = eq(F_count, UInt<1>("h0")) @[house_HolderQR.scala 712:16]
    node _GEN_0 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[house_HolderQR.scala 712:24 713:14 715:14]
    node _T_1 = eq(F_count, UInt<2>("h2")) @[house_HolderQR.scala 718:16]
    node _GEN_1 = mux(_T_1, UInt<1>("h1"), UInt<1>("h0")) @[house_HolderQR.scala 718:28 719:13 721:13]
    node _T_2 = neq(fullReg, UInt<1>("h1")) @[house_HolderQR.scala 724:36]
    node _T_3 = and(io_write_enable, _T_2) @[house_HolderQR.scala 724:24]
    node _F_count_T = add(F_count, UInt<2>("h2")) @[house_HolderQR.scala 725:22]
    node _F_count_T_1 = tail(_F_count_T, 1) @[house_HolderQR.scala 725:22]
    node _GEN_2 = mux(_T_3, _F_count_T_1, F_count) @[house_HolderQR.scala 724:49 725:11 708:24]
    io_test <= pad(H_count, 2) @[house_HolderQR.scala 735:13]
    io_test_1 <= fullReg @[house_HolderQR.scala 736:15]
    reg_array_h_0 <= reg_array_h_0 @[house_HolderQR.scala 704:24]
    reg_array_h_1 <= reg_array_h_1 @[house_HolderQR.scala 704:24]
    reg_array_h_2 <= reg_array_h_2 @[house_HolderQR.scala 704:24]
    reg_array_h_3 <= reg_array_h_3 @[house_HolderQR.scala 704:24]
    H_count <= mux(reset, UInt<1>("h0"), H_count) @[house_HolderQR.scala 706:{24,24,24}]
    T_count <= mux(reset, UInt<1>("h0"), T_count) @[house_HolderQR.scala 707:{24,24,24}]
    F_count <= mux(reset, UInt<2>("h0"), _GEN_2) @[house_HolderQR.scala 708:{24,24}]
    emptyReg <= mux(reset, UInt<1>("h1"), _GEN_0) @[house_HolderQR.scala 709:{25,25}]
    fullReg <= mux(reset, UInt<1>("h0"), _GEN_1) @[house_HolderQR.scala 710:{24,24}]
