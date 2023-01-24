circuit first_In_First_Out :
  module first_In_First_Out :
    input clock : Clock
    input reset : UInt<1>
    input io_write_in_0 : UInt<2>
    input io_write_in_1 : UInt<2>
    output io_test : UInt<2>
    input io_read_enable : UInt<1>
    input io_write_enable : UInt<1>

    reg reg_array_Fifo_0 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_0) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_1) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_2) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_3 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_3) @[house_HolderQR.scala 704:27]
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
    node _F_count_T = add(F_count, UInt<1>("h1")) @[house_HolderQR.scala 725:22]
    node _F_count_T_1 = tail(_F_count_T, 1) @[house_HolderQR.scala 725:22]
    node _H_count_T = add(H_count, UInt<1>("h1")) @[house_HolderQR.scala 726:24]
    node _H_count_T_1 = tail(_H_count_T, 1) @[house_HolderQR.scala 726:24]
    node _T_4 = sub(H_count, UInt<1>("h1")) @[house_HolderQR.scala 729:28]
    node _T_5 = tail(_T_4, 1) @[house_HolderQR.scala 729:28]
    node _T_6 = mul(_T_5, UInt<2>("h2")) @[house_HolderQR.scala 729:33]
    node _T_7 = add(_T_6, UInt<1>("h0")) @[house_HolderQR.scala 729:41]
    node _T_8 = tail(_T_7, 1) @[house_HolderQR.scala 729:41]
    node _T_9 = bits(_T_8, 1, 0)
    node _reg_array_Fifo_T_9 = io_write_in_0 @[house_HolderQR.scala 729:{47,47}]
    node _GEN_2 = mux(eq(UInt<1>("h0"), _T_9), _reg_array_Fifo_T_9, reg_array_Fifo_0) @[house_HolderQR.scala 704:27 729:{47,47}]
    node _GEN_3 = mux(eq(UInt<1>("h1"), _T_9), _reg_array_Fifo_T_9, reg_array_Fifo_1) @[house_HolderQR.scala 704:27 729:{47,47}]
    node _GEN_4 = mux(eq(UInt<2>("h2"), _T_9), _reg_array_Fifo_T_9, reg_array_Fifo_2) @[house_HolderQR.scala 704:27 729:{47,47}]
    node _GEN_5 = mux(eq(UInt<2>("h3"), _T_9), _reg_array_Fifo_T_9, reg_array_Fifo_3) @[house_HolderQR.scala 704:27 729:{47,47}]
    node _T_10 = sub(H_count, UInt<1>("h1")) @[house_HolderQR.scala 729:28]
    node _T_11 = tail(_T_10, 1) @[house_HolderQR.scala 729:28]
    node _T_12 = mul(_T_11, UInt<2>("h2")) @[house_HolderQR.scala 729:33]
    node _T_13 = add(_T_12, UInt<1>("h1")) @[house_HolderQR.scala 729:41]
    node _T_14 = tail(_T_13, 1) @[house_HolderQR.scala 729:41]
    node _T_15 = bits(_T_14, 1, 0)
    node _reg_array_Fifo_T_15 = io_write_in_1 @[house_HolderQR.scala 729:{47,47}]
    node _GEN_6 = mux(eq(UInt<1>("h0"), _T_15), _reg_array_Fifo_T_15, _GEN_2) @[house_HolderQR.scala 729:{47,47}]
    node _GEN_7 = mux(eq(UInt<1>("h1"), _T_15), _reg_array_Fifo_T_15, _GEN_3) @[house_HolderQR.scala 729:{47,47}]
    node _GEN_8 = mux(eq(UInt<2>("h2"), _T_15), _reg_array_Fifo_T_15, _GEN_4) @[house_HolderQR.scala 729:{47,47}]
    node _GEN_9 = mux(eq(UInt<2>("h3"), _T_15), _reg_array_Fifo_T_15, _GEN_5) @[house_HolderQR.scala 729:{47,47}]
    node _GEN_10 = mux(_T_3, _F_count_T_1, F_count) @[house_HolderQR.scala 724:49 725:11 708:24]
    node _GEN_11 = mux(_T_3, _H_count_T_1, H_count) @[house_HolderQR.scala 724:49 726:13 706:24]
    node _GEN_12 = mux(_T_3, _GEN_6, reg_array_Fifo_0) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_13 = mux(_T_3, _GEN_7, reg_array_Fifo_1) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_14 = mux(_T_3, _GEN_8, reg_array_Fifo_2) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_15 = mux(_T_3, _GEN_9, reg_array_Fifo_3) @[house_HolderQR.scala 704:27 724:49]
    io_test <= reg_array_Fifo_2 @[house_HolderQR.scala 739:13]
    reg_array_Fifo_0 <= _GEN_12
    reg_array_Fifo_1 <= _GEN_13
    reg_array_Fifo_2 <= _GEN_14
    reg_array_Fifo_3 <= _GEN_15
    H_count <= mux(reset, UInt<1>("h0"), _GEN_11) @[house_HolderQR.scala 706:{24,24}]
    T_count <= mux(reset, UInt<1>("h0"), T_count) @[house_HolderQR.scala 707:{24,24,24}]
    F_count <= mux(reset, UInt<2>("h0"), _GEN_10) @[house_HolderQR.scala 708:{24,24}]
    emptyReg <= mux(reset, UInt<1>("h1"), _GEN_0) @[house_HolderQR.scala 709:{25,25}]
    fullReg <= mux(reset, UInt<1>("h0"), _GEN_1) @[house_HolderQR.scala 710:{24,24}]
