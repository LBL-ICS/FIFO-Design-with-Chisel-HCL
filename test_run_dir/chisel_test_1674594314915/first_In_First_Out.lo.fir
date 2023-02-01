circuit first_In_First_Out :
  module first_In_First_Out :
    input clock : Clock
    input reset : UInt<1>
    input io_write_in_0 : UInt<32>
    input io_write_in_1 : UInt<32>
    input io_write_in_2 : UInt<32>
    output io_read_out_0 : UInt<32>
    output io_read_out_1 : UInt<32>
    output io_read_out_2 : UInt<32>
    input io_read_enable : UInt<1>
    input io_write_enable : UInt<1>

    reg reg_array_Fifo_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_0) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_1) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_2) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_3) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_4) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_5) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_6 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_6) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_7 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_7) @[house_HolderQR.scala 704:27]
    reg reg_array_Fifo_8 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_Fifo_8) @[house_HolderQR.scala 704:27]
    reg H_count : UInt<32>, clock with :
      reset => (UInt<1>("h0"), H_count) @[house_HolderQR.scala 706:24]
    reg T_count : UInt<32>, clock with :
      reset => (UInt<1>("h0"), T_count) @[house_HolderQR.scala 707:24]
    reg F_count : UInt<3>, clock with :
      reset => (UInt<1>("h0"), F_count) @[house_HolderQR.scala 708:24]
    reg emptyReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), emptyReg) @[house_HolderQR.scala 709:25]
    reg fullReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), fullReg) @[house_HolderQR.scala 710:24]
    node _T = eq(F_count, UInt<1>("h0")) @[house_HolderQR.scala 712:16]
    node _GEN_0 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[house_HolderQR.scala 712:24 713:14 715:14]
    node _T_1 = eq(F_count, UInt<2>("h3")) @[house_HolderQR.scala 718:16]
    node _GEN_1 = mux(_T_1, UInt<1>("h1"), UInt<1>("h0")) @[house_HolderQR.scala 718:28 719:13 721:13]
    node _T_2 = neq(fullReg, UInt<1>("h1")) @[house_HolderQR.scala 724:36]
    node _T_3 = and(io_write_enable, _T_2) @[house_HolderQR.scala 724:24]
    node _F_count_T = add(F_count, UInt<1>("h1")) @[house_HolderQR.scala 725:22]
    node _F_count_T_1 = tail(_F_count_T, 1) @[house_HolderQR.scala 725:22]
    node _H_count_T = add(H_count, UInt<1>("h1")) @[house_HolderQR.scala 726:26]
    node _H_count_T_1 = tail(_H_count_T, 1) @[house_HolderQR.scala 726:26]
    node _T_4 = rem(H_count, UInt<2>("h3")) @[house_HolderQR.scala 730:30]
    node _T_5 = mul(_T_4, UInt<2>("h3")) @[house_HolderQR.scala 730:38]
    node _T_6 = add(_T_5, UInt<1>("h0")) @[house_HolderQR.scala 730:46]
    node _T_7 = tail(_T_6, 1) @[house_HolderQR.scala 730:46]
    node _reg_array_Fifo_T_7 = io_write_in_0 @[house_HolderQR.scala 730:{52,52}]
    node _GEN_2 = mux(eq(UInt<1>("h0"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_0) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_3 = mux(eq(UInt<1>("h1"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_1) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_4 = mux(eq(UInt<2>("h2"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_2) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_5 = mux(eq(UInt<2>("h3"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_3) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_6 = mux(eq(UInt<3>("h4"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_4) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_7 = mux(eq(UInt<3>("h5"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_5) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_8 = mux(eq(UInt<3>("h6"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_6) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_9 = mux(eq(UInt<3>("h7"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_7) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _GEN_10 = mux(eq(UInt<4>("h8"), _T_7), _reg_array_Fifo_T_7, reg_array_Fifo_8) @[house_HolderQR.scala 704:27 730:{52,52}]
    node _T_8 = rem(H_count, UInt<2>("h3")) @[house_HolderQR.scala 730:30]
    node _T_9 = mul(_T_8, UInt<2>("h3")) @[house_HolderQR.scala 730:38]
    node _T_10 = add(_T_9, UInt<1>("h1")) @[house_HolderQR.scala 730:46]
    node _T_11 = tail(_T_10, 1) @[house_HolderQR.scala 730:46]
    node _reg_array_Fifo_T_11 = io_write_in_1 @[house_HolderQR.scala 730:{52,52}]
    node _GEN_11 = mux(eq(UInt<1>("h0"), _T_11), _reg_array_Fifo_T_11, _GEN_2) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_12 = mux(eq(UInt<1>("h1"), _T_11), _reg_array_Fifo_T_11, _GEN_3) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_13 = mux(eq(UInt<2>("h2"), _T_11), _reg_array_Fifo_T_11, _GEN_4) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_14 = mux(eq(UInt<2>("h3"), _T_11), _reg_array_Fifo_T_11, _GEN_5) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_15 = mux(eq(UInt<3>("h4"), _T_11), _reg_array_Fifo_T_11, _GEN_6) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_16 = mux(eq(UInt<3>("h5"), _T_11), _reg_array_Fifo_T_11, _GEN_7) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_17 = mux(eq(UInt<3>("h6"), _T_11), _reg_array_Fifo_T_11, _GEN_8) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_18 = mux(eq(UInt<3>("h7"), _T_11), _reg_array_Fifo_T_11, _GEN_9) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_19 = mux(eq(UInt<4>("h8"), _T_11), _reg_array_Fifo_T_11, _GEN_10) @[house_HolderQR.scala 730:{52,52}]
    node _T_12 = rem(H_count, UInt<2>("h3")) @[house_HolderQR.scala 730:30]
    node _T_13 = mul(_T_12, UInt<2>("h3")) @[house_HolderQR.scala 730:38]
    node _T_14 = add(_T_13, UInt<2>("h2")) @[house_HolderQR.scala 730:46]
    node _T_15 = tail(_T_14, 1) @[house_HolderQR.scala 730:46]
    node _reg_array_Fifo_T_15 = io_write_in_2 @[house_HolderQR.scala 730:{52,52}]
    node _GEN_20 = mux(eq(UInt<1>("h0"), _T_15), _reg_array_Fifo_T_15, _GEN_11) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_21 = mux(eq(UInt<1>("h1"), _T_15), _reg_array_Fifo_T_15, _GEN_12) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_22 = mux(eq(UInt<2>("h2"), _T_15), _reg_array_Fifo_T_15, _GEN_13) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_23 = mux(eq(UInt<2>("h3"), _T_15), _reg_array_Fifo_T_15, _GEN_14) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_24 = mux(eq(UInt<3>("h4"), _T_15), _reg_array_Fifo_T_15, _GEN_15) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_25 = mux(eq(UInt<3>("h5"), _T_15), _reg_array_Fifo_T_15, _GEN_16) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_26 = mux(eq(UInt<3>("h6"), _T_15), _reg_array_Fifo_T_15, _GEN_17) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_27 = mux(eq(UInt<3>("h7"), _T_15), _reg_array_Fifo_T_15, _GEN_18) @[house_HolderQR.scala 730:{52,52}]
    node _GEN_28 = mux(eq(UInt<4>("h8"), _T_15), _reg_array_Fifo_T_15, _GEN_19) @[house_HolderQR.scala 730:{52,52}]
    node _T_16 = neq(emptyReg, UInt<1>("h1")) @[house_HolderQR.scala 738:42]
    node _T_17 = and(io_read_enable, _T_16) @[house_HolderQR.scala 738:29]
    node _F_count_T_2 = sub(F_count, UInt<1>("h1")) @[house_HolderQR.scala 739:24]
    node _F_count_T_3 = tail(_F_count_T_2, 1) @[house_HolderQR.scala 739:24]
    node _T_count_T = add(T_count, UInt<1>("h1")) @[house_HolderQR.scala 741:26]
    node _T_count_T_1 = tail(_T_count_T, 1) @[house_HolderQR.scala 741:26]
    node _io_read_out_0_T = sub(T_count, UInt<1>("h1")) @[house_HolderQR.scala 745:48]
    node _io_read_out_0_T_1 = tail(_io_read_out_0_T, 1) @[house_HolderQR.scala 745:48]
    node _io_read_out_0_T_2 = mul(_io_read_out_0_T_1, UInt<2>("h3")) @[house_HolderQR.scala 745:53]
    node _io_read_out_0_T_3 = add(_io_read_out_0_T_2, UInt<1>("h0")) @[house_HolderQR.scala 745:61]
    node _io_read_out_0_T_4 = tail(_io_read_out_0_T_3, 1) @[house_HolderQR.scala 745:61]
    node _io_read_out_0_T_5 = bits(_io_read_out_0_T_4, 3, 0)
    node _GEN_29 = validif(eq(UInt<1>("h0"), _io_read_out_0_T_5), reg_array_Fifo_0) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_30 = mux(eq(UInt<1>("h1"), _io_read_out_0_T_5), reg_array_Fifo_1, _GEN_29) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_31 = mux(eq(UInt<2>("h2"), _io_read_out_0_T_5), reg_array_Fifo_2, _GEN_30) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_32 = mux(eq(UInt<2>("h3"), _io_read_out_0_T_5), reg_array_Fifo_3, _GEN_31) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_33 = mux(eq(UInt<3>("h4"), _io_read_out_0_T_5), reg_array_Fifo_4, _GEN_32) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_34 = mux(eq(UInt<3>("h5"), _io_read_out_0_T_5), reg_array_Fifo_5, _GEN_33) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_35 = mux(eq(UInt<3>("h6"), _io_read_out_0_T_5), reg_array_Fifo_6, _GEN_34) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_36 = mux(eq(UInt<3>("h7"), _io_read_out_0_T_5), reg_array_Fifo_7, _GEN_35) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_37 = mux(eq(UInt<4>("h8"), _io_read_out_0_T_5), reg_array_Fifo_8, _GEN_36) @[house_HolderQR.scala 745:{22,22}]
    node _io_read_out_1_T = sub(T_count, UInt<1>("h1")) @[house_HolderQR.scala 745:48]
    node _io_read_out_1_T_1 = tail(_io_read_out_1_T, 1) @[house_HolderQR.scala 745:48]
    node _io_read_out_1_T_2 = mul(_io_read_out_1_T_1, UInt<2>("h3")) @[house_HolderQR.scala 745:53]
    node _io_read_out_1_T_3 = add(_io_read_out_1_T_2, UInt<1>("h1")) @[house_HolderQR.scala 745:61]
    node _io_read_out_1_T_4 = tail(_io_read_out_1_T_3, 1) @[house_HolderQR.scala 745:61]
    node _io_read_out_1_T_5 = bits(_io_read_out_1_T_4, 3, 0)
    node _GEN_38 = validif(eq(UInt<1>("h0"), _io_read_out_1_T_5), reg_array_Fifo_0) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_39 = mux(eq(UInt<1>("h1"), _io_read_out_1_T_5), reg_array_Fifo_1, _GEN_38) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_40 = mux(eq(UInt<2>("h2"), _io_read_out_1_T_5), reg_array_Fifo_2, _GEN_39) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_41 = mux(eq(UInt<2>("h3"), _io_read_out_1_T_5), reg_array_Fifo_3, _GEN_40) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_42 = mux(eq(UInt<3>("h4"), _io_read_out_1_T_5), reg_array_Fifo_4, _GEN_41) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_43 = mux(eq(UInt<3>("h5"), _io_read_out_1_T_5), reg_array_Fifo_5, _GEN_42) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_44 = mux(eq(UInt<3>("h6"), _io_read_out_1_T_5), reg_array_Fifo_6, _GEN_43) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_45 = mux(eq(UInt<3>("h7"), _io_read_out_1_T_5), reg_array_Fifo_7, _GEN_44) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_46 = mux(eq(UInt<4>("h8"), _io_read_out_1_T_5), reg_array_Fifo_8, _GEN_45) @[house_HolderQR.scala 745:{22,22}]
    node _io_read_out_2_T = sub(T_count, UInt<1>("h1")) @[house_HolderQR.scala 745:48]
    node _io_read_out_2_T_1 = tail(_io_read_out_2_T, 1) @[house_HolderQR.scala 745:48]
    node _io_read_out_2_T_2 = mul(_io_read_out_2_T_1, UInt<2>("h3")) @[house_HolderQR.scala 745:53]
    node _io_read_out_2_T_3 = add(_io_read_out_2_T_2, UInt<2>("h2")) @[house_HolderQR.scala 745:61]
    node _io_read_out_2_T_4 = tail(_io_read_out_2_T_3, 1) @[house_HolderQR.scala 745:61]
    node _io_read_out_2_T_5 = bits(_io_read_out_2_T_4, 3, 0)
    node _GEN_47 = validif(eq(UInt<1>("h0"), _io_read_out_2_T_5), reg_array_Fifo_0) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_48 = mux(eq(UInt<1>("h1"), _io_read_out_2_T_5), reg_array_Fifo_1, _GEN_47) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_49 = mux(eq(UInt<2>("h2"), _io_read_out_2_T_5), reg_array_Fifo_2, _GEN_48) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_50 = mux(eq(UInt<2>("h3"), _io_read_out_2_T_5), reg_array_Fifo_3, _GEN_49) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_51 = mux(eq(UInt<3>("h4"), _io_read_out_2_T_5), reg_array_Fifo_4, _GEN_50) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_52 = mux(eq(UInt<3>("h5"), _io_read_out_2_T_5), reg_array_Fifo_5, _GEN_51) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_53 = mux(eq(UInt<3>("h6"), _io_read_out_2_T_5), reg_array_Fifo_6, _GEN_52) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_54 = mux(eq(UInt<3>("h7"), _io_read_out_2_T_5), reg_array_Fifo_7, _GEN_53) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_55 = mux(eq(UInt<4>("h8"), _io_read_out_2_T_5), reg_array_Fifo_8, _GEN_54) @[house_HolderQR.scala 745:{22,22}]
    node _GEN_56 = mux(_T_17, _F_count_T_3, F_count) @[house_HolderQR.scala 738:55 739:13 708:24]
    node _GEN_57 = mux(_T_17, _T_count_T_1, T_count) @[house_HolderQR.scala 738:55 741:15 707:24]
    node _reg_array_Fifo_io_read_out_0_T_5 = _GEN_37 @[house_HolderQR.scala 745:22]
    node _GEN_58 = mux(_T_17, _reg_array_Fifo_io_read_out_0_T_5, UInt<1>("h0")) @[house_HolderQR.scala 738:55 745:22 751:22]
    node _reg_array_Fifo_io_read_out_1_T_5 = _GEN_46 @[house_HolderQR.scala 745:22]
    node _GEN_59 = mux(_T_17, _reg_array_Fifo_io_read_out_1_T_5, UInt<1>("h0")) @[house_HolderQR.scala 738:55 745:22 751:22]
    node _reg_array_Fifo_io_read_out_2_T_5 = _GEN_55 @[house_HolderQR.scala 745:22]
    node _GEN_60 = mux(_T_17, _reg_array_Fifo_io_read_out_2_T_5, UInt<1>("h0")) @[house_HolderQR.scala 738:55 745:22 751:22]
    node _GEN_61 = mux(_T_3, _F_count_T_1, _GEN_56) @[house_HolderQR.scala 724:49 725:11]
    node _GEN_62 = mux(_T_3, _H_count_T_1, H_count) @[house_HolderQR.scala 724:49 726:15 706:24]
    node _GEN_63 = mux(_T_3, _GEN_20, reg_array_Fifo_0) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_64 = mux(_T_3, _GEN_21, reg_array_Fifo_1) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_65 = mux(_T_3, _GEN_22, reg_array_Fifo_2) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_66 = mux(_T_3, _GEN_23, reg_array_Fifo_3) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_67 = mux(_T_3, _GEN_24, reg_array_Fifo_4) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_68 = mux(_T_3, _GEN_25, reg_array_Fifo_5) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_69 = mux(_T_3, _GEN_26, reg_array_Fifo_6) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_70 = mux(_T_3, _GEN_27, reg_array_Fifo_7) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_71 = mux(_T_3, _GEN_28, reg_array_Fifo_8) @[house_HolderQR.scala 704:27 724:49]
    node _GEN_72 = mux(_T_3, UInt<1>("h0"), _GEN_58) @[house_HolderQR.scala 724:49 736:22]
    node _GEN_73 = mux(_T_3, UInt<1>("h0"), _GEN_59) @[house_HolderQR.scala 724:49 736:22]
    node _GEN_74 = mux(_T_3, UInt<1>("h0"), _GEN_60) @[house_HolderQR.scala 724:49 736:22]
    node _GEN_75 = mux(_T_3, T_count, _GEN_57) @[house_HolderQR.scala 707:24 724:49]
    io_read_out_0 <= _GEN_72
    io_read_out_1 <= _GEN_73
    io_read_out_2 <= _GEN_74
    reg_array_Fifo_0 <= _GEN_63
    reg_array_Fifo_1 <= _GEN_64
    reg_array_Fifo_2 <= _GEN_65
    reg_array_Fifo_3 <= _GEN_66
    reg_array_Fifo_4 <= _GEN_67
    reg_array_Fifo_5 <= _GEN_68
    reg_array_Fifo_6 <= _GEN_69
    reg_array_Fifo_7 <= _GEN_70
    reg_array_Fifo_8 <= _GEN_71
    H_count <= mux(reset, UInt<32>("h0"), _GEN_62) @[house_HolderQR.scala 706:{24,24}]
    T_count <= mux(reset, UInt<32>("h0"), _GEN_75) @[house_HolderQR.scala 707:{24,24}]
    F_count <= mux(reset, UInt<3>("h0"), _GEN_61) @[house_HolderQR.scala 708:{24,24}]
    emptyReg <= mux(reset, UInt<1>("h1"), _GEN_0) @[house_HolderQR.scala 709:{25,25}]
    fullReg <= mux(reset, UInt<1>("h0"), _GEN_1) @[house_HolderQR.scala 710:{24,24}]
