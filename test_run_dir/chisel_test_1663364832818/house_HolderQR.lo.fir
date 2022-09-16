circuit house_HolderQR :
  module multiplier :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<24>
    input io_in_b : UInt<24>
    output io_out_s : UInt<48>

    node _result_T = mul(io_in_a, io_in_b) @[Arithmetic.scala 87:23]
    node result = _result_T @[Arithmetic.scala 86:22 87:12]
    io_out_s <= result @[Arithmetic.scala 88:14]

  module full_subber :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<8>
    input io_in_b : UInt<8>
    input io_in_c : UInt<1>
    output io_out_s : UInt<8>
    output io_out_c : UInt<1>

    node _result_T = sub(io_in_a, io_in_b) @[Arithmetic.scala 75:23]
    node _result_T_1 = asUInt(_result_T) @[Arithmetic.scala 75:23]
    node _result_T_2 = sub(_result_T_1, io_in_c) @[Arithmetic.scala 75:34]
    node _result_T_3 = asUInt(_result_T_2) @[Arithmetic.scala 75:34]
    node result = bits(_result_T_3, 8, 0) @[Arithmetic.scala 74:22 75:12]
    node _io_out_s_T = bits(result, 7, 0) @[Arithmetic.scala 76:23]
    node _io_out_c_T = bits(result, 8, 8) @[Arithmetic.scala 77:23]
    io_out_s <= _io_out_s_T @[Arithmetic.scala 76:14]
    io_out_c <= _io_out_c_T @[Arithmetic.scala 77:14]

  module twoscomplement :
    input clock : Clock
    input reset : UInt<1>
    input io_in : UInt<8>
    output io_out : UInt<8>

    node _x_T = not(io_in) @[Arithmetic.scala 31:16]
    node _x_T_1 = add(UInt<1>("h1"), _x_T) @[Arithmetic.scala 31:14]
    node _x_T_2 = tail(_x_T_1, 1) @[Arithmetic.scala 31:14]
    node x = _x_T_2 @[Arithmetic.scala 30:17 31:7]
    io_out <= x @[Arithmetic.scala 32:12]

  module full_adder :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<8>
    input io_in_b : UInt<8>
    input io_in_c : UInt<1>
    output io_out_s : UInt<8>
    output io_out_c : UInt<1>

    node _result_T = add(io_in_a, io_in_b) @[Arithmetic.scala 61:23]
    node _result_T_1 = add(_result_T, io_in_c) @[Arithmetic.scala 61:34]
    node result = bits(_result_T_1, 8, 0) @[Arithmetic.scala 60:22 61:12]
    node _io_out_s_T = bits(result, 7, 0) @[Arithmetic.scala 62:23]
    node _io_out_c_T = bits(result, 8, 8) @[Arithmetic.scala 63:23]
    io_out_s <= _io_out_s_T @[Arithmetic.scala 62:14]
    io_out_c <= _io_out_c_T @[Arithmetic.scala 63:14]

  module FP_multiplier :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b : UInt<32>
    output io_out_s : UInt<32>

    inst multiplier of multiplier @[FPArithmetic.scala 800:28]
    inst subber of full_subber @[FPArithmetic.scala 805:24]
    inst complementN of twoscomplement @[FPArithmetic.scala 811:29]
    inst adderN of full_adder @[FPArithmetic.scala 815:24]
    node _s_0_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 765:20]
    node _s_1_T = bits(io_in_b, 31, 31) @[FPArithmetic.scala 766:20]
    node _T = bits(io_in_a, 30, 23) @[FPArithmetic.scala 770:17]
    node _T_1 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 770:62]
    node _T_2 = tail(_T_1, 1) @[FPArithmetic.scala 770:62]
    node _T_3 = gt(_T, _T_2) @[FPArithmetic.scala 770:34]
    node _exp_0_T = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 771:43]
    node _exp_0_T_1 = tail(_exp_0_T, 1) @[FPArithmetic.scala 771:43]
    node _exp_0_T_2 = bits(io_in_a, 30, 23) @[FPArithmetic.scala 773:24]
    node _GEN_0 = mux(_T_3, _exp_0_T_1, _exp_0_T_2) @[FPArithmetic.scala 770:68 771:14 773:14]
    node _T_4 = bits(io_in_b, 30, 23) @[FPArithmetic.scala 775:17]
    node _T_5 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 775:62]
    node _T_6 = tail(_T_5, 1) @[FPArithmetic.scala 775:62]
    node _T_7 = gt(_T_4, _T_6) @[FPArithmetic.scala 775:34]
    node _exp_1_T = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 776:43]
    node _exp_1_T_1 = tail(_exp_1_T, 1) @[FPArithmetic.scala 776:43]
    node _exp_1_T_2 = bits(io_in_b, 30, 23) @[FPArithmetic.scala 778:24]
    node _GEN_1 = mux(_T_7, _exp_1_T_1, _exp_1_T_2) @[FPArithmetic.scala 775:68 776:14 778:14]
    node _exp_check_0_T = bits(io_in_a, 30, 23) @[FPArithmetic.scala 782:28]
    node _exp_check_1_T = bits(io_in_b, 30, 23) @[FPArithmetic.scala 783:28]
    node exp_check_0 = pad(_exp_check_0_T, 23) @[FPArithmetic.scala 781:25 782:18]
    node _cond_holder_T = add(exp_check_0, UInt<1>("h1")) @[FPArithmetic.scala 786:34]
    node _cond_holder_T_1 = tail(_cond_holder_T, 1) @[FPArithmetic.scala 786:34]
    node exp_check_1 = pad(_exp_check_1_T, 23) @[FPArithmetic.scala 781:25 783:18]
    node _cond_holder_T_2 = sub(UInt<7>("h7f"), exp_check_1) @[FPArithmetic.scala 786:80]
    node _cond_holder_T_3 = tail(_cond_holder_T_2, 1) @[FPArithmetic.scala 786:80]
    node _cond_holder_T_4 = not(_cond_holder_T_3) @[FPArithmetic.scala 786:42]
    node _cond_holder_T_5 = add(_cond_holder_T_1, _cond_holder_T_4) @[FPArithmetic.scala 786:40]
    node _cond_holder_T_6 = tail(_cond_holder_T_5, 1) @[FPArithmetic.scala 786:40]
    node _frac_0_T = bits(io_in_a, 22, 0) @[FPArithmetic.scala 790:23]
    node _frac_1_T = bits(io_in_b, 22, 0) @[FPArithmetic.scala 791:23]
    node frac_0 = _frac_0_T @[FPArithmetic.scala 789:20 790:13]
    node _new_frac_0_T = cat(UInt<1>("h1"), frac_0) @[FPArithmetic.scala 795:24]
    node frac_1 = _frac_1_T @[FPArithmetic.scala 789:20 791:13]
    node _new_frac_1_T = cat(UInt<1>("h1"), frac_1) @[FPArithmetic.scala 796:24]
    node s_0 = _s_0_T @[FPArithmetic.scala 764:17 765:10]
    node s_1 = _s_1_T @[FPArithmetic.scala 764:17 766:10]
    node _new_s_T = xor(s_0, s_1) @[FPArithmetic.scala 822:19]
    node _T_8 = bits(multiplier.io_out_s, 47, 47) @[FPArithmetic.scala 832:29]
    node _T_9 = eq(_T_8, UInt<1>("h1")) @[FPArithmetic.scala 832:52]
    node _new_exp_T = add(adderN.io_out_s, UInt<1>("h1")) @[FPArithmetic.scala 833:34]
    node _new_exp_T_1 = tail(_new_exp_T, 1) @[FPArithmetic.scala 833:34]
    node _cond_check_T = add(adderN.io_out_s, UInt<1>("h1")) @[FPArithmetic.scala 834:37]
    node _cond_check_T_1 = tail(_cond_check_T, 1) @[FPArithmetic.scala 834:37]
    node _cond_holder_T_7 = add(exp_check_0, UInt<2>("h2")) @[FPArithmetic.scala 835:36]
    node _cond_holder_T_8 = tail(_cond_holder_T_7, 1) @[FPArithmetic.scala 835:36]
    node _cond_holder_T_9 = sub(UInt<7>("h7f"), exp_check_1) @[FPArithmetic.scala 835:82]
    node _cond_holder_T_10 = tail(_cond_holder_T_9, 1) @[FPArithmetic.scala 835:82]
    node _cond_holder_T_11 = not(_cond_holder_T_10) @[FPArithmetic.scala 835:44]
    node _cond_holder_T_12 = add(_cond_holder_T_8, _cond_holder_T_11) @[FPArithmetic.scala 835:42]
    node _cond_holder_T_13 = tail(_cond_holder_T_12, 1) @[FPArithmetic.scala 835:42]
    node _new_mant_T = bits(multiplier.io_out_s, 46, 24) @[FPArithmetic.scala 836:38]
    node _cond_holder_T_14 = add(exp_check_0, UInt<1>("h1")) @[FPArithmetic.scala 840:36]
    node _cond_holder_T_15 = tail(_cond_holder_T_14, 1) @[FPArithmetic.scala 840:36]
    node _cond_holder_T_16 = sub(UInt<7>("h7f"), exp_check_1) @[FPArithmetic.scala 840:82]
    node _cond_holder_T_17 = tail(_cond_holder_T_16, 1) @[FPArithmetic.scala 840:82]
    node _cond_holder_T_18 = not(_cond_holder_T_17) @[FPArithmetic.scala 840:44]
    node _cond_holder_T_19 = add(_cond_holder_T_15, _cond_holder_T_18) @[FPArithmetic.scala 840:42]
    node _cond_holder_T_20 = tail(_cond_holder_T_19, 1) @[FPArithmetic.scala 840:42]
    node _new_mant_T_1 = bits(multiplier.io_out_s, 46, 24) @[FPArithmetic.scala 841:38]
    node _new_mant_T_2 = dshl(_new_mant_T_1, UInt<1>("h1")) @[FPArithmetic.scala 841:73]
    node _GEN_2 = mux(_T_9, _new_exp_T_1, adderN.io_out_s) @[FPArithmetic.scala 832:60 833:15 838:15]
    node _GEN_3 = mux(_T_9, _cond_check_T_1, adderN.io_out_s) @[FPArithmetic.scala 832:60 834:18 839:18]
    node _GEN_4 = mux(_T_9, _cond_holder_T_13, _cond_holder_T_20) @[FPArithmetic.scala 832:60 835:19 840:19]
    node _GEN_5 = mux(_T_9, _new_mant_T, _new_mant_T_2) @[FPArithmetic.scala 832:60 836:16 841:16]
    reg reg_out_s : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_out_s) @[FPArithmetic.scala 843:28]
    node _T_10 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 845:36]
    node _T_11 = tail(_T_10, 1) @[FPArithmetic.scala 845:36]
    node cond_holder = _GEN_4 @[FPArithmetic.scala 785:27]
    node _T_12 = not(cond_holder) @[FPArithmetic.scala 845:51]
    node _T_13 = add(UInt<1>("h1"), _T_12) @[FPArithmetic.scala 845:49]
    node _T_14 = tail(_T_13, 1) @[FPArithmetic.scala 845:49]
    node _T_15 = geq(_T_11, _T_14) @[FPArithmetic.scala 845:42]
    node new_s = _new_s_T @[FPArithmetic.scala 821:21 822:11]
    node _T_16 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 850:57]
    node _T_17 = tail(_T_16, 1) @[FPArithmetic.scala 850:57]
    node _T_18 = gt(cond_holder, _T_17) @[FPArithmetic.scala 850:29]
    node _new_exp_T_2 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 851:46]
    node _new_exp_T_3 = tail(_new_exp_T_2, 1) @[FPArithmetic.scala 851:46]
    node _GEN_6 = mux(_T_18, _new_exp_T_3, _GEN_2) @[FPArithmetic.scala 850:63 851:17]
    node _GEN_9 = mux(_T_15, UInt<8>("h1"), _GEN_6) @[FPArithmetic.scala 845:67 846:15]
    node new_exp = bits(_GEN_9, 7, 0) @[FPArithmetic.scala 825:23]
    node _reg_out_s_T = cat(new_s, new_exp) @[FPArithmetic.scala 848:26]
    node _new_mant_T_3 = sub(UInt<24>("h800000"), UInt<1>("h1")) @[FPArithmetic.scala 852:47]
    node _new_mant_T_4 = tail(_new_mant_T_3, 1) @[FPArithmetic.scala 852:47]
    node _GEN_7 = mux(_T_18, _new_mant_T_4, _GEN_5) @[FPArithmetic.scala 850:63 852:18]
    node _GEN_10 = mux(_T_15, UInt<23>("h400000"), _GEN_7) @[FPArithmetic.scala 845:67 847:16]
    node new_mant = bits(_GEN_10, 22, 0) @[FPArithmetic.scala 827:24]
    node _reg_out_s_T_1 = cat(_reg_out_s_T, new_mant) @[FPArithmetic.scala 848:37]
    node _reg_out_s_T_2 = cat(new_s, new_exp) @[FPArithmetic.scala 853:28]
    node _reg_out_s_T_3 = cat(_reg_out_s_T_2, new_mant) @[FPArithmetic.scala 853:39]
    node _GEN_8 = mux(_T_18, _reg_out_s_T_3, reg_out_s) @[FPArithmetic.scala 850:63 853:19 843:28]
    node _GEN_11 = mux(_T_15, _reg_out_s_T_1, _GEN_8) @[FPArithmetic.scala 845:67 848:17]
    node exp_0 = bits(_GEN_0, 7, 0) @[FPArithmetic.scala 769:19]
    node _T_19 = eq(exp_0, UInt<1>("h0")) @[FPArithmetic.scala 855:17]
    node exp_1 = bits(_GEN_1, 7, 0) @[FPArithmetic.scala 769:19]
    node _T_20 = eq(exp_1, UInt<1>("h0")) @[FPArithmetic.scala 855:35]
    node _T_21 = or(_T_19, _T_20) @[FPArithmetic.scala 855:25]
    node _reg_out_s_T_4 = cat(new_s, new_exp) @[FPArithmetic.scala 858:26]
    node _reg_out_s_T_5 = cat(_reg_out_s_T_4, new_mant) @[FPArithmetic.scala 858:37]
    node _GEN_12 = mux(_T_21, UInt<1>("h0"), _reg_out_s_T_5) @[FPArithmetic.scala 855:43 856:17 858:17]
    node new_frac_0 = _new_frac_0_T @[FPArithmetic.scala 794:24 795:17]
    node new_frac_1 = _new_frac_1_T @[FPArithmetic.scala 794:24 796:17]
    node cond_check = pad(_GEN_3, 23) @[FPArithmetic.scala 829:26]
    io_out_s <= reg_out_s @[FPArithmetic.scala 860:14]
    multiplier.clock <= clock
    multiplier.reset <= reset
    multiplier.io_in_a <= new_frac_0 @[FPArithmetic.scala 801:24]
    multiplier.io_in_b <= new_frac_1 @[FPArithmetic.scala 802:24]
    subber.clock <= clock
    subber.reset <= reset
    subber.io_in_a <= UInt<8>("h7f") @[FPArithmetic.scala 806:20]
    subber.io_in_b <= exp_1 @[FPArithmetic.scala 807:20]
    subber.io_in_c <= UInt<1>("h0") @[FPArithmetic.scala 808:20]
    complementN.clock <= clock
    complementN.reset <= reset
    complementN.io_in <= subber.io_out_s @[FPArithmetic.scala 812:23]
    adderN.clock <= clock
    adderN.reset <= reset
    adderN.io_in_a <= exp_0 @[FPArithmetic.scala 816:20]
    adderN.io_in_b <= complementN.io_out @[FPArithmetic.scala 817:20]
    adderN.io_in_c <= UInt<1>("h0") @[FPArithmetic.scala 818:20]
    reg_out_s <= mux(reset, UInt<32>("h0"), _GEN_12) @[FPArithmetic.scala 843:{28,28}]

  module full_adder_2 :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<24>
    input io_in_b : UInt<24>
    input io_in_c : UInt<1>
    output io_out_s : UInt<24>
    output io_out_c : UInt<1>

    node _result_T = add(io_in_a, io_in_b) @[Arithmetic.scala 61:23]
    node _result_T_1 = add(_result_T, io_in_c) @[Arithmetic.scala 61:34]
    node result = bits(_result_T_1, 24, 0) @[Arithmetic.scala 60:22 61:12]
    node _io_out_s_T = bits(result, 23, 0) @[Arithmetic.scala 62:23]
    node _io_out_c_T = bits(result, 24, 24) @[Arithmetic.scala 63:23]
    io_out_s <= _io_out_s_T @[Arithmetic.scala 62:14]
    io_out_c <= _io_out_c_T @[Arithmetic.scala 63:14]

  module twoscomplement_3 :
    input clock : Clock
    input reset : UInt<1>
    input io_in : UInt<24>
    output io_out : UInt<24>

    node _x_T = not(io_in) @[Arithmetic.scala 31:16]
    node _x_T_1 = add(UInt<1>("h1"), _x_T) @[Arithmetic.scala 31:14]
    node _x_T_2 = tail(_x_T_1, 1) @[Arithmetic.scala 31:14]
    node x = _x_T_2 @[Arithmetic.scala 30:17 31:7]
    io_out <= x @[Arithmetic.scala 32:12]

  module shifter :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<24>
    input io_in_b : UInt<5>
    input io_in_c : UInt<1>
    output io_out_s : UInt<24>

    node _T = eq(io_in_c, UInt<1>("h1")) @[Arithmetic.scala 44:18]
    node _result_T = dshr(io_in_a, io_in_b) @[Arithmetic.scala 45:25]
    node _result_T_1 = dshl(io_in_a, io_in_b) @[Arithmetic.scala 47:25]
    node _GEN_0 = mux(_T, _result_T, _result_T_1) @[Arithmetic.scala 44:26 45:14 47:14]
    node result = bits(_GEN_0, 23, 0) @[Arithmetic.scala 42:22]
    io_out_s <= result @[Arithmetic.scala 49:14]

  module leadingOneDetector :
    input clock : Clock
    input reset : UInt<1>
    input io_in : UInt<24>
    output io_out : UInt<5>

    node _T = bits(io_in, 23, 23) @[Arithmetic.scala 19:13]
    node _T_1 = bits(io_in, 22, 22) @[Arithmetic.scala 19:13]
    node _T_2 = bits(io_in, 21, 21) @[Arithmetic.scala 19:13]
    node _T_3 = bits(io_in, 20, 20) @[Arithmetic.scala 19:13]
    node _T_4 = bits(io_in, 19, 19) @[Arithmetic.scala 19:13]
    node _T_5 = bits(io_in, 18, 18) @[Arithmetic.scala 19:13]
    node _T_6 = bits(io_in, 17, 17) @[Arithmetic.scala 19:13]
    node _T_7 = bits(io_in, 16, 16) @[Arithmetic.scala 19:13]
    node _T_8 = bits(io_in, 15, 15) @[Arithmetic.scala 19:13]
    node _T_9 = bits(io_in, 14, 14) @[Arithmetic.scala 19:13]
    node _T_10 = bits(io_in, 13, 13) @[Arithmetic.scala 19:13]
    node _T_11 = bits(io_in, 12, 12) @[Arithmetic.scala 19:13]
    node _T_12 = bits(io_in, 11, 11) @[Arithmetic.scala 19:13]
    node _T_13 = bits(io_in, 10, 10) @[Arithmetic.scala 19:13]
    node _T_14 = bits(io_in, 9, 9) @[Arithmetic.scala 19:13]
    node _T_15 = bits(io_in, 8, 8) @[Arithmetic.scala 19:13]
    node _T_16 = bits(io_in, 7, 7) @[Arithmetic.scala 19:13]
    node _T_17 = bits(io_in, 6, 6) @[Arithmetic.scala 19:13]
    node _T_18 = bits(io_in, 5, 5) @[Arithmetic.scala 19:13]
    node _T_19 = bits(io_in, 4, 4) @[Arithmetic.scala 19:13]
    node _T_20 = bits(io_in, 3, 3) @[Arithmetic.scala 19:13]
    node _T_21 = bits(io_in, 2, 2) @[Arithmetic.scala 19:13]
    node _T_22 = bits(io_in, 1, 1) @[Arithmetic.scala 19:13]
    node _T_23 = bits(io_in, 0, 0) @[Arithmetic.scala 19:13]
    node _hotValue_T = mux(_T_22, UInt<2>("h2"), UInt<1>("h1")) @[Mux.scala 47:70]
    node _hotValue_T_1 = mux(_T_21, UInt<2>("h3"), _hotValue_T) @[Mux.scala 47:70]
    node _hotValue_T_2 = mux(_T_20, UInt<3>("h4"), _hotValue_T_1) @[Mux.scala 47:70]
    node _hotValue_T_3 = mux(_T_19, UInt<3>("h5"), _hotValue_T_2) @[Mux.scala 47:70]
    node _hotValue_T_4 = mux(_T_18, UInt<3>("h6"), _hotValue_T_3) @[Mux.scala 47:70]
    node _hotValue_T_5 = mux(_T_17, UInt<3>("h7"), _hotValue_T_4) @[Mux.scala 47:70]
    node _hotValue_T_6 = mux(_T_16, UInt<4>("h8"), _hotValue_T_5) @[Mux.scala 47:70]
    node _hotValue_T_7 = mux(_T_15, UInt<4>("h9"), _hotValue_T_6) @[Mux.scala 47:70]
    node _hotValue_T_8 = mux(_T_14, UInt<4>("ha"), _hotValue_T_7) @[Mux.scala 47:70]
    node _hotValue_T_9 = mux(_T_13, UInt<4>("hb"), _hotValue_T_8) @[Mux.scala 47:70]
    node _hotValue_T_10 = mux(_T_12, UInt<4>("hc"), _hotValue_T_9) @[Mux.scala 47:70]
    node _hotValue_T_11 = mux(_T_11, UInt<4>("hd"), _hotValue_T_10) @[Mux.scala 47:70]
    node _hotValue_T_12 = mux(_T_10, UInt<4>("he"), _hotValue_T_11) @[Mux.scala 47:70]
    node _hotValue_T_13 = mux(_T_9, UInt<4>("hf"), _hotValue_T_12) @[Mux.scala 47:70]
    node _hotValue_T_14 = mux(_T_8, UInt<5>("h10"), _hotValue_T_13) @[Mux.scala 47:70]
    node _hotValue_T_15 = mux(_T_7, UInt<5>("h11"), _hotValue_T_14) @[Mux.scala 47:70]
    node _hotValue_T_16 = mux(_T_6, UInt<5>("h12"), _hotValue_T_15) @[Mux.scala 47:70]
    node _hotValue_T_17 = mux(_T_5, UInt<5>("h13"), _hotValue_T_16) @[Mux.scala 47:70]
    node _hotValue_T_18 = mux(_T_4, UInt<5>("h14"), _hotValue_T_17) @[Mux.scala 47:70]
    node _hotValue_T_19 = mux(_T_3, UInt<5>("h15"), _hotValue_T_18) @[Mux.scala 47:70]
    node _hotValue_T_20 = mux(_T_2, UInt<5>("h16"), _hotValue_T_19) @[Mux.scala 47:70]
    node _hotValue_T_21 = mux(_T_1, UInt<5>("h17"), _hotValue_T_20) @[Mux.scala 47:70]
    node hotValue = mux(_T, UInt<5>("h18"), _hotValue_T_21) @[Mux.scala 47:70]
    io_out <= hotValue @[Arithmetic.scala 22:12]

  module FP_adder :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b : UInt<32>
    output io_out_s : UInt<32>

    inst subber of full_subber @[FPArithmetic.scala 77:24]
    inst complement of twoscomplement @[FPArithmetic.scala 83:28]
    inst adder of full_adder_2 @[FPArithmetic.scala 87:23]
    inst complementN_0 of twoscomplement_3 @[FPArithmetic.scala 93:31]
    inst complementN_1 of twoscomplement_3 @[FPArithmetic.scala 95:31]
    inst shifter of shifter @[FPArithmetic.scala 99:25]
    inst complementN_2 of twoscomplement_3 @[FPArithmetic.scala 144:31]
    inst leadingOneFinder of leadingOneDetector @[FPArithmetic.scala 164:34]
    inst subber2 of full_subber @[FPArithmetic.scala 166:25]
    node _sign_0_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 39:23]
    node _sign_1_T = bits(io_in_b, 31, 31) @[FPArithmetic.scala 40:23]
    node _T = bits(io_in_a, 30, 23) @[FPArithmetic.scala 44:17]
    node _T_1 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 44:62]
    node _T_2 = tail(_T_1, 1) @[FPArithmetic.scala 44:62]
    node _T_3 = gt(_T, _T_2) @[FPArithmetic.scala 44:34]
    node _exp_0_T = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 45:43]
    node _exp_0_T_1 = tail(_exp_0_T, 1) @[FPArithmetic.scala 45:43]
    node _exp_0_T_2 = bits(io_in_a, 30, 23) @[FPArithmetic.scala 47:24]
    node _GEN_0 = mux(_T_3, _exp_0_T_1, _exp_0_T_2) @[FPArithmetic.scala 44:68 45:14 47:14]
    node _T_4 = bits(io_in_b, 30, 23) @[FPArithmetic.scala 49:17]
    node _T_5 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 49:62]
    node _T_6 = tail(_T_5, 1) @[FPArithmetic.scala 49:62]
    node _T_7 = gt(_T_4, _T_6) @[FPArithmetic.scala 49:34]
    node _exp_1_T = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 50:43]
    node _exp_1_T_1 = tail(_exp_1_T, 1) @[FPArithmetic.scala 50:43]
    node _exp_1_T_2 = bits(io_in_b, 30, 23) @[FPArithmetic.scala 52:24]
    node _GEN_1 = mux(_T_7, _exp_1_T_1, _exp_1_T_2) @[FPArithmetic.scala 49:68 50:14 52:14]
    node _frac_0_T = bits(io_in_a, 22, 0) @[FPArithmetic.scala 57:23]
    node _frac_1_T = bits(io_in_b, 22, 0) @[FPArithmetic.scala 58:23]
    node frac_0 = _frac_0_T @[FPArithmetic.scala 56:20 57:13]
    node _whole_frac_0_T = cat(UInt<1>("h1"), frac_0) @[FPArithmetic.scala 62:26]
    node frac_1 = _frac_1_T @[FPArithmetic.scala 56:20 58:13]
    node _whole_frac_1_T = cat(UInt<1>("h1"), frac_1) @[FPArithmetic.scala 63:26]
    node _T_8 = eq(subber.io_out_c, UInt<1>("h1")) @[FPArithmetic.scala 105:26]
    node exp_1 = bits(_GEN_1, 7, 0) @[FPArithmetic.scala 43:19]
    node exp_0 = bits(_GEN_0, 7, 0) @[FPArithmetic.scala 43:19]
    node _GEN_2 = mux(_T_8, exp_1, exp_0) @[FPArithmetic.scala 105:34 106:15 116:15]
    node _GEN_3 = mux(_T_8, complement.io_out, subber.io_out_s) @[FPArithmetic.scala 105:34 107:15 117:15]
    node sign_1 = _sign_1_T @[FPArithmetic.scala 38:20 40:13]
    node sign_0 = _sign_0_T @[FPArithmetic.scala 38:20 39:13]
    node _GEN_4 = mux(_T_8, sign_1, sign_0) @[FPArithmetic.scala 105:34 108:13 118:13]
    node _GEN_5 = mux(_T_8, frac_1, frac_0) @[FPArithmetic.scala 105:34 109:16 119:16]
    node whole_frac_0 = _whole_frac_0_T @[FPArithmetic.scala 61:26 62:19]
    node whole_frac_1 = _whole_frac_1_T @[FPArithmetic.scala 61:26 63:19]
    node _GEN_6 = mux(_T_8, whole_frac_0, whole_frac_1) @[FPArithmetic.scala 105:34 110:23 120:23]
    node _GEN_7 = mux(_T_8, UInt<1>("h1"), UInt<1>("h1")) @[FPArithmetic.scala 105:34 112:23 122:23]
    node _GEN_8 = mux(_T_8, shifter.io_out_s, whole_frac_0) @[FPArithmetic.scala 105:34 113:21 88:19]
    node _GEN_9 = mux(_T_8, whole_frac_1, shifter.io_out_s) @[FPArithmetic.scala 105:34 89:19 123:21]
    node _T_9 = eq(sign_0, UInt<1>("h1")) @[FPArithmetic.scala 128:18]
    node _T_10 = eq(sign_1, UInt<1>("h0")) @[FPArithmetic.scala 128:37]
    node _T_11 = and(_T_9, _T_10) @[FPArithmetic.scala 128:26]
    node _GEN_10 = mux(_T_11, complementN_0.io_out, _GEN_8) @[FPArithmetic.scala 128:45 129:21]
    node _T_12 = eq(sign_1, UInt<1>("h1")) @[FPArithmetic.scala 132:18]
    node _T_13 = eq(sign_0, UInt<1>("h0")) @[FPArithmetic.scala 132:37]
    node _T_14 = and(_T_12, _T_13) @[FPArithmetic.scala 132:26]
    node _GEN_11 = mux(_T_14, complementN_1.io_out, _GEN_9) @[FPArithmetic.scala 132:45 133:21]
    node _new_s_T = not(adder.io_out_c) @[FPArithmetic.scala 139:15]
    node _new_s_T_1 = and(_new_s_T, sign_0) @[FPArithmetic.scala 139:31]
    node _new_s_T_2 = and(sign_0, sign_1) @[FPArithmetic.scala 139:53]
    node _new_s_T_3 = or(_new_s_T_1, _new_s_T_2) @[FPArithmetic.scala 139:42]
    node _new_s_T_4 = not(adder.io_out_c) @[FPArithmetic.scala 139:67]
    node _new_s_T_5 = and(_new_s_T_4, sign_1) @[FPArithmetic.scala 139:83]
    node _new_s_T_6 = or(_new_s_T_3, _new_s_T_5) @[FPArithmetic.scala 139:64]
    node _D_T = not(adder.io_out_c) @[FPArithmetic.scala 152:11]
    node _D_T_1 = xor(sign_0, sign_1) @[FPArithmetic.scala 152:39]
    node _D_T_2 = or(_D_T, _D_T_1) @[FPArithmetic.scala 152:28]
    node _E_T = not(adder.io_out_c) @[FPArithmetic.scala 155:11]
    node _E_T_1 = bits(adder.io_out_s, 23, 23) @[FPArithmetic.scala 155:44]
    node _E_T_2 = not(_E_T_1) @[FPArithmetic.scala 155:29]
    node _E_T_3 = and(_E_T, _E_T_2) @[FPArithmetic.scala 155:27]
    node _E_T_4 = not(adder.io_out_c) @[FPArithmetic.scala 155:59]
    node _E_T_5 = xor(sign_0, sign_1) @[FPArithmetic.scala 155:87]
    node _E_T_6 = not(_E_T_5) @[FPArithmetic.scala 155:77]
    node _E_T_7 = and(_E_T_4, _E_T_6) @[FPArithmetic.scala 155:75]
    node _E_T_8 = or(_E_T_3, _E_T_7) @[FPArithmetic.scala 155:56]
    node _E_T_9 = bits(adder.io_out_s, 23, 23) @[FPArithmetic.scala 155:133]
    node _E_T_10 = and(adder.io_out_c, _E_T_9) @[FPArithmetic.scala 155:117]
    node _E_T_11 = xor(sign_0, sign_1) @[FPArithmetic.scala 155:155]
    node _E_T_12 = and(_E_T_10, _E_T_11) @[FPArithmetic.scala 155:144]
    node _E_T_13 = or(_E_T_8, _E_T_12) @[FPArithmetic.scala 155:99]
    node _T_18 = bits(io_in_a, 30, 0) @[FPArithmetic.scala 170:17]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[FPArithmetic.scala 170:26]
    node _T_20 = bits(io_in_b, 30, 0) @[FPArithmetic.scala 170:44]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[FPArithmetic.scala 170:54]
    node _T_22 = and(_T_19, _T_21) @[FPArithmetic.scala 170:34]
    node sub_exp = _GEN_3 @[FPArithmetic.scala 69:23]
    node _T_23 = geq(sub_exp, UInt<5>("h17")) @[FPArithmetic.scala 174:25]
    node out_s = _GEN_4 @[FPArithmetic.scala 66:21]
    node _GEN_25 = mux(_T_23, out_s, _new_s_T_6) @[FPArithmetic.scala 139:11 174:39 175:13]
    node _GEN_28 = mux(_T_22, UInt<1>("h0"), _GEN_25) @[FPArithmetic.scala 170:62 171:13]
    node new_s = _GEN_28 @[FPArithmetic.scala 136:21]
    node _T_15 = eq(new_s, UInt<1>("h1")) @[FPArithmetic.scala 159:16]
    node _T_16 = neq(sign_0, sign_1) @[FPArithmetic.scala 159:35]
    node _T_17 = and(_T_15, _T_16) @[FPArithmetic.scala 159:24]
    node _GEN_12 = mux(_T_17, complementN_2.io_out, adder.io_out_s) @[FPArithmetic.scala 158:18 159:47 160:20]
    node _subber2_io_in_b_T = sub(UInt<5>("h18"), leadingOneFinder.io_out) @[FPArithmetic.scala 168:42]
    node _subber2_io_in_b_T_1 = tail(_subber2_io_in_b_T, 1) @[FPArithmetic.scala 168:42]
    node E = _E_T_13 @[FPArithmetic.scala 149:17 155:7]
    node _T_24 = eq(E, UInt<1>("h1")) @[FPArithmetic.scala 178:18]
    node adder_result = _GEN_12 @[FPArithmetic.scala 157:28]
    node _new_out_frac_T = bits(adder_result, 22, 0) @[FPArithmetic.scala 180:35]
    node D = _D_T_2 @[FPArithmetic.scala 148:17 152:7]
    node _T_25 = eq(D, UInt<1>("h0")) @[FPArithmetic.scala 181:18]
    node _T_26 = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 182:50]
    node _T_27 = tail(_T_26, 1) @[FPArithmetic.scala 182:50]
    node out_exp = _GEN_2 @[FPArithmetic.scala 68:23]
    node _T_28 = eq(out_exp, _T_27) @[FPArithmetic.scala 182:20]
    node _new_out_exp_T = sub(UInt<9>("h100"), UInt<2>("h2")) @[FPArithmetic.scala 183:50]
    node _new_out_exp_T_1 = tail(_new_out_exp_T, 1) @[FPArithmetic.scala 183:50]
    node _new_out_frac_T_1 = sub(UInt<24>("h800000"), UInt<1>("h1")) @[FPArithmetic.scala 184:51]
    node _new_out_frac_T_2 = tail(_new_out_frac_T_1, 1) @[FPArithmetic.scala 184:51]
    node _new_out_exp_T_2 = add(out_exp, UInt<1>("h1")) @[FPArithmetic.scala 186:32]
    node _new_out_exp_T_3 = tail(_new_out_exp_T_2, 1) @[FPArithmetic.scala 186:32]
    node _new_out_frac_T_3 = bits(adder_result, 23, 1) @[FPArithmetic.scala 187:37]
    node _GEN_13 = mux(_T_28, _new_out_exp_T_1, _new_out_exp_T_3) @[FPArithmetic.scala 182:56 183:21 186:21]
    node _GEN_14 = mux(_T_28, _new_out_frac_T_2, _new_out_frac_T_3) @[FPArithmetic.scala 182:56 184:22 187:22]
    node _T_29 = eq(D, UInt<1>("h1")) @[FPArithmetic.scala 189:18]
    node _T_30 = eq(leadingOneFinder.io_out, UInt<1>("h1")) @[FPArithmetic.scala 190:36]
    node _T_31 = eq(adder_result, UInt<1>("h0")) @[FPArithmetic.scala 190:60]
    node _T_32 = and(_T_30, _T_31) @[FPArithmetic.scala 190:44]
    node _T_33 = xor(sign_0, sign_1) @[FPArithmetic.scala 190:90]
    node _T_34 = eq(UInt<1>("h1"), _T_33) @[FPArithmetic.scala 190:77]
    node _T_35 = bits(io_in_a, 30, 0) @[FPArithmetic.scala 190:111]
    node _T_36 = bits(io_in_b, 30, 0) @[FPArithmetic.scala 190:130]
    node _T_37 = eq(_T_35, _T_36) @[FPArithmetic.scala 190:120]
    node _T_38 = and(_T_34, _T_37) @[FPArithmetic.scala 190:101]
    node _T_39 = and(_T_32, _T_38) @[FPArithmetic.scala 190:68]
    node _T_40 = eq(subber2.io_out_c, UInt<1>("h1")) @[FPArithmetic.scala 193:31]
    node _new_out_frac_T_4 = bits(adder_result, 22, 0) @[FPArithmetic.scala 198:39]
    node _new_out_frac_T_5 = sub(UInt<5>("h18"), leadingOneFinder.io_out) @[FPArithmetic.scala 198:78]
    node _new_out_frac_T_6 = tail(_new_out_frac_T_5, 1) @[FPArithmetic.scala 198:78]
    node _new_out_frac_T_7 = dshl(_new_out_frac_T_4, _new_out_frac_T_6) @[FPArithmetic.scala 198:57]
    node _GEN_15 = mux(_T_40, UInt<8>("h1"), subber2.io_out_s) @[FPArithmetic.scala 193:39 194:23 197:23]
    node _GEN_16 = mux(_T_40, UInt<23>("h400000"), _new_out_frac_T_7) @[FPArithmetic.scala 193:39 195:24 198:24]
    node _GEN_17 = mux(_T_39, UInt<1>("h0"), _GEN_15) @[FPArithmetic.scala 190:141 191:21]
    node _GEN_18 = mux(_T_39, UInt<1>("h0"), _GEN_16) @[FPArithmetic.scala 190:141 140:18]
    node _GEN_19 = mux(_T_29, _GEN_17, UInt<1>("h0")) @[FPArithmetic.scala 141:17 189:26]
    node _GEN_20 = mux(_T_29, _GEN_18, UInt<1>("h0")) @[FPArithmetic.scala 140:18 189:26]
    node _GEN_21 = mux(_T_25, _GEN_13, _GEN_19) @[FPArithmetic.scala 181:26]
    node _GEN_22 = mux(_T_25, _GEN_14, _GEN_20) @[FPArithmetic.scala 181:26]
    node _GEN_23 = mux(_T_24, out_exp, _GEN_21) @[FPArithmetic.scala 178:26 179:19]
    node _GEN_24 = mux(_T_24, _new_out_frac_T, _GEN_22) @[FPArithmetic.scala 178:26 180:20]
    node out_frac = _GEN_5 @[FPArithmetic.scala 67:24]
    node _GEN_26 = mux(_T_23, out_frac, _GEN_24) @[FPArithmetic.scala 174:39 176:20]
    node _GEN_27 = mux(_T_23, out_exp, _GEN_23) @[FPArithmetic.scala 174:39 177:19]
    node _GEN_29 = mux(_T_22, UInt<1>("h0"), _GEN_27) @[FPArithmetic.scala 170:62 172:19]
    node _GEN_30 = mux(_T_22, UInt<1>("h0"), _GEN_26) @[FPArithmetic.scala 170:62 173:20]
    reg reg_out_s : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_out_s) @[FPArithmetic.scala 202:28]
    node new_out_exp = bits(_GEN_29, 7, 0) @[FPArithmetic.scala 138:27]
    node _reg_out_s_T = cat(new_s, new_out_exp) @[FPArithmetic.scala 204:24]
    node new_out_frac = bits(_GEN_30, 22, 0) @[FPArithmetic.scala 137:28]
    node _reg_out_s_T_1 = cat(_reg_out_s_T, new_out_frac) @[FPArithmetic.scala 204:39]
    io_out_s <= reg_out_s @[FPArithmetic.scala 206:14]
    subber.clock <= clock
    subber.reset <= reset
    subber.io_in_a <= exp_0 @[FPArithmetic.scala 78:20]
    subber.io_in_b <= exp_1 @[FPArithmetic.scala 79:20]
    subber.io_in_c <= UInt<1>("h0") @[FPArithmetic.scala 80:20]
    complement.clock <= clock
    complement.reset <= reset
    complement.io_in <= subber.io_out_s @[FPArithmetic.scala 84:22]
    adder.clock <= clock
    adder.reset <= reset
    adder.io_in_a <= _GEN_10
    adder.io_in_b <= _GEN_11
    adder.io_in_c <= UInt<1>("h0") @[FPArithmetic.scala 90:19]
    complementN_0.clock <= clock
    complementN_0.reset <= reset
    complementN_0.io_in <= _GEN_8
    complementN_1.clock <= clock
    complementN_1.reset <= reset
    complementN_1.io_in <= _GEN_9
    shifter.clock <= clock
    shifter.reset <= reset
    shifter.io_in_a <= _GEN_6
    shifter.io_in_b <= bits(_GEN_3, 4, 0)
    shifter.io_in_c <= _GEN_7
    complementN_2.clock <= clock
    complementN_2.reset <= reset
    complementN_2.io_in <= adder.io_out_s @[FPArithmetic.scala 145:25]
    leadingOneFinder.clock <= clock
    leadingOneFinder.reset <= reset
    leadingOneFinder.io_in <= adder_result @[FPArithmetic.scala 165:28]
    subber2.clock <= clock
    subber2.reset <= reset
    subber2.io_in_a <= out_exp @[FPArithmetic.scala 167:21]
    subber2.io_in_b <= pad(_subber2_io_in_b_T_1, 8) @[FPArithmetic.scala 168:21]
    subber2.io_in_c <= UInt<1>("h0") @[FPArithmetic.scala 169:21]
    reg_out_s <= mux(reset, UInt<32>("h0"), _reg_out_s_T_1) @[FPArithmetic.scala 202:{28,28} 204:15]

  module FP_dDot_2 :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a_0 : UInt<32>
    input io_in_a_1 : UInt<32>
    input io_in_b_0 : UInt<32>
    input io_in_b_1 : UInt<32>
    output io_out_s : UInt<32>

    inst FP_multiplier of FP_multiplier @[house_HolderQR.scala 401:26]
    inst FP_multiplier_1 of FP_multiplier @[house_HolderQR.scala 401:26]
    inst FP_adder of FP_adder @[house_HolderQR.scala 405:23]
    io_out_s <= FP_adder.io_out_s @[house_HolderQR.scala 508:12]
    FP_multiplier.clock <= clock
    FP_multiplier.reset <= reset
    FP_multiplier.io_in_a <= io_in_a_0 @[house_HolderQR.scala 412:28]
    FP_multiplier.io_in_b <= io_in_b_0 @[house_HolderQR.scala 413:28]
    FP_multiplier_1.clock <= clock
    FP_multiplier_1.reset <= reset
    FP_multiplier_1.io_in_a <= io_in_a_1 @[house_HolderQR.scala 412:28]
    FP_multiplier_1.io_in_b <= io_in_b_1 @[house_HolderQR.scala 413:28]
    FP_adder.clock <= clock
    FP_adder.reset <= reset
    FP_adder.io_in_a <= FP_multiplier.io_out_s @[house_HolderQR.scala 424:38]
    FP_adder.io_in_b <= FP_multiplier_1.io_out_s @[house_HolderQR.scala 425:38]

  module FP_subber :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b : UInt<32>
    output io_out_s : UInt<32>

    inst FP_adder of FP_adder @[FPArithmetic.scala 726:26]
    node _adjusted_in_b_T = bits(io_in_b, 31, 31) @[FPArithmetic.scala 729:31]
    node _adjusted_in_b_T_1 = not(_adjusted_in_b_T) @[FPArithmetic.scala 729:23]
    node _adjusted_in_b_T_2 = bits(io_in_b, 30, 0) @[FPArithmetic.scala 729:49]
    node _adjusted_in_b_T_3 = cat(_adjusted_in_b_T_1, _adjusted_in_b_T_2) @[FPArithmetic.scala 729:39]
    node adjusted_in_b = _adjusted_in_b_T_3
    io_out_s <= FP_adder.io_out_s @[FPArithmetic.scala 732:14]
    FP_adder.clock <= clock
    FP_adder.reset <= reset
    FP_adder.io_in_a <= io_in_a @[FPArithmetic.scala 730:22]
    FP_adder.io_in_b <= adjusted_in_b @[FPArithmetic.scala 731:22]

  module FP_inverse_square_root :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    output io_out_s : UInt<32>

    inst multiplier1 of FP_multiplier @[FPArithmetic.scala 687:29]
    inst multiplier2 of FP_multiplier @[FPArithmetic.scala 690:29]
    inst sub of FP_subber @[FPArithmetic.scala 694:21]
    inst multiplier3 of FP_multiplier @[FPArithmetic.scala 698:29]
    node _T = bits(io_in_a, 30, 0) @[FPArithmetic.scala 663:17]
    node _T_1 = gt(_T, UInt<31>("h7ef477d4")) @[FPArithmetic.scala 663:27]
    node _number_T = bits(io_in_a, 30, 0) @[FPArithmetic.scala 666:24]
    node _number_T_1 = dshr(_number_T, UInt<1>("h1")) @[FPArithmetic.scala 666:34]
    node _GEN_0 = mux(_T_1, UInt<30>("h3f7a3bea"), _number_T_1) @[FPArithmetic.scala 663:41 664:14 666:14]
    node number = pad(_GEN_0, 32) @[FPArithmetic.scala 660:22]
    node _result_T = sub(UInt<32>("h5f3759df"), number) @[FPArithmetic.scala 679:25]
    node _result_T_1 = tail(_result_T, 1) @[FPArithmetic.scala 679:25]
    node _exp_T = bits(io_in_a, 30, 23) @[FPArithmetic.scala 682:19]
    node _exp_T_1 = sub(_exp_T, UInt<1>("h1")) @[FPArithmetic.scala 682:35]
    node _exp_T_2 = tail(_exp_T_1, 1) @[FPArithmetic.scala 682:35]
    node _half_input_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 685:26]
    node exp = _exp_T_2 @[FPArithmetic.scala 681:19 682:9]
    node _half_input_T_1 = cat(_half_input_T, exp) @[FPArithmetic.scala 685:33]
    node _half_input_T_2 = bits(io_in_a, 22, 0) @[FPArithmetic.scala 685:50]
    node _half_input_T_3 = cat(_half_input_T_1, _half_input_T_2) @[FPArithmetic.scala 685:40]
    node result = _result_T_1 @[FPArithmetic.scala 678:22 679:12]
    node _multiplier1_io_in_a_T = bits(result, 30, 0) @[FPArithmetic.scala 688:46]
    node _multiplier1_io_in_a_T_1 = cat(UInt<1>("h0"), _multiplier1_io_in_a_T) @[FPArithmetic.scala 688:37]
    node _multiplier1_io_in_b_T = bits(result, 30, 0) @[FPArithmetic.scala 689:46]
    node _multiplier1_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier1_io_in_b_T) @[FPArithmetic.scala 689:37]
    node half_input = _half_input_T_3 @[FPArithmetic.scala 684:26 685:16]
    node _multiplier2_io_in_b_T = bits(half_input, 30, 0) @[FPArithmetic.scala 692:50]
    node _multiplier2_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier2_io_in_b_T) @[FPArithmetic.scala 692:37]
    node _multiplier3_io_in_b_T = bits(result, 30, 0) @[FPArithmetic.scala 700:46]
    node _multiplier3_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier3_io_in_b_T) @[FPArithmetic.scala 700:37]
    node _io_out_s_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 702:24]
    node _io_out_s_T_1 = bits(multiplier3.io_out_s, 30, 0) @[FPArithmetic.scala 702:54]
    node _io_out_s_T_2 = cat(_io_out_s_T, _io_out_s_T_1) @[FPArithmetic.scala 702:31]
    node threehalfs = UInt<32>("h3fc00000") @[FPArithmetic.scala 661:26 662:16]
    io_out_s <= _io_out_s_T_2 @[FPArithmetic.scala 702:14]
    multiplier1.clock <= clock
    multiplier1.reset <= reset
    multiplier1.io_in_a <= _multiplier1_io_in_a_T_1 @[FPArithmetic.scala 688:25]
    multiplier1.io_in_b <= _multiplier1_io_in_b_T_1 @[FPArithmetic.scala 689:25]
    multiplier2.clock <= clock
    multiplier2.reset <= reset
    multiplier2.io_in_a <= multiplier1.io_out_s @[FPArithmetic.scala 691:25]
    multiplier2.io_in_b <= _multiplier2_io_in_b_T_1 @[FPArithmetic.scala 692:25]
    sub.clock <= clock
    sub.reset <= reset
    sub.io_in_a <= threehalfs @[FPArithmetic.scala 695:17]
    sub.io_in_b <= multiplier2.io_out_s @[FPArithmetic.scala 696:17]
    multiplier3.clock <= clock
    multiplier3.reset <= reset
    multiplier3.io_in_a <= sub.io_out_s @[FPArithmetic.scala 699:25]
    multiplier3.io_in_b <= _multiplier3_io_in_b_T_1 @[FPArithmetic.scala 700:25]

  module FP_reciprocal :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    output io_out_s : UInt<32>

    inst multiplier1 of FP_multiplier @[FPArithmetic.scala 464:29]
    inst multiplier2 of FP_multiplier @[FPArithmetic.scala 467:29]
    inst sub of FP_subber @[FPArithmetic.scala 471:21]
    inst multiplier3 of FP_multiplier @[FPArithmetic.scala 475:29]
    inst multiplier4 of FP_multiplier @[FPArithmetic.scala 480:29]
    node _T = bits(io_in_a, 30, 0) @[FPArithmetic.scala 440:17]
    node _T_1 = gt(_T, UInt<31>("h7ef477d4")) @[FPArithmetic.scala 440:27]
    node _number_T = bits(io_in_a, 30, 0) @[FPArithmetic.scala 443:24]
    node _number_T_1 = dshr(_number_T, UInt<1>("h1")) @[FPArithmetic.scala 443:34]
    node _GEN_0 = mux(_T_1, UInt<30>("h3f7a3bea"), _number_T_1) @[FPArithmetic.scala 440:41 441:14 443:14]
    node number = pad(_GEN_0, 32) @[FPArithmetic.scala 437:22]
    node _result_T = sub(UInt<32>("h5f3759df"), number) @[FPArithmetic.scala 456:25]
    node _result_T_1 = tail(_result_T, 1) @[FPArithmetic.scala 456:25]
    node _exp_T = bits(io_in_a, 30, 23) @[FPArithmetic.scala 459:19]
    node _exp_T_1 = sub(_exp_T, UInt<1>("h1")) @[FPArithmetic.scala 459:35]
    node _exp_T_2 = tail(_exp_T_1, 1) @[FPArithmetic.scala 459:35]
    node _half_input_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 462:26]
    node exp = _exp_T_2 @[FPArithmetic.scala 458:19 459:9]
    node _half_input_T_1 = cat(_half_input_T, exp) @[FPArithmetic.scala 462:33]
    node _half_input_T_2 = bits(io_in_a, 22, 0) @[FPArithmetic.scala 462:50]
    node _half_input_T_3 = cat(_half_input_T_1, _half_input_T_2) @[FPArithmetic.scala 462:40]
    node result = _result_T_1 @[FPArithmetic.scala 455:22 456:12]
    node _multiplier1_io_in_a_T = bits(result, 30, 0) @[FPArithmetic.scala 465:46]
    node _multiplier1_io_in_a_T_1 = cat(UInt<1>("h0"), _multiplier1_io_in_a_T) @[FPArithmetic.scala 465:37]
    node _multiplier1_io_in_b_T = bits(result, 30, 0) @[FPArithmetic.scala 466:46]
    node _multiplier1_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier1_io_in_b_T) @[FPArithmetic.scala 466:37]
    node half_input = _half_input_T_3 @[FPArithmetic.scala 461:26 462:16]
    node _multiplier2_io_in_b_T = bits(half_input, 30, 0) @[FPArithmetic.scala 469:50]
    node _multiplier2_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier2_io_in_b_T) @[FPArithmetic.scala 469:37]
    node _multiplier3_io_in_b_T = bits(result, 30, 0) @[FPArithmetic.scala 477:46]
    node _multiplier3_io_in_b_T_1 = cat(UInt<1>("h0"), _multiplier3_io_in_b_T) @[FPArithmetic.scala 477:37]
    node _multiplier4_io_in_a_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 481:35]
    node _multiplier4_io_in_a_T_1 = bits(multiplier3.io_out_s, 30, 0) @[FPArithmetic.scala 481:65]
    node _multiplier4_io_in_a_T_2 = cat(_multiplier4_io_in_a_T, _multiplier4_io_in_a_T_1) @[FPArithmetic.scala 481:42]
    node _io_out_s_T = bits(io_in_a, 31, 31) @[FPArithmetic.scala 484:24]
    node _io_out_s_T_1 = bits(multiplier4.io_out_s, 30, 0) @[FPArithmetic.scala 484:54]
    node _io_out_s_T_2 = cat(_io_out_s_T, _io_out_s_T_1) @[FPArithmetic.scala 484:31]
    node threehalfs = UInt<32>("h3fc00000") @[FPArithmetic.scala 438:26 439:16]
    io_out_s <= _io_out_s_T_2 @[FPArithmetic.scala 484:14]
    multiplier1.clock <= clock
    multiplier1.reset <= reset
    multiplier1.io_in_a <= _multiplier1_io_in_a_T_1 @[FPArithmetic.scala 465:25]
    multiplier1.io_in_b <= _multiplier1_io_in_b_T_1 @[FPArithmetic.scala 466:25]
    multiplier2.clock <= clock
    multiplier2.reset <= reset
    multiplier2.io_in_a <= multiplier1.io_out_s @[FPArithmetic.scala 468:25]
    multiplier2.io_in_b <= _multiplier2_io_in_b_T_1 @[FPArithmetic.scala 469:25]
    sub.clock <= clock
    sub.reset <= reset
    sub.io_in_a <= threehalfs @[FPArithmetic.scala 472:17]
    sub.io_in_b <= multiplier2.io_out_s @[FPArithmetic.scala 473:17]
    multiplier3.clock <= clock
    multiplier3.reset <= reset
    multiplier3.io_in_a <= sub.io_out_s @[FPArithmetic.scala 476:25]
    multiplier3.io_in_b <= _multiplier3_io_in_b_T_1 @[FPArithmetic.scala 477:25]
    multiplier4.clock <= clock
    multiplier4.reset <= reset
    multiplier4.io_in_a <= _multiplier4_io_in_a_T_2 @[FPArithmetic.scala 481:25]
    multiplier4.io_in_b <= multiplier3.io_out_s @[FPArithmetic.scala 482:25]

  module FP_square_root :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    output io_out_s : UInt<32>

    inst FP_inverse_square_root of FP_inverse_square_root @[FPArithmetic.scala 710:24]
    inst FP_reciprocal of FP_reciprocal @[FPArithmetic.scala 711:28]
    io_out_s <= FP_reciprocal.io_out_s @[FPArithmetic.scala 714:14]
    FP_inverse_square_root.clock <= clock
    FP_inverse_square_root.reset <= reset
    FP_inverse_square_root.io_in_a <= io_in_a @[FPArithmetic.scala 712:17]
    FP_reciprocal.clock <= clock
    FP_reciprocal.reset <= reset
    FP_reciprocal.io_in_a <= FP_inverse_square_root.io_out_s @[FPArithmetic.scala 713:21]

  module sign_Calculator :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b : UInt<32>
    output io_out_s : UInt<32>

    inst FP_adder of FP_adder @[house_HolderQR.scala 516:21]
    inst FP_subber of FP_subber @[house_HolderQR.scala 517:22]
    node _T = geq(io_in_a, UInt<32>("h80000000")) @[house_HolderQR.scala 525:16]
    node _GEN_0 = mux(_T, FP_subber.io_out_s, FP_adder.io_out_s) @[house_HolderQR.scala 525:57 526:14 530:16]
    io_out_s <= _GEN_0
    FP_adder.clock <= clock
    FP_adder.reset <= reset
    FP_adder.io_in_a <= io_in_a @[house_HolderQR.scala 520:14]
    FP_adder.io_in_b <= io_in_b @[house_HolderQR.scala 521:14]
    FP_subber.clock <= clock
    FP_subber.reset <= reset
    FP_subber.io_in_a <= io_in_a @[house_HolderQR.scala 522:15]
    FP_subber.io_in_b <= io_in_b @[house_HolderQR.scala 523:15]

  module FP_divider :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b : UInt<32>
    output io_out_s : UInt<32>

    inst FP_reciprocal of FP_reciprocal @[FPArithmetic.scala 621:28]
    inst FP_multiplier of FP_multiplier @[FPArithmetic.scala 622:28]
    io_out_s <= FP_multiplier.io_out_s @[FPArithmetic.scala 626:14]
    FP_reciprocal.clock <= clock
    FP_reciprocal.reset <= reset
    FP_reciprocal.io_in_a <= io_in_b @[FPArithmetic.scala 623:21]
    FP_multiplier.clock <= clock
    FP_multiplier.reset <= reset
    FP_multiplier.io_in_a <= io_in_a @[FPArithmetic.scala 624:21]
    FP_multiplier.io_in_b <= FP_reciprocal.io_out_s @[FPArithmetic.scala 625:21]

  module reg_arrays :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a_0 : UInt<32>
    input io_in_a_1 : UInt<32>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    reg reg_array_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 590:24]
    reg reg_array_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 590:24]
    reg reg_array_h_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_2) @[house_HolderQR.scala 590:24]
    reg reg_array_h_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_3) @[house_HolderQR.scala 590:24]
    reg reg_array_h_4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_4) @[house_HolderQR.scala 590:24]
    reg reg_array_h_5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_5) @[house_HolderQR.scala 590:24]
    reg reg_array_h_6 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_6) @[house_HolderQR.scala 590:24]
    reg reg_array_h_7 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_7) @[house_HolderQR.scala 590:24]
    reg reg_array_h_8 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_8) @[house_HolderQR.scala 590:24]
    reg reg_array_h_9 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_9) @[house_HolderQR.scala 590:24]
    reg reg_array_h_10 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_10) @[house_HolderQR.scala 590:24]
    reg reg_array_h_11 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_11) @[house_HolderQR.scala 590:24]
    reg reg_array_h_12 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_12) @[house_HolderQR.scala 590:24]
    reg reg_array_h_13 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_13) @[house_HolderQR.scala 590:24]
    io_out_s_0 <= reg_array_h_3 @[house_HolderQR.scala 598:15]
    io_out_s_1 <= reg_array_h_10 @[house_HolderQR.scala 606:15]
    reg_array_h_0 <= io_in_a_0 @[house_HolderQR.scala 591:18]
    reg_array_h_1 <= reg_array_h_0 @[house_HolderQR.scala 592:18]
    reg_array_h_2 <= reg_array_h_1 @[house_HolderQR.scala 593:18]
    reg_array_h_3 <= reg_array_h_2 @[house_HolderQR.scala 594:18]
    reg_array_h_4 <= reg_array_h_3 @[house_HolderQR.scala 595:18]
    reg_array_h_5 <= reg_array_h_4 @[house_HolderQR.scala 596:18]
    reg_array_h_6 <= reg_array_h_5 @[house_HolderQR.scala 597:18]
    reg_array_h_7 <= io_in_a_1 @[house_HolderQR.scala 599:18]
    reg_array_h_8 <= reg_array_h_7 @[house_HolderQR.scala 600:18]
    reg_array_h_9 <= reg_array_h_8 @[house_HolderQR.scala 601:18]
    reg_array_h_10 <= reg_array_h_9 @[house_HolderQR.scala 602:19]
    reg_array_h_11 <= reg_array_h_10 @[house_HolderQR.scala 603:19]
    reg_array_h_12 <= reg_array_h_11 @[house_HolderQR.scala 604:19]
    reg_array_h_13 <= reg_array_h_12 @[house_HolderQR.scala 605:19]

  module axpy :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a : UInt<32>
    input io_in_b_0 : UInt<32>
    input io_in_b_1 : UInt<32>
    input io_in_c_0 : UInt<32>
    input io_in_c_1 : UInt<32>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    inst FP_multiplier of FP_multiplier @[house_HolderQR.scala 560:26]
    inst FP_multiplier_1 of FP_multiplier @[house_HolderQR.scala 560:26]
    inst FP_adder of FP_adder @[house_HolderQR.scala 564:23]
    inst FP_adder_1 of FP_adder @[house_HolderQR.scala 564:23]
    io_out_s_0 <= FP_adder.io_out_s @[house_HolderQR.scala 579:13]
    io_out_s_1 <= FP_adder_1.io_out_s @[house_HolderQR.scala 579:13]
    FP_multiplier.clock <= clock
    FP_multiplier.reset <= reset
    FP_multiplier.io_in_a <= io_in_a @[house_HolderQR.scala 569:28]
    FP_multiplier.io_in_b <= io_in_b_0 @[house_HolderQR.scala 570:28]
    FP_multiplier_1.clock <= clock
    FP_multiplier_1.reset <= reset
    FP_multiplier_1.io_in_a <= io_in_a @[house_HolderQR.scala 569:28]
    FP_multiplier_1.io_in_b <= io_in_b_1 @[house_HolderQR.scala 570:28]
    FP_adder.clock <= clock
    FP_adder.reset <= reset
    FP_adder.io_in_a <= FP_multiplier.io_out_s @[house_HolderQR.scala 573:25]
    FP_adder.io_in_b <= io_in_c_0 @[house_HolderQR.scala 574:25]
    FP_adder_1.clock <= clock
    FP_adder_1.reset <= reset
    FP_adder_1.io_in_a <= FP_multiplier_1.io_out_s @[house_HolderQR.scala 573:25]
    FP_adder_1.io_in_b <= io_in_c_1 @[house_HolderQR.scala 574:25]

  module house_HolderQR :
    input clock : Clock
    input reset : UInt<1>
    input io_col_1_0 : UInt<32>
    input io_col_1_1 : UInt<32>
    input io_reg_array_0 : UInt<32>
    input io_reg_array_1 : UInt<32>
    input io_reg_arrays_0 : UInt<32>
    input io_reg_arrays_1 : UInt<32>
    input io_Tk : UInt<1>
    input io_Vk : UInt<1>
    input io_Tr : UInt<1>
    input io_h_count : UInt<32>
    output io_out_test : UInt<32>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    inst dDot of FP_dDot_2 @[house_HolderQR.scala 264:20]
    inst sqrt of FP_square_root @[house_HolderQR.scala 316:22]
    inst sign of sign_Calculator @[house_HolderQR.scala 317:22]
    inst divide of FP_divider @[house_HolderQR.scala 327:24]
    inst reg_array of reg_arrays @[house_HolderQR.scala 332:27]
    inst d5_M of FP_multiplier @[house_HolderQR.scala 335:22]
    inst axpy of axpy @[house_HolderQR.scala 336:22]
    reg reg_array_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 251:24]
    reg reg_array_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 251:24]
    reg reg_arrays_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_arrays_h_0) @[house_HolderQR.scala 252:25]
    reg reg_arrays_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_arrays_h_1) @[house_HolderQR.scala 252:25]
    reg reg_arrays_h_b_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_arrays_h_b_0) @[house_HolderQR.scala 253:27]
    reg reg_arrays_h_b_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_arrays_h_b_1) @[house_HolderQR.scala 253:27]
    reg reg_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_1) @[house_HolderQR.scala 254:18]
    reg reg_2_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_2_0) @[house_HolderQR.scala 255:18]
    reg reg_2_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_2_1) @[house_HolderQR.scala 255:18]
    reg tk : UInt<32>, clock with :
      reset => (UInt<1>("h0"), tk) @[house_HolderQR.scala 256:15]
    reg test : UInt<32>, clock with :
      reset => (UInt<1>("h0"), test) @[house_HolderQR.scala 265:17]
    node _T = eq(io_h_count, UInt<1>("h0")) @[house_HolderQR.scala 274:19]
    node _T_1 = eq(io_h_count, UInt<1>("h1")) @[house_HolderQR.scala 274:19]
    node _T_2 = eq(io_h_count, UInt<2>("h2")) @[house_HolderQR.scala 274:19]
    node _T_3 = eq(io_h_count, UInt<2>("h3")) @[house_HolderQR.scala 274:19]
    node _T_4 = eq(io_h_count, UInt<3>("h4")) @[house_HolderQR.scala 274:19]
    node _T_5 = eq(io_h_count, UInt<3>("h5")) @[house_HolderQR.scala 274:19]
    node _T_6 = eq(io_h_count, UInt<3>("h6")) @[house_HolderQR.scala 274:19]
    node _T_7 = eq(io_h_count, UInt<3>("h7")) @[house_HolderQR.scala 274:19]
    node _T_8 = eq(io_h_count, UInt<4>("h8")) @[house_HolderQR.scala 274:19]
    node _T_9 = eq(io_h_count, UInt<4>("h9")) @[house_HolderQR.scala 274:19]
    node _T_10 = eq(io_h_count, UInt<4>("ha")) @[house_HolderQR.scala 274:19]
    node _T_11 = eq(io_h_count, UInt<4>("hb")) @[house_HolderQR.scala 274:19]
    node _T_12 = eq(io_h_count, UInt<4>("hc")) @[house_HolderQR.scala 274:19]
    node _T_13 = eq(io_h_count, UInt<4>("hd")) @[house_HolderQR.scala 274:19]
    node _T_14 = eq(io_h_count, UInt<4>("he")) @[house_HolderQR.scala 274:19]
    node _T_15 = eq(io_h_count, UInt<4>("hf")) @[house_HolderQR.scala 274:19]
    node _T_16 = eq(io_h_count, UInt<5>("h10")) @[house_HolderQR.scala 274:19]
    node _T_17 = eq(io_h_count, UInt<5>("h11")) @[house_HolderQR.scala 274:19]
    node _T_18 = eq(io_h_count, UInt<5>("h12")) @[house_HolderQR.scala 274:19]
    node _T_19 = eq(io_h_count, UInt<5>("h13")) @[house_HolderQR.scala 274:19]
    node _T_20 = eq(io_h_count, UInt<5>("h14")) @[house_HolderQR.scala 274:19]
    node _T_21 = eq(io_h_count, UInt<5>("h15")) @[house_HolderQR.scala 274:19]
    node _T_22 = eq(io_h_count, UInt<5>("h16")) @[house_HolderQR.scala 274:19]
    node _T_23 = eq(io_h_count, UInt<5>("h17")) @[house_HolderQR.scala 274:19]
    node _T_24 = eq(io_h_count, UInt<5>("h18")) @[house_HolderQR.scala 274:19]
    node _T_25 = eq(io_h_count, UInt<5>("h19")) @[house_HolderQR.scala 274:19]
    node _T_26 = eq(io_h_count, UInt<5>("h1a")) @[house_HolderQR.scala 274:19]
    node _T_27 = eq(io_h_count, UInt<5>("h1b")) @[house_HolderQR.scala 274:19]
    node _T_28 = eq(io_h_count, UInt<5>("h1c")) @[house_HolderQR.scala 274:19]
    node _T_29 = eq(io_h_count, UInt<5>("h1d")) @[house_HolderQR.scala 274:19]
    node _T_30 = eq(io_h_count, UInt<5>("h1e")) @[house_HolderQR.scala 274:19]
    node _T_31 = eq(io_h_count, UInt<5>("h1f")) @[house_HolderQR.scala 274:19]
    node _T_32 = eq(io_h_count, UInt<6>("h20")) @[house_HolderQR.scala 274:19]
    node _T_33 = eq(io_h_count, UInt<6>("h21")) @[house_HolderQR.scala 274:19]
    node _T_34 = eq(io_h_count, UInt<6>("h22")) @[house_HolderQR.scala 274:19]
    node _T_35 = eq(io_h_count, UInt<6>("h23")) @[house_HolderQR.scala 274:19]
    node _T_36 = eq(io_h_count, UInt<6>("h24")) @[house_HolderQR.scala 274:19]
    node _T_37 = eq(io_h_count, UInt<6>("h25")) @[house_HolderQR.scala 274:19]
    node _T_38 = eq(io_h_count, UInt<6>("h26")) @[house_HolderQR.scala 274:19]
    node _T_39 = eq(io_h_count, UInt<6>("h27")) @[house_HolderQR.scala 274:19]
    node _T_40 = eq(io_h_count, UInt<6>("h28")) @[house_HolderQR.scala 274:19]
    node _T_41 = eq(io_h_count, UInt<6>("h29")) @[house_HolderQR.scala 274:19]
    node _T_42 = eq(io_h_count, UInt<6>("h2a")) @[house_HolderQR.scala 274:19]
    node _T_43 = eq(io_h_count, UInt<6>("h2b")) @[house_HolderQR.scala 274:19]
    node _T_44 = eq(io_h_count, UInt<6>("h2c")) @[house_HolderQR.scala 274:19]
    node _T_45 = eq(io_h_count, UInt<6>("h2d")) @[house_HolderQR.scala 274:19]
    node _T_46 = eq(io_h_count, UInt<6>("h2e")) @[house_HolderQR.scala 274:19]
    node _T_47 = eq(io_h_count, UInt<6>("h2f")) @[house_HolderQR.scala 274:19]
    node _T_48 = eq(io_h_count, UInt<6>("h30")) @[house_HolderQR.scala 274:19]
    node _T_49 = eq(io_h_count, UInt<6>("h31")) @[house_HolderQR.scala 274:19]
    node _T_50 = eq(io_h_count, UInt<6>("h32")) @[house_HolderQR.scala 274:19]
    node _T_51 = eq(io_h_count, UInt<6>("h33")) @[house_HolderQR.scala 274:19]
    node _T_52 = eq(io_h_count, UInt<6>("h34")) @[house_HolderQR.scala 274:19]
    node _T_53 = eq(io_h_count, UInt<6>("h35")) @[house_HolderQR.scala 274:19]
    node _T_54 = eq(io_h_count, UInt<6>("h36")) @[house_HolderQR.scala 274:19]
    node _T_55 = eq(io_h_count, UInt<6>("h37")) @[house_HolderQR.scala 274:19]
    node _T_56 = eq(io_h_count, UInt<6>("h38")) @[house_HolderQR.scala 274:19]
    node _T_57 = eq(io_h_count, UInt<6>("h39")) @[house_HolderQR.scala 274:19]
    node _T_58 = eq(io_h_count, UInt<6>("h3a")) @[house_HolderQR.scala 274:19]
    node _T_59 = eq(io_h_count, UInt<6>("h3b")) @[house_HolderQR.scala 274:19]
    node _T_60 = eq(io_h_count, UInt<6>("h3c")) @[house_HolderQR.scala 274:19]
    node _T_61 = eq(io_h_count, UInt<6>("h3d")) @[house_HolderQR.scala 274:19]
    node _T_62 = eq(io_h_count, UInt<6>("h3e")) @[house_HolderQR.scala 274:19]
    node _T_63 = eq(io_h_count, UInt<6>("h3f")) @[house_HolderQR.scala 274:19]
    node _T_64 = eq(io_h_count, UInt<7>("h40")) @[house_HolderQR.scala 274:19]
    node _T_65 = eq(io_h_count, UInt<7>("h41")) @[house_HolderQR.scala 274:19]
    node _T_66 = eq(io_h_count, UInt<7>("h42")) @[house_HolderQR.scala 274:19]
    node _T_67 = eq(io_h_count, UInt<7>("h43")) @[house_HolderQR.scala 274:19]
    node _T_68 = eq(io_h_count, UInt<7>("h44")) @[house_HolderQR.scala 274:19]
    node _T_69 = eq(io_h_count, UInt<7>("h45")) @[house_HolderQR.scala 274:19]
    node _T_70 = eq(io_h_count, UInt<7>("h46")) @[house_HolderQR.scala 274:19]
    node _T_71 = eq(io_h_count, UInt<7>("h47")) @[house_HolderQR.scala 274:19]
    node _T_72 = eq(io_h_count, UInt<7>("h48")) @[house_HolderQR.scala 274:19]
    node _T_73 = eq(io_h_count, UInt<7>("h49")) @[house_HolderQR.scala 274:19]
    node _T_74 = eq(io_h_count, UInt<7>("h4a")) @[house_HolderQR.scala 274:19]
    node _T_75 = eq(io_h_count, UInt<7>("h4b")) @[house_HolderQR.scala 274:19]
    node _T_76 = eq(io_h_count, UInt<7>("h4c")) @[house_HolderQR.scala 274:19]
    node _T_77 = eq(io_h_count, UInt<7>("h4d")) @[house_HolderQR.scala 274:19]
    node _T_78 = eq(io_h_count, UInt<7>("h4e")) @[house_HolderQR.scala 274:19]
    node _T_79 = eq(io_h_count, UInt<7>("h4f")) @[house_HolderQR.scala 274:19]
    node _T_80 = eq(io_h_count, UInt<7>("h50")) @[house_HolderQR.scala 274:19]
    node _T_81 = eq(io_h_count, UInt<7>("h51")) @[house_HolderQR.scala 274:19]
    node _T_82 = eq(io_h_count, UInt<7>("h52")) @[house_HolderQR.scala 274:19]
    node _T_83 = eq(io_h_count, UInt<7>("h53")) @[house_HolderQR.scala 274:19]
    node _T_84 = eq(io_h_count, UInt<7>("h54")) @[house_HolderQR.scala 274:19]
    node _T_85 = eq(io_h_count, UInt<7>("h55")) @[house_HolderQR.scala 274:19]
    node _T_86 = eq(io_h_count, UInt<7>("h56")) @[house_HolderQR.scala 274:19]
    node _T_87 = eq(io_h_count, UInt<7>("h57")) @[house_HolderQR.scala 274:19]
    node _T_88 = eq(io_h_count, UInt<7>("h58")) @[house_HolderQR.scala 274:19]
    node _T_89 = eq(io_h_count, UInt<7>("h59")) @[house_HolderQR.scala 274:19]
    node _T_90 = eq(io_h_count, UInt<7>("h5a")) @[house_HolderQR.scala 274:19]
    node _T_91 = eq(io_h_count, UInt<7>("h5b")) @[house_HolderQR.scala 274:19]
    node _T_92 = eq(io_h_count, UInt<7>("h5c")) @[house_HolderQR.scala 274:19]
    node _T_93 = eq(io_h_count, UInt<7>("h5d")) @[house_HolderQR.scala 274:19]
    node _T_94 = eq(io_h_count, UInt<7>("h5e")) @[house_HolderQR.scala 274:19]
    node _T_95 = eq(io_h_count, UInt<7>("h5f")) @[house_HolderQR.scala 274:19]
    node _T_96 = eq(io_h_count, UInt<7>("h60")) @[house_HolderQR.scala 274:19]
    node _T_97 = eq(io_h_count, UInt<7>("h61")) @[house_HolderQR.scala 274:19]
    node _T_98 = eq(io_h_count, UInt<7>("h62")) @[house_HolderQR.scala 274:19]
    node _T_99 = eq(io_h_count, UInt<7>("h63")) @[house_HolderQR.scala 274:19]
    node _T_100 = eq(io_h_count, UInt<7>("h64")) @[house_HolderQR.scala 274:19]
    node _T_101 = eq(io_h_count, UInt<7>("h65")) @[house_HolderQR.scala 274:19]
    node _T_102 = eq(io_h_count, UInt<7>("h66")) @[house_HolderQR.scala 274:19]
    node _T_103 = eq(io_h_count, UInt<7>("h67")) @[house_HolderQR.scala 274:19]
    node _T_104 = eq(io_h_count, UInt<7>("h68")) @[house_HolderQR.scala 274:19]
    node _T_105 = eq(io_h_count, UInt<7>("h69")) @[house_HolderQR.scala 274:19]
    node _T_106 = eq(io_h_count, UInt<7>("h6a")) @[house_HolderQR.scala 274:19]
    node _T_107 = eq(io_h_count, UInt<7>("h6b")) @[house_HolderQR.scala 274:19]
    node _T_108 = eq(io_h_count, UInt<7>("h6c")) @[house_HolderQR.scala 274:19]
    node _T_109 = eq(io_h_count, UInt<7>("h6d")) @[house_HolderQR.scala 274:19]
    node _T_110 = eq(io_h_count, UInt<7>("h6e")) @[house_HolderQR.scala 274:19]
    node _T_111 = eq(io_h_count, UInt<7>("h6f")) @[house_HolderQR.scala 274:19]
    node _T_112 = eq(io_h_count, UInt<7>("h70")) @[house_HolderQR.scala 274:19]
    node _T_113 = eq(io_h_count, UInt<7>("h71")) @[house_HolderQR.scala 274:19]
    node _T_114 = eq(io_h_count, UInt<7>("h72")) @[house_HolderQR.scala 274:19]
    node _T_115 = eq(io_h_count, UInt<7>("h73")) @[house_HolderQR.scala 274:19]
    node _T_116 = eq(io_h_count, UInt<7>("h74")) @[house_HolderQR.scala 274:19]
    node _T_117 = eq(io_h_count, UInt<7>("h75")) @[house_HolderQR.scala 274:19]
    node _T_118 = eq(io_h_count, UInt<7>("h76")) @[house_HolderQR.scala 274:19]
    node _T_119 = eq(io_h_count, UInt<7>("h77")) @[house_HolderQR.scala 274:19]
    node _T_120 = eq(io_h_count, UInt<7>("h78")) @[house_HolderQR.scala 274:19]
    node _T_121 = eq(io_h_count, UInt<7>("h79")) @[house_HolderQR.scala 274:19]
    node _T_122 = eq(io_h_count, UInt<7>("h7a")) @[house_HolderQR.scala 274:19]
    node _T_123 = eq(io_h_count, UInt<7>("h7b")) @[house_HolderQR.scala 274:19]
    node _T_124 = eq(io_h_count, UInt<7>("h7c")) @[house_HolderQR.scala 274:19]
    node _T_125 = eq(io_h_count, UInt<7>("h7d")) @[house_HolderQR.scala 274:19]
    node _T_126 = eq(io_h_count, UInt<7>("h7e")) @[house_HolderQR.scala 274:19]
    node _T_127 = eq(io_Tk, UInt<1>("h1")) @[house_HolderQR.scala 286:14]
    node _T_128 = eq(io_Vk, UInt<1>("h1")) @[house_HolderQR.scala 290:20]
    node _T_129 = eq(io_Tr, UInt<1>("h1")) @[house_HolderQR.scala 295:21]
    node _GEN_0 = mux(_T_129, reg_array_h_0, reg_2_0) @[house_HolderQR.scala 295:32 296:13 255:18]
    node _GEN_1 = mux(_T_129, reg_array_h_1, reg_2_1) @[house_HolderQR.scala 295:32 296:13 255:18]
    node _GEN_2 = mux(_T_129, reg_2_0, UInt<1>("h0")) @[house_HolderQR.scala 295:32 297:10 302:15]
    node _GEN_3 = mux(_T_129, reg_2_1, UInt<1>("h0")) @[house_HolderQR.scala 295:32 297:10 302:15]
    node _GEN_4 = mux(_T_129, reg_arrays_h_0, UInt<1>("h0")) @[house_HolderQR.scala 295:32 298:10 303:15]
    node _GEN_5 = mux(_T_129, reg_arrays_h_1, UInt<1>("h0")) @[house_HolderQR.scala 295:32 298:10 303:15]
    node _GEN_6 = mux(_T_128, reg_array_h_0, _GEN_2) @[house_HolderQR.scala 290:31 291:8]
    node _GEN_7 = mux(_T_128, reg_array_h_1, _GEN_3) @[house_HolderQR.scala 290:31 291:8]
    node _GEN_8 = mux(_T_128, reg_array_h_0, _GEN_4) @[house_HolderQR.scala 290:31 292:8]
    node _GEN_9 = mux(_T_128, reg_array_h_1, _GEN_5) @[house_HolderQR.scala 290:31 292:8]
    node _GEN_10 = mux(_T_128, reg_2_0, _GEN_0) @[house_HolderQR.scala 255:18 290:31]
    node _GEN_11 = mux(_T_128, reg_2_1, _GEN_1) @[house_HolderQR.scala 255:18 290:31]
    node _GEN_12 = mux(_T_127, io_col_1_0, _GEN_6) @[house_HolderQR.scala 286:25 287:8]
    node _GEN_13 = mux(_T_127, io_col_1_1, _GEN_7) @[house_HolderQR.scala 286:25 287:8]
    node _GEN_14 = mux(_T_127, io_col_1_0, _GEN_8) @[house_HolderQR.scala 286:25 288:8]
    node _GEN_15 = mux(_T_127, io_col_1_1, _GEN_9) @[house_HolderQR.scala 286:25 288:8]
    node _GEN_16 = mux(_T_127, reg_2_0, _GEN_10) @[house_HolderQR.scala 255:18 286:25]
    node _GEN_17 = mux(_T_127, reg_2_1, _GEN_11) @[house_HolderQR.scala 255:18 286:25]
    node _T_130 = eq(io_Tk, UInt<1>("h1")) @[house_HolderQR.scala 312:14]
    node _T_131 = eq(io_Vk, UInt<1>("h1")) @[house_HolderQR.scala 326:20]
    node _T_132 = eq(io_Tr, UInt<1>("h1")) @[house_HolderQR.scala 331:20]
    node _GEN_18 = mux(_T_132, reg_arrays_h_b_0, test) @[house_HolderQR.scala 331:31 337:10 265:17]
    node _GEN_19 = mux(_T_132, axpy.io_out_s_0, reg_arrays_h_b_0) @[house_HolderQR.scala 331:31 343:20 253:27]
    node _GEN_20 = mux(_T_132, axpy.io_out_s_1, reg_arrays_h_b_1) @[house_HolderQR.scala 331:31 343:20 253:27]
    node _GEN_21 = mux(_T_131, divide.io_out_s, tk) @[house_HolderQR.scala 256:15 326:31 330:8]
    node _GEN_22 = mux(_T_131, test, _GEN_18) @[house_HolderQR.scala 265:17 326:31]
    node _GEN_23 = mux(_T_131, reg_arrays_h_b_0, _GEN_19) @[house_HolderQR.scala 253:27 326:31]
    node _GEN_24 = mux(_T_131, reg_arrays_h_b_1, _GEN_20) @[house_HolderQR.scala 253:27 326:31]
    node _GEN_25 = mux(_T_130, sign.io_out_s, reg_1) @[house_HolderQR.scala 312:25 325:11 254:18]
    node _GEN_26 = mux(_T_130, tk, _GEN_21) @[house_HolderQR.scala 256:15 312:25]
    node _GEN_27 = mux(_T_130, test, _GEN_22) @[house_HolderQR.scala 265:17 312:25]
    node _GEN_28 = mux(_T_130, reg_arrays_h_b_0, _GEN_23) @[house_HolderQR.scala 312:25 253:27]
    node _GEN_29 = mux(_T_130, reg_arrays_h_b_1, _GEN_24) @[house_HolderQR.scala 312:25 253:27]
    node _T_133 = eq(io_Vk, UInt<1>("h1")) @[house_HolderQR.scala 349:14]
    node _GEN_30 = mux(_T_133, reg_1, io_reg_array_0) @[house_HolderQR.scala 269:15 349:25 350:20]
    node _T_134 = eq(io_Tr, UInt<1>("h1")) @[house_HolderQR.scala 352:14]
    node _GEN_31 = mux(_T_134, reg_1, _GEN_30) @[house_HolderQR.scala 352:25 353:20]
    node w0_0 = _GEN_12 @[house_HolderQR.scala 260:16]
    node w0_1 = _GEN_13 @[house_HolderQR.scala 260:16]
    node w1_0 = _GEN_14 @[house_HolderQR.scala 261:16]
    node w1_1 = _GEN_15 @[house_HolderQR.scala 261:16]
    node d1 = dDot.io_out_s @[house_HolderQR.scala 314:18 318:6]
    node d2 = sqrt.io_out_s @[house_HolderQR.scala 315:18 321:8]
    io_out_test <= test @[house_HolderQR.scala 355:15]
    io_out_s_0 <= w0_0 @[house_HolderQR.scala 356:10]
    io_out_s_1 <= w0_1 @[house_HolderQR.scala 356:10]
    reg_array_h_0 <= _GEN_31
    reg_array_h_1 <= io_reg_array_1 @[house_HolderQR.scala 269:15]
    reg_arrays_h_0 <= io_reg_arrays_0 @[house_HolderQR.scala 270:16]
    reg_arrays_h_1 <= io_reg_arrays_1 @[house_HolderQR.scala 270:16]
    reg_arrays_h_b_0 <= _GEN_28
    reg_arrays_h_b_1 <= _GEN_29
    reg_1 <= _GEN_25
    reg_2_0 <= _GEN_16
    reg_2_1 <= _GEN_17
    tk <= _GEN_26
    dDot.clock <= clock
    dDot.reset <= reset
    dDot.io_in_a_0 <= w0_0 @[house_HolderQR.scala 308:16]
    dDot.io_in_a_1 <= w0_1 @[house_HolderQR.scala 308:16]
    dDot.io_in_b_0 <= w1_0 @[house_HolderQR.scala 309:16]
    dDot.io_in_b_1 <= w1_1 @[house_HolderQR.scala 309:16]
    test <= _GEN_27
    sqrt.clock <= clock
    sqrt.reset <= reset
    sqrt.io_in_a <= d1 @[house_HolderQR.scala 320:18]
    sign.clock <= clock
    sign.reset <= reset
    sign.io_in_a <= reg_array_h_0 @[house_HolderQR.scala 323:18]
    sign.io_in_b <= d2 @[house_HolderQR.scala 324:18]
    divide.clock <= clock
    divide.reset <= reset
    divide.io_in_a <= UInt<32>("hc0000000") @[house_HolderQR.scala 328:20]
    divide.io_in_b <= dDot.io_out_s @[house_HolderQR.scala 329:20]
    reg_array.clock <= clock
    reg_array.reset <= reset
    reg_array.io_in_a_0 <= reg_arrays_h_0 @[house_HolderQR.scala 333:23]
    reg_array.io_in_a_1 <= reg_arrays_h_1 @[house_HolderQR.scala 333:23]
    d5_M.clock <= clock
    d5_M.reset <= reset
    d5_M.io_in_a <= tk @[house_HolderQR.scala 338:18]
    d5_M.io_in_b <= dDot.io_out_s @[house_HolderQR.scala 339:18]
    axpy.clock <= clock
    axpy.reset <= reset
    axpy.io_in_a <= d5_M.io_out_s @[house_HolderQR.scala 340:18]
    axpy.io_in_b_0 <= reg_2_0 @[house_HolderQR.scala 341:20]
    axpy.io_in_b_1 <= reg_2_1 @[house_HolderQR.scala 341:20]
    axpy.io_in_c_0 <= reg_array.io_out_s_0 @[house_HolderQR.scala 342:20]
    axpy.io_in_c_1 <= reg_array.io_out_s_1 @[house_HolderQR.scala 342:20]
