circuit house_HolderQR :
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

  module twoscomplement_1 :
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
    inst adder of full_adder @[FPArithmetic.scala 87:23]
    inst complementN_0 of twoscomplement_1 @[FPArithmetic.scala 93:31]
    inst complementN_1 of twoscomplement_1 @[FPArithmetic.scala 95:31]
    inst shifter of shifter @[FPArithmetic.scala 99:25]
    inst complementN_2 of twoscomplement_1 @[FPArithmetic.scala 144:31]
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

  module house_HolderQR :
    input clock : Clock
    input reset : UInt<1>
    input io_in_a_0 : UInt<32>
    input io_in_a_1 : UInt<32>
    input io_in_a_2 : UInt<32>
    input io_in_a_3 : UInt<32>
    output io_out_0 : UInt<32>
    output io_out_1 : UInt<32>
    output io_out_test : UInt<32>

    inst adder2 of FP_adder @[house_HolderQR.scala 54:22]
    inst subber1 of FP_subber @[house_HolderQR.scala 58:23]
    reg holder_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), holder_0) @[house_HolderQR.scala 31:19]
    reg holder_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), holder_1) @[house_HolderQR.scala 31:19]
    reg reflector_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reflector_0) @[house_HolderQR.scala 32:22]
    reg reflector_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reflector_1) @[house_HolderQR.scala 32:22]
    reg in_a_prime_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_a_prime_0) @[house_HolderQR.scala 35:23]
    reg in_a_prime_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_a_prime_1) @[house_HolderQR.scala 35:23]
    reg in_a_prime_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_a_prime_2) @[house_HolderQR.scala 35:23]
    reg in_a_prime_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_a_prime_3) @[house_HolderQR.scala 35:23]
    reg d1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), d1) @[house_HolderQR.scala 41:19]
    reg d2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), d2) @[house_HolderQR.scala 42:19]
    reg d3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), d3) @[house_HolderQR.scala 43:19]
    reg tk : UInt<32>, clock with :
      reset => (UInt<1>("h0"), tk) @[house_HolderQR.scala 44:19]
    reg d4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), d4) @[house_HolderQR.scala 45:19]
    reg d5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), d5) @[house_HolderQR.scala 46:19]
    io_out_0 <= holder_0 @[house_HolderQR.scala 175:10]
    io_out_1 <= holder_1 @[house_HolderQR.scala 175:10]
    io_out_test <= UInt<32>("h3f922d0e") @[house_HolderQR.scala 176:15]
    holder_0 <= in_a_prime_0 @[house_HolderQR.scala 71:21]
    holder_1 <= in_a_prime_2 @[house_HolderQR.scala 71:21]
    reflector_0 <= adder2.io_out_s @[house_HolderQR.scala 107:23]
    reflector_1 <= holder_1 @[house_HolderQR.scala 97:15]
    in_a_prime_0 <= io_in_a_0 @[house_HolderQR.scala 63:12]
    in_a_prime_1 <= io_in_a_1 @[house_HolderQR.scala 63:12]
    in_a_prime_2 <= io_in_a_2 @[house_HolderQR.scala 63:12]
    in_a_prime_3 <= io_in_a_3 @[house_HolderQR.scala 63:12]
    d1 <= mux(reset, UInt<32>("h0"), d1) @[house_HolderQR.scala 41:{19,19,19}]
    d2 <= mux(reset, UInt<32>("h0"), d2) @[house_HolderQR.scala 42:{19,19,19}]
    d3 <= mux(reset, UInt<32>("h0"), d3) @[house_HolderQR.scala 43:{19,19,19}]
    tk <= mux(reset, UInt<32>("h0"), tk) @[house_HolderQR.scala 44:{19,19,19}]
    d4 <= mux(reset, UInt<32>("h0"), d4) @[house_HolderQR.scala 45:{19,19,19}]
    d5 <= mux(reset, UInt<32>("h0"), d5) @[house_HolderQR.scala 46:{19,19,19}]
    adder2.clock <= clock
    adder2.reset <= reset
    adder2.io_in_a <= holder_0 @[house_HolderQR.scala 105:24]
    adder2.io_in_b <= UInt<32>("h3f922d0e") @[house_HolderQR.scala 106:24]
    subber1.clock <= clock
    subber1.reset <= reset
    subber1.io_in_a <= holder_0 @[house_HolderQR.scala 100:23]
    subber1.io_in_b <= UInt<32>("h3f922d0e") @[house_HolderQR.scala 101:23]
