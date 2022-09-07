circuit house_HolderQR :
  module house_HolderQR :
    input clock : Clock
    input reset : UInt<1>
    input io_col_1_0 : UInt<32>
    input io_col_1_1 : UInt<32>
    input io_reg_array_0 : UInt<32>
    input io_reg_array_1 : UInt<32>
    input io_Tk : UInt<1>
    input io_Vk : UInt<1>
    output io_out_s_0 : UInt<32>
    output io_out_s_1 : UInt<32>

    reg reg_array_h_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_0) @[house_HolderQR.scala 251:24]
    reg reg_array_h_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_array_h_1) @[house_HolderQR.scala 251:24]
    node _T = eq(io_Tk, UInt<1>("h1")) @[house_HolderQR.scala 258:14]
    node _T_1 = eq(io_Vk, UInt<1>("h1")) @[house_HolderQR.scala 261:20]
    node _GEN_0 = mux(_T_1, reg_array_h_0, reg_array_h_0) @[house_HolderQR.scala 261:31 262:8 265:8]
    node _GEN_1 = mux(_T_1, reg_array_h_1, reg_array_h_1) @[house_HolderQR.scala 261:31 262:8 265:8]
    node _GEN_2 = mux(_T, io_col_1_0, _GEN_0) @[house_HolderQR.scala 258:25 259:6]
    node _GEN_3 = mux(_T, io_col_1_1, _GEN_1) @[house_HolderQR.scala 258:25 259:6]
    node w0_0 = _GEN_2 @[house_HolderQR.scala 253:16]
    node w0_1 = _GEN_3 @[house_HolderQR.scala 253:16]
    node w1_0 = _GEN_2 @[house_HolderQR.scala 254:16]
    node w1_1 = _GEN_3 @[house_HolderQR.scala 254:16]
    io_out_s_0 <= w0_0 @[house_HolderQR.scala 276:10]
    io_out_s_1 <= w0_1 @[house_HolderQR.scala 276:10]
    reg_array_h_0 <= io_reg_array_0 @[house_HolderQR.scala 256:15]
    reg_array_h_1 <= io_reg_array_1 @[house_HolderQR.scala 256:15]
