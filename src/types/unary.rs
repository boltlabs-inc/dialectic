//! The unary numbers, represented by zero [`Z`] and successor [`S`].

/// The number zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Z;

/// The successor of `N` (i.e. `N + 1`).
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct S<N>(pub N);

/// All unary numbers can be converted to their value-level equivalent `usize`.
pub trait Unary: sealed::Sealed {
    /// The runtime value of this type-level number, as a `usize`.
    const VALUE: usize;
}

impl Unary for Z {
    const VALUE: usize = 0;
}

impl<N: Unary> Unary for S<N> {
    const VALUE: usize = N::VALUE + 1;
}

mod sealed {
    use super::*;
    pub trait Sealed {}
    impl Sealed for Z {}
    impl<N: Sealed> Sealed for S<N> {}
}

pub mod constants {
    //! Predefined value-level constants for small-ish type-level numbers. Each of these corresponds
    //! to a type synonym in [`types`].
    use super::types::*;
    use super::*;

    pub const _0: _0 = Z;
    pub const _1: _1 = S(_0);
    pub const _2: _2 = S(_1);
    pub const _3: _3 = S(_2);
    pub const _4: _4 = S(_3);
    pub const _5: _5 = S(_4);
    pub const _6: _6 = S(_5);
    pub const _7: _7 = S(_6);
    pub const _8: _8 = S(_7);
    pub const _9: _9 = S(_8);
    pub const _10: _10 = S(_9);
    pub const _11: _11 = S(_10);
    pub const _12: _12 = S(_11);
    pub const _13: _13 = S(_12);
    pub const _14: _14 = S(_13);
    pub const _15: _15 = S(_14);
    pub const _16: _16 = S(_15);
    pub const _17: _17 = S(_16);
    pub const _18: _18 = S(_17);
    pub const _19: _19 = S(_18);
    pub const _20: _20 = S(_19);
    pub const _21: _21 = S(_20);
    pub const _22: _22 = S(_21);
    pub const _23: _23 = S(_22);
    pub const _24: _24 = S(_23);
    pub const _25: _25 = S(_24);
    pub const _26: _26 = S(_25);
    pub const _27: _27 = S(_26);
    pub const _28: _28 = S(_27);
    pub const _29: _29 = S(_28);
    pub const _30: _30 = S(_29);
    pub const _31: _31 = S(_30);
    pub const _32: _32 = S(_31);
    pub const _33: _33 = S(_32);
    pub const _34: _34 = S(_33);
    pub const _35: _35 = S(_34);
    pub const _36: _36 = S(_35);
    pub const _37: _37 = S(_36);
    pub const _38: _38 = S(_37);
    pub const _39: _39 = S(_38);
    pub const _40: _40 = S(_39);
    pub const _41: _41 = S(_40);
    pub const _42: _42 = S(_41);
    pub const _43: _43 = S(_42);
    pub const _44: _44 = S(_43);
    pub const _45: _45 = S(_44);
    pub const _46: _46 = S(_45);
    pub const _47: _47 = S(_46);
    pub const _48: _48 = S(_47);
    pub const _49: _49 = S(_48);
    pub const _50: _50 = S(_49);
    pub const _51: _51 = S(_50);
    pub const _52: _52 = S(_51);
    pub const _53: _53 = S(_52);
    pub const _54: _54 = S(_53);
    pub const _55: _55 = S(_54);
    pub const _56: _56 = S(_55);
    pub const _57: _57 = S(_56);
    pub const _58: _58 = S(_57);
    pub const _59: _59 = S(_58);
    pub const _60: _60 = S(_59);
    pub const _61: _61 = S(_60);
    pub const _62: _62 = S(_61);
    pub const _63: _63 = S(_62);
    pub const _64: _64 = S(_63);
    pub const _65: _65 = S(_64);
    pub const _66: _66 = S(_65);
    pub const _67: _67 = S(_66);
    pub const _68: _68 = S(_67);
    pub const _69: _69 = S(_68);
    pub const _70: _70 = S(_69);
    pub const _71: _71 = S(_70);
    pub const _72: _72 = S(_71);
    pub const _73: _73 = S(_72);
    pub const _74: _74 = S(_73);
    pub const _75: _75 = S(_74);
    pub const _76: _76 = S(_75);
    pub const _77: _77 = S(_76);
    pub const _78: _78 = S(_77);
    pub const _79: _79 = S(_78);
    pub const _80: _80 = S(_79);
    pub const _81: _81 = S(_80);
    pub const _82: _82 = S(_81);
    pub const _83: _83 = S(_82);
    pub const _84: _84 = S(_83);
    pub const _85: _85 = S(_84);
    pub const _86: _86 = S(_85);
    pub const _87: _87 = S(_86);
    pub const _88: _88 = S(_87);
    pub const _89: _89 = S(_88);
    pub const _90: _90 = S(_89);
    pub const _91: _91 = S(_90);
    pub const _92: _92 = S(_91);
    pub const _93: _93 = S(_92);
    pub const _94: _94 = S(_93);
    pub const _95: _95 = S(_94);
    pub const _96: _96 = S(_95);
    pub const _97: _97 = S(_96);
    pub const _98: _98 = S(_97);
    pub const _99: _99 = S(_98);
    pub const _100: _100 = S(_99);
    pub const _101: _101 = S(_100);
    pub const _102: _102 = S(_101);
    pub const _103: _103 = S(_102);
    pub const _104: _104 = S(_103);
    pub const _105: _105 = S(_104);
    pub const _106: _106 = S(_105);
    pub const _107: _107 = S(_106);
    pub const _108: _108 = S(_107);
    pub const _109: _109 = S(_108);
    pub const _110: _110 = S(_109);
    pub const _111: _111 = S(_110);
    pub const _112: _112 = S(_111);
    pub const _113: _113 = S(_112);
    pub const _114: _114 = S(_113);
    pub const _115: _115 = S(_114);
    pub const _116: _116 = S(_115);
    pub const _117: _117 = S(_116);
    pub const _118: _118 = S(_117);
    pub const _119: _119 = S(_118);
    pub const _120: _120 = S(_119);
    pub const _121: _121 = S(_120);
    pub const _122: _122 = S(_121);
    pub const _123: _123 = S(_122);
    pub const _124: _124 = S(_123);
    pub const _125: _125 = S(_124);
    pub const _126: _126 = S(_125);
    pub const _127: _127 = S(_126);
}

pub mod types {
    //! Predefined type-level constants for small-ish type-level numbers. Each of these types is
    //! inhabited by a corresponding value constant in [`constants`].
    use super::*;

    pub type _0 = Z;
    pub type _1 = S<_0>;
    pub type _2 = S<_1>;
    pub type _3 = S<_2>;
    pub type _4 = S<_3>;
    pub type _5 = S<_4>;
    pub type _6 = S<_5>;
    pub type _7 = S<_6>;
    pub type _8 = S<_7>;
    pub type _9 = S<_8>;
    pub type _10 = S<_9>;
    pub type _11 = S<_10>;
    pub type _12 = S<_11>;
    pub type _13 = S<_12>;
    pub type _14 = S<_13>;
    pub type _15 = S<_14>;
    pub type _16 = S<_15>;
    pub type _17 = S<_16>;
    pub type _18 = S<_17>;
    pub type _19 = S<_18>;
    pub type _20 = S<_19>;
    pub type _21 = S<_20>;
    pub type _22 = S<_21>;
    pub type _23 = S<_22>;
    pub type _24 = S<_23>;
    pub type _25 = S<_24>;
    pub type _26 = S<_25>;
    pub type _27 = S<_26>;
    pub type _28 = S<_27>;
    pub type _29 = S<_28>;
    pub type _30 = S<_29>;
    pub type _31 = S<_30>;
    pub type _32 = S<_31>;
    pub type _33 = S<_32>;
    pub type _34 = S<_33>;
    pub type _35 = S<_34>;
    pub type _36 = S<_35>;
    pub type _37 = S<_36>;
    pub type _38 = S<_37>;
    pub type _39 = S<_38>;
    pub type _40 = S<_39>;
    pub type _41 = S<_40>;
    pub type _42 = S<_41>;
    pub type _43 = S<_42>;
    pub type _44 = S<_43>;
    pub type _45 = S<_44>;
    pub type _46 = S<_45>;
    pub type _47 = S<_46>;
    pub type _48 = S<_47>;
    pub type _49 = S<_48>;
    pub type _50 = S<_49>;
    pub type _51 = S<_50>;
    pub type _52 = S<_51>;
    pub type _53 = S<_52>;
    pub type _54 = S<_53>;
    pub type _55 = S<_54>;
    pub type _56 = S<_55>;
    pub type _57 = S<_56>;
    pub type _58 = S<_57>;
    pub type _59 = S<_58>;
    pub type _60 = S<_59>;
    pub type _61 = S<_60>;
    pub type _62 = S<_61>;
    pub type _63 = S<_62>;
    pub type _64 = S<_63>;
    pub type _65 = S<_64>;
    pub type _66 = S<_65>;
    pub type _67 = S<_66>;
    pub type _68 = S<_67>;
    pub type _69 = S<_68>;
    pub type _70 = S<_69>;
    pub type _71 = S<_70>;
    pub type _72 = S<_71>;
    pub type _73 = S<_72>;
    pub type _74 = S<_73>;
    pub type _75 = S<_74>;
    pub type _76 = S<_75>;
    pub type _77 = S<_76>;
    pub type _78 = S<_77>;
    pub type _79 = S<_78>;
    pub type _80 = S<_79>;
    pub type _81 = S<_80>;
    pub type _82 = S<_81>;
    pub type _83 = S<_82>;
    pub type _84 = S<_83>;
    pub type _85 = S<_84>;
    pub type _86 = S<_85>;
    pub type _87 = S<_86>;
    pub type _88 = S<_87>;
    pub type _89 = S<_88>;
    pub type _90 = S<_89>;
    pub type _91 = S<_90>;
    pub type _92 = S<_91>;
    pub type _93 = S<_92>;
    pub type _94 = S<_93>;
    pub type _95 = S<_94>;
    pub type _96 = S<_95>;
    pub type _97 = S<_96>;
    pub type _98 = S<_97>;
    pub type _99 = S<_98>;
    pub type _100 = S<_99>;
    pub type _101 = S<_100>;
    pub type _102 = S<_101>;
    pub type _103 = S<_102>;
    pub type _104 = S<_103>;
    pub type _105 = S<_104>;
    pub type _106 = S<_105>;
    pub type _107 = S<_106>;
    pub type _108 = S<_107>;
    pub type _109 = S<_108>;
    pub type _110 = S<_109>;
    pub type _111 = S<_110>;
    pub type _112 = S<_111>;
    pub type _113 = S<_112>;
    pub type _114 = S<_113>;
    pub type _115 = S<_114>;
    pub type _116 = S<_115>;
    pub type _117 = S<_116>;
    pub type _118 = S<_117>;
    pub type _119 = S<_118>;
    pub type _120 = S<_119>;
    pub type _121 = S<_120>;
    pub type _122 = S<_121>;
    pub type _123 = S<_122>;
    pub type _124 = S<_123>;
    pub type _125 = S<_124>;
    pub type _126 = S<_125>;
    pub type _127 = S<_126>;
}
