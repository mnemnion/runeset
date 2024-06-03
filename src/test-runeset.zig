//! RuneSet test suite
//!
//! The intention is to provide 100% code coverage to the machine-instruction
//! level.

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub const elements = @import("elements.zig");
pub const runeset = @import("runeset.zig");

const RuneSet = runeset.RuneSet;
const codeunit = elements.codeunit;

const expect = std.testing.expect;
const expectEqual = testing.expectEqual;

//| Test data type(s)

/// LRstrings
///
/// A string split into canonical left and right portions.
///
/// To be well-formed, `str` must have all runes in `l` and `r`, which
/// must not themselves share any runes in common.
const LRstrings = struct {
    l: []const u8,
    r: []const u8,
    str: []const u8,
};

//| Test Functions

/// This confirms that none of the runes in `str` match in `set`.
///
/// Allows invalid UTF-8.
fn testMatchNone(set: RuneSet, str: []const u8) !void {
    var idx: usize = 0;
    while (idx < str.len) {
        const slice = str[idx..];
        const nB = codeunit(slice[0]).nBytes() orelse 1;
        try expectEqual(0, set.matchOne(slice));
        idx += nB;
    }
}

/// Confirms that a RuneSet built from `str` will match every rune in `str`.
///
/// Invalid UTF-8 is safe, but the test will fail.
fn buildAndTestRuneSet(str: []const u8, alloc: Allocator) !void {
    const set = try RuneSet.createFromConstString(str, alloc);
    defer set.deinit(alloc);
    const matched = set.matchMany(str);
    if (matched) |m| {
        try expectEqual(str.len, m);
        try expectEqual(str.len, set.codeunitCount());
    } else try expect(false);
}

/// Built a set from `a` and `b`, take the union of these sets, and confirm that
/// all runes in `a` and `b` are matches for the union.
///
/// This test will fail safely if `a` and `b` are not valid UTF-8.
fn buildAndTestUnion(a: []const u8, b: []const u8, alloc: Allocator) !void {
    const setA = try RuneSet.createFromConstString(a, alloc);
    defer setA.deinit(alloc);
    const setB = try RuneSet.createFromConstString(b, alloc);
    defer setB.deinit(alloc);
    const setU = try setA.setUnion(setB, alloc);
    defer setU.deinit(alloc);
    const matchA = setU.matchMany(a);
    if (matchA) |m| {
        try expectEqual(a.len, m);
    } else try expect(false);
    const matchB = setU.matchMany(b);
    if (matchB) |m| {
        try expectEqual(b.len, m);
    } else try expect(false);
}

/// Validate union properties of an LRstring set:
///
/// - The union of the `l` set and the `r` set matches both `l` and `r`
/// - The union of sets matches `str`
/// - A set of `str` is equal to the union of `l` and `r`
///
fn buildAndTestLRUnion(s: LRstrings, alloc: Allocator) !void {
    const setL = try RuneSet.createFromConstString(s.l, alloc);
    defer setL.deinit(alloc);
    const setR = try RuneSet.createFromConstString(s.r, alloc);
    defer setR.deinit(alloc);
    const setU = try setL.setUnion(setR, alloc);
    defer setU.deinit(alloc);
    const setAll = try RuneSet.createFromConstString(s.str, alloc);
    defer setAll.deinit(alloc);
    const matchL = setU.matchMany(s.l);
    if (matchL) |m| {
        try expectEqual(s.l.len, m);
    } else try expect(false);
    const matchR = setU.matchMany(s.r);
    if (matchR) |m| {
        try expectEqual(s.r.len, m);
    } else try expect(false);
    const matchAll = setU.matchMany(s.str);
    if (matchAll) |m| {
        try expectEqual(s.str.len, m);
    } else try expect(false);
    try expect(setU.equalTo(setAll));
}

/// Verify correct set difference of LR string:
///
/// - The diff of set of `str`:
///     - With set of `l` matches all of `r`
///     - With set of `r` matches all of `l`
///     - With set of `l` matches none of `l`
///     - With set of `r` matches none of `r`
/// - The diff of set of `str` with itself is the empty set ∅
///
fn verifySetDifference(LR: LRstrings, alloc: Allocator) !void {
    const setAll = try RuneSet.createFromConstString(LR.str, alloc);
    defer setAll.deinit(alloc);
    const setR = try RuneSet.createFromConstString(LR.r, alloc);
    defer setR.deinit(alloc);
    const setL = try RuneSet.createFromConstString(LR.l, alloc);
    defer setL.deinit(alloc);
    const setAdiffR = try setAll.setDifference(setR, alloc);
    defer setAdiffR.deinit(alloc);
    const setAdiffL = try setAll.setDifference(setL, alloc);
    defer setAdiffL.deinit(alloc);
    const matchL = setAdiffR.matchMany(LR.l);
    if (matchL) |nMatch| {
        try expectEqual(LR.l.len, nMatch);
    } else try expect(false);
    const matchR = setAdiffL.matchMany(LR.r);
    if (matchR) |nMatch| {
        try expectEqual(LR.r.len, nMatch);
    } else try expect(false);
    try testMatchNone(setAdiffL, LR.l);
    try testMatchNone(setAdiffR, LR.r);
    const setNone = setAll.setDifference(setAll);
    defer setNone.deinit(alloc);
    try expectEqual(0, setNone.codeunitCount);
    try expectEqual(4, setNone.body.len);
}

//| Test Suite

test "is this thing on?" {
    std.debug.print("\nthis thing is, in fact, on\n", .{});
    try expect(true);
}

test "create set unions" {
    const allocator = std.testing.allocator;
    try buildAndTestLRUnion(ascii, allocator);
    try buildAndTestLRUnion(greek, allocator);
    try buildAndTestLRUnion(math, allocator);
    try buildAndTestLRUnion(linear_B, allocator);
    try buildAndTestLRUnion(han_sample, allocator);
    try buildAndTestLRUnion(deseret, allocator);
}

// Inline tests of runeset.zig and all tests of element.zig
test {
    std.testing.refAllDecls(@This());
}

//| Test Data
//|
//| An extensive collection of string data, meant to fully exercise the
//| functionality of the RuneSet type.

//| LRstrings

/// All printable characters in ASCII
const ascii: LRstrings = .{
    .r = "!#%')+-/13579;=?ACEGIKMOQSUWY[]_acegikmoqsuwy{}",
    .l = " \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~",
    .str = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
};

/// The basic range of the Greek alphabet
const greek: LRstrings = .{
    .r = "ΒΔΖΘΚΜΞΠΣΥΧΩΫέίαγεηιλνορσυχω",
    .l = "ΑΓΕΗΙΛΝΟΡΤΦΨΪάήΰβδζθκμξπςτφψ",
    .str = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψω",
};

/// A representative range of three-byte mathematical symbols
const math: LRstrings = .{
    .r = "∁∃∅∇∉∋∍∏∑∓∕∗∙∛∝∟∡∣∥∧∩∫∭∯∱∳∵∷∹∻∽∿≁≃≅≇≉≋≍≏≑≓≕≗≙≛≝≟≡≣≥≧≩≫≭≯≱≳≵≷≹≻≽≿⊁⊃⊅⊇⊉⊋⊍⊏⊑⊓⊕⊗⊙⊛⊝⊟⊡⊣⊥⊧⊩⊫⊭⊯⊱⊳⊵⊷⊹⊻⊽⊿⋁⋃⋅⋇⋉⋋⋍⋏⋑⋓⋕⋗⋙⋛⋝⋟⋡⋣⋥⋧⋩⋫⋭⋯⋱⋳⋵⋷⋹⋻⋽⋿",
    .l = "∀∂∄∆∈∊∌∎∐−∔∖∘√∜∞∠∢∤∦∨∪∬∮∰∲∴∶∸∺∼∾≀≂≄≆≈≊≌≎≐≒≔≖≘≚≜≞≠≢≤≦≨≪≬≮≰≲≴≶≸≺≼≾⊀⊂⊄⊆⊈⊊⊌⊎⊐⊒⊔⊖⊘⊚⊜⊞⊠⊢⊤⊦⊨⊪⊬⊮⊰⊲⊴⊶⊸⊺⊼⊾⋀⋂⋄⋆⋈⋊⋌⋎⋐⋒⋔⋖⋘⋚⋜⋞⋠⋢⋤⋦⋨⋪⋬⋮⋰⋲⋴⋶⋸⋺⋼⋾",
    .str = "∀∁∂∃∄∅∆∇∈∉∊∋∌∍∎∏∐∑−∓∔∕∖∗∘∙√∛∜∝∞∟∠∡∢∣∤∥∦∧∨∩∪∫∬∭∮∯∰∱∲∳∴∵∶∷∸∹∺∻∼∽∾∿≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⊾⊿⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋮⋯⋰⋱⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿",
};

/// A syllabary for Mycenaean Greek, and the first characters of four bytes in UTF-8
const linear_B: LRstrings = .{
    .r = "𐀁𐀃𐀅𐀇𐀉𐀋𐀎𐀐𐀒𐀔𐀖𐀘𐀚𐀜𐀞𐀠𐀢𐀤𐀦𐀩𐀫𐀭𐀯𐀱𐀳𐀵𐀷𐀹𐀼𐀿𐁁𐁃𐁅𐁇𐁉𐁋𐁍𐁑𐁓𐁕𐁗𐁙𐁛𐁝",
    .l = "𐀀𐀂𐀄𐀆𐀈𐀊𐀍𐀏𐀑𐀓𐀕𐀗𐀙𐀛𐀝𐀟𐀡𐀣𐀥𐀨𐀪𐀬𐀮𐀰𐀲𐀴𐀶𐀸𐀺𐀽𐁀𐁂𐁄𐁆𐁈𐁊𐁌𐁐𐁒𐁔𐁖𐁘𐁚𐁜",
    .str = "𐀀𐀁𐀂𐀃𐀄𐀅𐀆𐀇𐀈𐀉𐀊𐀋𐀍𐀎𐀏𐀐𐀑𐀒𐀓𐀔𐀕𐀖𐀗𐀘𐀙𐀚𐀛𐀜𐀝𐀞𐀟𐀠𐀡𐀢𐀣𐀤𐀥𐀦𐀨𐀩𐀪𐀫𐀬𐀭𐀮𐀯𐀰𐀱𐀲𐀳𐀴𐀵𐀶𐀷𐀸𐀹𐀺𐀼𐀽𐀿𐁀𐁁𐁂𐁃𐁄𐁅𐁆𐁇𐁈𐁉𐁊𐁋𐁌𐁍𐁐𐁑𐁒𐁓𐁔𐁕𐁖𐁗𐁘𐁙𐁚𐁛𐁜𐁝",
};

/// An esoteric alphabet of Mormon origin, found in the four-byte range.
const deseret: LRstrings = .{
    .r = "𐐁𐐃𐐅𐐇𐐉𐐋𐐍𐐏𐐑𐐓𐐕𐐗𐐙𐐛𐐝𐐟𐐡𐐣𐐥𐐧𐐩𐐫𐐭𐐯𐐱𐐳𐐵𐐷𐐹𐐻𐐽𐐿𐑁𐑃𐑅𐑇𐑉𐑋𐑍𐑏",
    .l = "𐐀𐐂𐐄𐐆𐐈𐐊𐐌𐐎𐐐𐐒𐐔𐐖𐐘𐐚𐐜𐐞𐐠𐐢𐐤𐐦𐐨𐐪𐐬𐐮𐐰𐐲𐐴𐐶𐐸𐐺𐐼𐐾𐑀𐑂𐑄𐑆𐑈𐑊𐑌𐑎",
    .str = "𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈𐐉𐐊𐐋𐐌𐐍𐐎𐐏𐐐𐐑𐐒𐐓𐐔𐐕𐐖𐐗𐐘𐐙𐐚𐐛𐐜𐐝𐐞𐐟𐐠𐐡𐐢𐐣𐐤𐐥𐐦𐐧𐐨𐐩𐐪𐐫𐐬𐐭𐐮𐐯𐐰𐐱𐐲𐐳𐐴𐐵𐐶𐐷𐐸𐐹𐐺𐐻𐐼𐐽𐐾𐐿𐑀𐑁𐑂𐑃𐑄𐑅𐑆𐑇𐑈𐑉𐑊𐑋𐑌𐑍𐑎𐑏",
};

/// A slice of Hanzi ideographs in the three-byte range.
const han_sample: LRstrings = .{
    .r = "葉殺沈若略兩梁良量呂廬濾閭麗力歷年戀漣璉練輦連列咽裂廉捻簾令寧怜瑩聆零領禮隸了寮料燎蓼龍阮杻流琉硫類戮倫淪律栗隆吏易梨理罹裡離溺燐藺鱗林臨笠狀識茶切拓宅暴行見兀﨎塚晴﨔猪礼祥靖羽蘒諸﨤都﨨飯館郞侮免勤喝器墨屮慨懲既梅渚煮琢社祈祖禍穀節縉署臭艹褐謁賓辶難頻𤋮",
    .l = "省說辰拾掠亮凉糧諒勵女旅礪驪黎曆轢憐撚煉秊聯蓮鍊劣烈說念殮獵囹嶺玲羚鈴靈例醴惡僚尿樂療遼暈劉柳溜留紐六陸崙輪慄率利履李泥痢裏里匿吝璘隣麟淋立粒炙什刺度糖洞輻降廓嗀﨏﨑﨓凞益神福精﨟﨡﨣逸﨧﨩飼鶴隷僧勉卑嘆塀層悔憎敏暑海漢爫碑祉祐祝禎突練繁者艹著視謹贈逸響恵舘",
    .str = "省葉說殺辰沈拾若掠略亮兩凉梁糧良諒量勵呂女廬旅濾礪閭驪麗黎力曆歷轢年憐戀撚漣煉璉秊練聯輦蓮連鍊列劣咽烈裂說廉念捻殮簾獵令囹寧嶺怜玲瑩羚聆鈴零靈領例禮醴隸惡了僚寮尿料樂燎療蓼遼龍暈阮劉杻柳流溜琉留硫紐類六戮陸倫崙淪輪律慄栗率隆利吏履易李梨泥理痢罹裏裡里離匿溺吝燐璘藺隣鱗麟林淋臨立笠粒狀炙識什茶刺切度拓糖宅洞暴輻行降見廓兀嗀﨎﨏塚﨑晴﨓﨔凞猪益礼神祥福靖精羽﨟蘒﨡諸﨣﨤逸都﨧﨨﨩飯飼館鶴郞隷侮僧免勉勤卑喝嘆器塀墨層屮悔慨憎懲敏既暑梅海渚漢煮爫琢碑社祉祈祐祖祝禍禎穀突節練縉繁署者臭艹艹著褐視謁謹賓贈辶逸難響頻恵𤋮舘",
};
