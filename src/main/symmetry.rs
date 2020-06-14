pub const SYMMETRY_PERMUTATIONS: [[u8; 9]; 8] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8],
    [6, 3, 0, 7, 4, 1, 8, 5, 2],
    [8, 7, 6, 5, 4, 3, 2, 1, 0],
    [2, 5, 8, 1, 4, 7, 0, 3, 6],
    [2, 1, 0, 5, 4, 3, 8, 7, 6],
    [0, 3, 6, 1, 4, 7, 2, 5, 8],
    [6, 7, 8, 3, 4, 5, 0, 1, 2],
    [8, 5, 2, 7, 4, 1, 6, 3, 0],
];

pub struct Permutation([u16; 512]);

impl Permutation {
    pub fn apply(&self, bits: u16) -> u16 {
        debug_assert!(bits & 0x1ff == bits);
        self.0[bits as usize]
    }
}

pub const PERM_TABLES: [Permutation; 8] = [
    Permutation([
        0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0x10, 0x11,
        0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20,
        0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e,
        0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d,
        0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c,
        0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b,
        0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a,
        0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
        0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
        0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
        0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
        0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5,
        0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4,
        0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3,
        0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2,
        0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff, 0x100, 0x101,
        0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108, 0x109, 0x10a, 0x10b, 0x10c, 0x10d, 0x10e,
        0x10f, 0x110, 0x111, 0x112, 0x113, 0x114, 0x115, 0x116, 0x117, 0x118, 0x119, 0x11a, 0x11b,
        0x11c, 0x11d, 0x11e, 0x11f, 0x120, 0x121, 0x122, 0x123, 0x124, 0x125, 0x126, 0x127, 0x128,
        0x129, 0x12a, 0x12b, 0x12c, 0x12d, 0x12e, 0x12f, 0x130, 0x131, 0x132, 0x133, 0x134, 0x135,
        0x136, 0x137, 0x138, 0x139, 0x13a, 0x13b, 0x13c, 0x13d, 0x13e, 0x13f, 0x140, 0x141, 0x142,
        0x143, 0x144, 0x145, 0x146, 0x147, 0x148, 0x149, 0x14a, 0x14b, 0x14c, 0x14d, 0x14e, 0x14f,
        0x150, 0x151, 0x152, 0x153, 0x154, 0x155, 0x156, 0x157, 0x158, 0x159, 0x15a, 0x15b, 0x15c,
        0x15d, 0x15e, 0x15f, 0x160, 0x161, 0x162, 0x163, 0x164, 0x165, 0x166, 0x167, 0x168, 0x169,
        0x16a, 0x16b, 0x16c, 0x16d, 0x16e, 0x16f, 0x170, 0x171, 0x172, 0x173, 0x174, 0x175, 0x176,
        0x177, 0x178, 0x179, 0x17a, 0x17b, 0x17c, 0x17d, 0x17e, 0x17f, 0x180, 0x181, 0x182, 0x183,
        0x184, 0x185, 0x186, 0x187, 0x188, 0x189, 0x18a, 0x18b, 0x18c, 0x18d, 0x18e, 0x18f, 0x190,
        0x191, 0x192, 0x193, 0x194, 0x195, 0x196, 0x197, 0x198, 0x199, 0x19a, 0x19b, 0x19c, 0x19d,
        0x19e, 0x19f, 0x1a0, 0x1a1, 0x1a2, 0x1a3, 0x1a4, 0x1a5, 0x1a6, 0x1a7, 0x1a8, 0x1a9, 0x1aa,
        0x1ab, 0x1ac, 0x1ad, 0x1ae, 0x1af, 0x1b0, 0x1b1, 0x1b2, 0x1b3, 0x1b4, 0x1b5, 0x1b6, 0x1b7,
        0x1b8, 0x1b9, 0x1ba, 0x1bb, 0x1bc, 0x1bd, 0x1be, 0x1bf, 0x1c0, 0x1c1, 0x1c2, 0x1c3, 0x1c4,
        0x1c5, 0x1c6, 0x1c7, 0x1c8, 0x1c9, 0x1ca, 0x1cb, 0x1cc, 0x1cd, 0x1ce, 0x1cf, 0x1d0, 0x1d1,
        0x1d2, 0x1d3, 0x1d4, 0x1d5, 0x1d6, 0x1d7, 0x1d8, 0x1d9, 0x1da, 0x1db, 0x1dc, 0x1dd, 0x1de,
        0x1df, 0x1e0, 0x1e1, 0x1e2, 0x1e3, 0x1e4, 0x1e5, 0x1e6, 0x1e7, 0x1e8, 0x1e9, 0x1ea, 0x1eb,
        0x1ec, 0x1ed, 0x1ee, 0x1ef, 0x1f0, 0x1f1, 0x1f2, 0x1f3, 0x1f4, 0x1f5, 0x1f6, 0x1f7, 0x1f8,
        0x1f9, 0x1fa, 0x1fb, 0x1fc, 0x1fd, 0x1fe, 0x1ff,
    ]),
    Permutation([
        0x0, 0x4, 0x20, 0x24, 0x100, 0x104, 0x120, 0x124, 0x2, 0x6, 0x22, 0x26, 0x102, 0x106,
        0x122, 0x126, 0x10, 0x14, 0x30, 0x34, 0x110, 0x114, 0x130, 0x134, 0x12, 0x16, 0x32, 0x36,
        0x112, 0x116, 0x132, 0x136, 0x80, 0x84, 0xa0, 0xa4, 0x180, 0x184, 0x1a0, 0x1a4, 0x82, 0x86,
        0xa2, 0xa6, 0x182, 0x186, 0x1a2, 0x1a6, 0x90, 0x94, 0xb0, 0xb4, 0x190, 0x194, 0x1b0, 0x1b4,
        0x92, 0x96, 0xb2, 0xb6, 0x192, 0x196, 0x1b2, 0x1b6, 0x1, 0x5, 0x21, 0x25, 0x101, 0x105,
        0x121, 0x125, 0x3, 0x7, 0x23, 0x27, 0x103, 0x107, 0x123, 0x127, 0x11, 0x15, 0x31, 0x35,
        0x111, 0x115, 0x131, 0x135, 0x13, 0x17, 0x33, 0x37, 0x113, 0x117, 0x133, 0x137, 0x81, 0x85,
        0xa1, 0xa5, 0x181, 0x185, 0x1a1, 0x1a5, 0x83, 0x87, 0xa3, 0xa7, 0x183, 0x187, 0x1a3, 0x1a7,
        0x91, 0x95, 0xb1, 0xb5, 0x191, 0x195, 0x1b1, 0x1b5, 0x93, 0x97, 0xb3, 0xb7, 0x193, 0x197,
        0x1b3, 0x1b7, 0x8, 0xc, 0x28, 0x2c, 0x108, 0x10c, 0x128, 0x12c, 0xa, 0xe, 0x2a, 0x2e,
        0x10a, 0x10e, 0x12a, 0x12e, 0x18, 0x1c, 0x38, 0x3c, 0x118, 0x11c, 0x138, 0x13c, 0x1a, 0x1e,
        0x3a, 0x3e, 0x11a, 0x11e, 0x13a, 0x13e, 0x88, 0x8c, 0xa8, 0xac, 0x188, 0x18c, 0x1a8, 0x1ac,
        0x8a, 0x8e, 0xaa, 0xae, 0x18a, 0x18e, 0x1aa, 0x1ae, 0x98, 0x9c, 0xb8, 0xbc, 0x198, 0x19c,
        0x1b8, 0x1bc, 0x9a, 0x9e, 0xba, 0xbe, 0x19a, 0x19e, 0x1ba, 0x1be, 0x9, 0xd, 0x29, 0x2d,
        0x109, 0x10d, 0x129, 0x12d, 0xb, 0xf, 0x2b, 0x2f, 0x10b, 0x10f, 0x12b, 0x12f, 0x19, 0x1d,
        0x39, 0x3d, 0x119, 0x11d, 0x139, 0x13d, 0x1b, 0x1f, 0x3b, 0x3f, 0x11b, 0x11f, 0x13b, 0x13f,
        0x89, 0x8d, 0xa9, 0xad, 0x189, 0x18d, 0x1a9, 0x1ad, 0x8b, 0x8f, 0xab, 0xaf, 0x18b, 0x18f,
        0x1ab, 0x1af, 0x99, 0x9d, 0xb9, 0xbd, 0x199, 0x19d, 0x1b9, 0x1bd, 0x9b, 0x9f, 0xbb, 0xbf,
        0x19b, 0x19f, 0x1bb, 0x1bf, 0x40, 0x44, 0x60, 0x64, 0x140, 0x144, 0x160, 0x164, 0x42, 0x46,
        0x62, 0x66, 0x142, 0x146, 0x162, 0x166, 0x50, 0x54, 0x70, 0x74, 0x150, 0x154, 0x170, 0x174,
        0x52, 0x56, 0x72, 0x76, 0x152, 0x156, 0x172, 0x176, 0xc0, 0xc4, 0xe0, 0xe4, 0x1c0, 0x1c4,
        0x1e0, 0x1e4, 0xc2, 0xc6, 0xe2, 0xe6, 0x1c2, 0x1c6, 0x1e2, 0x1e6, 0xd0, 0xd4, 0xf0, 0xf4,
        0x1d0, 0x1d4, 0x1f0, 0x1f4, 0xd2, 0xd6, 0xf2, 0xf6, 0x1d2, 0x1d6, 0x1f2, 0x1f6, 0x41, 0x45,
        0x61, 0x65, 0x141, 0x145, 0x161, 0x165, 0x43, 0x47, 0x63, 0x67, 0x143, 0x147, 0x163, 0x167,
        0x51, 0x55, 0x71, 0x75, 0x151, 0x155, 0x171, 0x175, 0x53, 0x57, 0x73, 0x77, 0x153, 0x157,
        0x173, 0x177, 0xc1, 0xc5, 0xe1, 0xe5, 0x1c1, 0x1c5, 0x1e1, 0x1e5, 0xc3, 0xc7, 0xe3, 0xe7,
        0x1c3, 0x1c7, 0x1e3, 0x1e7, 0xd1, 0xd5, 0xf1, 0xf5, 0x1d1, 0x1d5, 0x1f1, 0x1f5, 0xd3, 0xd7,
        0xf3, 0xf7, 0x1d3, 0x1d7, 0x1f3, 0x1f7, 0x48, 0x4c, 0x68, 0x6c, 0x148, 0x14c, 0x168, 0x16c,
        0x4a, 0x4e, 0x6a, 0x6e, 0x14a, 0x14e, 0x16a, 0x16e, 0x58, 0x5c, 0x78, 0x7c, 0x158, 0x15c,
        0x178, 0x17c, 0x5a, 0x5e, 0x7a, 0x7e, 0x15a, 0x15e, 0x17a, 0x17e, 0xc8, 0xcc, 0xe8, 0xec,
        0x1c8, 0x1cc, 0x1e8, 0x1ec, 0xca, 0xce, 0xea, 0xee, 0x1ca, 0x1ce, 0x1ea, 0x1ee, 0xd8, 0xdc,
        0xf8, 0xfc, 0x1d8, 0x1dc, 0x1f8, 0x1fc, 0xda, 0xde, 0xfa, 0xfe, 0x1da, 0x1de, 0x1fa, 0x1fe,
        0x49, 0x4d, 0x69, 0x6d, 0x149, 0x14d, 0x169, 0x16d, 0x4b, 0x4f, 0x6b, 0x6f, 0x14b, 0x14f,
        0x16b, 0x16f, 0x59, 0x5d, 0x79, 0x7d, 0x159, 0x15d, 0x179, 0x17d, 0x5b, 0x5f, 0x7b, 0x7f,
        0x15b, 0x15f, 0x17b, 0x17f, 0xc9, 0xcd, 0xe9, 0xed, 0x1c9, 0x1cd, 0x1e9, 0x1ed, 0xcb, 0xcf,
        0xeb, 0xef, 0x1cb, 0x1cf, 0x1eb, 0x1ef, 0xd9, 0xdd, 0xf9, 0xfd, 0x1d9, 0x1dd, 0x1f9, 0x1fd,
        0xdb, 0xdf, 0xfb, 0xff, 0x1db, 0x1df, 0x1fb, 0x1ff,
    ]),
    Permutation([
        0x0, 0x100, 0x80, 0x180, 0x40, 0x140, 0xc0, 0x1c0, 0x20, 0x120, 0xa0, 0x1a0, 0x60, 0x160,
        0xe0, 0x1e0, 0x10, 0x110, 0x90, 0x190, 0x50, 0x150, 0xd0, 0x1d0, 0x30, 0x130, 0xb0, 0x1b0,
        0x70, 0x170, 0xf0, 0x1f0, 0x8, 0x108, 0x88, 0x188, 0x48, 0x148, 0xc8, 0x1c8, 0x28, 0x128,
        0xa8, 0x1a8, 0x68, 0x168, 0xe8, 0x1e8, 0x18, 0x118, 0x98, 0x198, 0x58, 0x158, 0xd8, 0x1d8,
        0x38, 0x138, 0xb8, 0x1b8, 0x78, 0x178, 0xf8, 0x1f8, 0x4, 0x104, 0x84, 0x184, 0x44, 0x144,
        0xc4, 0x1c4, 0x24, 0x124, 0xa4, 0x1a4, 0x64, 0x164, 0xe4, 0x1e4, 0x14, 0x114, 0x94, 0x194,
        0x54, 0x154, 0xd4, 0x1d4, 0x34, 0x134, 0xb4, 0x1b4, 0x74, 0x174, 0xf4, 0x1f4, 0xc, 0x10c,
        0x8c, 0x18c, 0x4c, 0x14c, 0xcc, 0x1cc, 0x2c, 0x12c, 0xac, 0x1ac, 0x6c, 0x16c, 0xec, 0x1ec,
        0x1c, 0x11c, 0x9c, 0x19c, 0x5c, 0x15c, 0xdc, 0x1dc, 0x3c, 0x13c, 0xbc, 0x1bc, 0x7c, 0x17c,
        0xfc, 0x1fc, 0x2, 0x102, 0x82, 0x182, 0x42, 0x142, 0xc2, 0x1c2, 0x22, 0x122, 0xa2, 0x1a2,
        0x62, 0x162, 0xe2, 0x1e2, 0x12, 0x112, 0x92, 0x192, 0x52, 0x152, 0xd2, 0x1d2, 0x32, 0x132,
        0xb2, 0x1b2, 0x72, 0x172, 0xf2, 0x1f2, 0xa, 0x10a, 0x8a, 0x18a, 0x4a, 0x14a, 0xca, 0x1ca,
        0x2a, 0x12a, 0xaa, 0x1aa, 0x6a, 0x16a, 0xea, 0x1ea, 0x1a, 0x11a, 0x9a, 0x19a, 0x5a, 0x15a,
        0xda, 0x1da, 0x3a, 0x13a, 0xba, 0x1ba, 0x7a, 0x17a, 0xfa, 0x1fa, 0x6, 0x106, 0x86, 0x186,
        0x46, 0x146, 0xc6, 0x1c6, 0x26, 0x126, 0xa6, 0x1a6, 0x66, 0x166, 0xe6, 0x1e6, 0x16, 0x116,
        0x96, 0x196, 0x56, 0x156, 0xd6, 0x1d6, 0x36, 0x136, 0xb6, 0x1b6, 0x76, 0x176, 0xf6, 0x1f6,
        0xe, 0x10e, 0x8e, 0x18e, 0x4e, 0x14e, 0xce, 0x1ce, 0x2e, 0x12e, 0xae, 0x1ae, 0x6e, 0x16e,
        0xee, 0x1ee, 0x1e, 0x11e, 0x9e, 0x19e, 0x5e, 0x15e, 0xde, 0x1de, 0x3e, 0x13e, 0xbe, 0x1be,
        0x7e, 0x17e, 0xfe, 0x1fe, 0x1, 0x101, 0x81, 0x181, 0x41, 0x141, 0xc1, 0x1c1, 0x21, 0x121,
        0xa1, 0x1a1, 0x61, 0x161, 0xe1, 0x1e1, 0x11, 0x111, 0x91, 0x191, 0x51, 0x151, 0xd1, 0x1d1,
        0x31, 0x131, 0xb1, 0x1b1, 0x71, 0x171, 0xf1, 0x1f1, 0x9, 0x109, 0x89, 0x189, 0x49, 0x149,
        0xc9, 0x1c9, 0x29, 0x129, 0xa9, 0x1a9, 0x69, 0x169, 0xe9, 0x1e9, 0x19, 0x119, 0x99, 0x199,
        0x59, 0x159, 0xd9, 0x1d9, 0x39, 0x139, 0xb9, 0x1b9, 0x79, 0x179, 0xf9, 0x1f9, 0x5, 0x105,
        0x85, 0x185, 0x45, 0x145, 0xc5, 0x1c5, 0x25, 0x125, 0xa5, 0x1a5, 0x65, 0x165, 0xe5, 0x1e5,
        0x15, 0x115, 0x95, 0x195, 0x55, 0x155, 0xd5, 0x1d5, 0x35, 0x135, 0xb5, 0x1b5, 0x75, 0x175,
        0xf5, 0x1f5, 0xd, 0x10d, 0x8d, 0x18d, 0x4d, 0x14d, 0xcd, 0x1cd, 0x2d, 0x12d, 0xad, 0x1ad,
        0x6d, 0x16d, 0xed, 0x1ed, 0x1d, 0x11d, 0x9d, 0x19d, 0x5d, 0x15d, 0xdd, 0x1dd, 0x3d, 0x13d,
        0xbd, 0x1bd, 0x7d, 0x17d, 0xfd, 0x1fd, 0x3, 0x103, 0x83, 0x183, 0x43, 0x143, 0xc3, 0x1c3,
        0x23, 0x123, 0xa3, 0x1a3, 0x63, 0x163, 0xe3, 0x1e3, 0x13, 0x113, 0x93, 0x193, 0x53, 0x153,
        0xd3, 0x1d3, 0x33, 0x133, 0xb3, 0x1b3, 0x73, 0x173, 0xf3, 0x1f3, 0xb, 0x10b, 0x8b, 0x18b,
        0x4b, 0x14b, 0xcb, 0x1cb, 0x2b, 0x12b, 0xab, 0x1ab, 0x6b, 0x16b, 0xeb, 0x1eb, 0x1b, 0x11b,
        0x9b, 0x19b, 0x5b, 0x15b, 0xdb, 0x1db, 0x3b, 0x13b, 0xbb, 0x1bb, 0x7b, 0x17b, 0xfb, 0x1fb,
        0x7, 0x107, 0x87, 0x187, 0x47, 0x147, 0xc7, 0x1c7, 0x27, 0x127, 0xa7, 0x1a7, 0x67, 0x167,
        0xe7, 0x1e7, 0x17, 0x117, 0x97, 0x197, 0x57, 0x157, 0xd7, 0x1d7, 0x37, 0x137, 0xb7, 0x1b7,
        0x77, 0x177, 0xf7, 0x1f7, 0xf, 0x10f, 0x8f, 0x18f, 0x4f, 0x14f, 0xcf, 0x1cf, 0x2f, 0x12f,
        0xaf, 0x1af, 0x6f, 0x16f, 0xef, 0x1ef, 0x1f, 0x11f, 0x9f, 0x19f, 0x5f, 0x15f, 0xdf, 0x1df,
        0x3f, 0x13f, 0xbf, 0x1bf, 0x7f, 0x17f, 0xff, 0x1ff,
    ]),
    Permutation([
        0x0, 0x40, 0x8, 0x48, 0x1, 0x41, 0x9, 0x49, 0x80, 0xc0, 0x88, 0xc8, 0x81, 0xc1, 0x89, 0xc9,
        0x10, 0x50, 0x18, 0x58, 0x11, 0x51, 0x19, 0x59, 0x90, 0xd0, 0x98, 0xd8, 0x91, 0xd1, 0x99,
        0xd9, 0x2, 0x42, 0xa, 0x4a, 0x3, 0x43, 0xb, 0x4b, 0x82, 0xc2, 0x8a, 0xca, 0x83, 0xc3, 0x8b,
        0xcb, 0x12, 0x52, 0x1a, 0x5a, 0x13, 0x53, 0x1b, 0x5b, 0x92, 0xd2, 0x9a, 0xda, 0x93, 0xd3,
        0x9b, 0xdb, 0x100, 0x140, 0x108, 0x148, 0x101, 0x141, 0x109, 0x149, 0x180, 0x1c0, 0x188,
        0x1c8, 0x181, 0x1c1, 0x189, 0x1c9, 0x110, 0x150, 0x118, 0x158, 0x111, 0x151, 0x119, 0x159,
        0x190, 0x1d0, 0x198, 0x1d8, 0x191, 0x1d1, 0x199, 0x1d9, 0x102, 0x142, 0x10a, 0x14a, 0x103,
        0x143, 0x10b, 0x14b, 0x182, 0x1c2, 0x18a, 0x1ca, 0x183, 0x1c3, 0x18b, 0x1cb, 0x112, 0x152,
        0x11a, 0x15a, 0x113, 0x153, 0x11b, 0x15b, 0x192, 0x1d2, 0x19a, 0x1da, 0x193, 0x1d3, 0x19b,
        0x1db, 0x20, 0x60, 0x28, 0x68, 0x21, 0x61, 0x29, 0x69, 0xa0, 0xe0, 0xa8, 0xe8, 0xa1, 0xe1,
        0xa9, 0xe9, 0x30, 0x70, 0x38, 0x78, 0x31, 0x71, 0x39, 0x79, 0xb0, 0xf0, 0xb8, 0xf8, 0xb1,
        0xf1, 0xb9, 0xf9, 0x22, 0x62, 0x2a, 0x6a, 0x23, 0x63, 0x2b, 0x6b, 0xa2, 0xe2, 0xaa, 0xea,
        0xa3, 0xe3, 0xab, 0xeb, 0x32, 0x72, 0x3a, 0x7a, 0x33, 0x73, 0x3b, 0x7b, 0xb2, 0xf2, 0xba,
        0xfa, 0xb3, 0xf3, 0xbb, 0xfb, 0x120, 0x160, 0x128, 0x168, 0x121, 0x161, 0x129, 0x169,
        0x1a0, 0x1e0, 0x1a8, 0x1e8, 0x1a1, 0x1e1, 0x1a9, 0x1e9, 0x130, 0x170, 0x138, 0x178, 0x131,
        0x171, 0x139, 0x179, 0x1b0, 0x1f0, 0x1b8, 0x1f8, 0x1b1, 0x1f1, 0x1b9, 0x1f9, 0x122, 0x162,
        0x12a, 0x16a, 0x123, 0x163, 0x12b, 0x16b, 0x1a2, 0x1e2, 0x1aa, 0x1ea, 0x1a3, 0x1e3, 0x1ab,
        0x1eb, 0x132, 0x172, 0x13a, 0x17a, 0x133, 0x173, 0x13b, 0x17b, 0x1b2, 0x1f2, 0x1ba, 0x1fa,
        0x1b3, 0x1f3, 0x1bb, 0x1fb, 0x4, 0x44, 0xc, 0x4c, 0x5, 0x45, 0xd, 0x4d, 0x84, 0xc4, 0x8c,
        0xcc, 0x85, 0xc5, 0x8d, 0xcd, 0x14, 0x54, 0x1c, 0x5c, 0x15, 0x55, 0x1d, 0x5d, 0x94, 0xd4,
        0x9c, 0xdc, 0x95, 0xd5, 0x9d, 0xdd, 0x6, 0x46, 0xe, 0x4e, 0x7, 0x47, 0xf, 0x4f, 0x86, 0xc6,
        0x8e, 0xce, 0x87, 0xc7, 0x8f, 0xcf, 0x16, 0x56, 0x1e, 0x5e, 0x17, 0x57, 0x1f, 0x5f, 0x96,
        0xd6, 0x9e, 0xde, 0x97, 0xd7, 0x9f, 0xdf, 0x104, 0x144, 0x10c, 0x14c, 0x105, 0x145, 0x10d,
        0x14d, 0x184, 0x1c4, 0x18c, 0x1cc, 0x185, 0x1c5, 0x18d, 0x1cd, 0x114, 0x154, 0x11c, 0x15c,
        0x115, 0x155, 0x11d, 0x15d, 0x194, 0x1d4, 0x19c, 0x1dc, 0x195, 0x1d5, 0x19d, 0x1dd, 0x106,
        0x146, 0x10e, 0x14e, 0x107, 0x147, 0x10f, 0x14f, 0x186, 0x1c6, 0x18e, 0x1ce, 0x187, 0x1c7,
        0x18f, 0x1cf, 0x116, 0x156, 0x11e, 0x15e, 0x117, 0x157, 0x11f, 0x15f, 0x196, 0x1d6, 0x19e,
        0x1de, 0x197, 0x1d7, 0x19f, 0x1df, 0x24, 0x64, 0x2c, 0x6c, 0x25, 0x65, 0x2d, 0x6d, 0xa4,
        0xe4, 0xac, 0xec, 0xa5, 0xe5, 0xad, 0xed, 0x34, 0x74, 0x3c, 0x7c, 0x35, 0x75, 0x3d, 0x7d,
        0xb4, 0xf4, 0xbc, 0xfc, 0xb5, 0xf5, 0xbd, 0xfd, 0x26, 0x66, 0x2e, 0x6e, 0x27, 0x67, 0x2f,
        0x6f, 0xa6, 0xe6, 0xae, 0xee, 0xa7, 0xe7, 0xaf, 0xef, 0x36, 0x76, 0x3e, 0x7e, 0x37, 0x77,
        0x3f, 0x7f, 0xb6, 0xf6, 0xbe, 0xfe, 0xb7, 0xf7, 0xbf, 0xff, 0x124, 0x164, 0x12c, 0x16c,
        0x125, 0x165, 0x12d, 0x16d, 0x1a4, 0x1e4, 0x1ac, 0x1ec, 0x1a5, 0x1e5, 0x1ad, 0x1ed, 0x134,
        0x174, 0x13c, 0x17c, 0x135, 0x175, 0x13d, 0x17d, 0x1b4, 0x1f4, 0x1bc, 0x1fc, 0x1b5, 0x1f5,
        0x1bd, 0x1fd, 0x126, 0x166, 0x12e, 0x16e, 0x127, 0x167, 0x12f, 0x16f, 0x1a6, 0x1e6, 0x1ae,
        0x1ee, 0x1a7, 0x1e7, 0x1af, 0x1ef, 0x136, 0x176, 0x13e, 0x17e, 0x137, 0x177, 0x13f, 0x17f,
        0x1b6, 0x1f6, 0x1be, 0x1fe, 0x1b7, 0x1f7, 0x1bf, 0x1ff,
    ]),
    Permutation([
        0x0, 0x4, 0x2, 0x6, 0x1, 0x5, 0x3, 0x7, 0x20, 0x24, 0x22, 0x26, 0x21, 0x25, 0x23, 0x27,
        0x10, 0x14, 0x12, 0x16, 0x11, 0x15, 0x13, 0x17, 0x30, 0x34, 0x32, 0x36, 0x31, 0x35, 0x33,
        0x37, 0x8, 0xc, 0xa, 0xe, 0x9, 0xd, 0xb, 0xf, 0x28, 0x2c, 0x2a, 0x2e, 0x29, 0x2d, 0x2b,
        0x2f, 0x18, 0x1c, 0x1a, 0x1e, 0x19, 0x1d, 0x1b, 0x1f, 0x38, 0x3c, 0x3a, 0x3e, 0x39, 0x3d,
        0x3b, 0x3f, 0x100, 0x104, 0x102, 0x106, 0x101, 0x105, 0x103, 0x107, 0x120, 0x124, 0x122,
        0x126, 0x121, 0x125, 0x123, 0x127, 0x110, 0x114, 0x112, 0x116, 0x111, 0x115, 0x113, 0x117,
        0x130, 0x134, 0x132, 0x136, 0x131, 0x135, 0x133, 0x137, 0x108, 0x10c, 0x10a, 0x10e, 0x109,
        0x10d, 0x10b, 0x10f, 0x128, 0x12c, 0x12a, 0x12e, 0x129, 0x12d, 0x12b, 0x12f, 0x118, 0x11c,
        0x11a, 0x11e, 0x119, 0x11d, 0x11b, 0x11f, 0x138, 0x13c, 0x13a, 0x13e, 0x139, 0x13d, 0x13b,
        0x13f, 0x80, 0x84, 0x82, 0x86, 0x81, 0x85, 0x83, 0x87, 0xa0, 0xa4, 0xa2, 0xa6, 0xa1, 0xa5,
        0xa3, 0xa7, 0x90, 0x94, 0x92, 0x96, 0x91, 0x95, 0x93, 0x97, 0xb0, 0xb4, 0xb2, 0xb6, 0xb1,
        0xb5, 0xb3, 0xb7, 0x88, 0x8c, 0x8a, 0x8e, 0x89, 0x8d, 0x8b, 0x8f, 0xa8, 0xac, 0xaa, 0xae,
        0xa9, 0xad, 0xab, 0xaf, 0x98, 0x9c, 0x9a, 0x9e, 0x99, 0x9d, 0x9b, 0x9f, 0xb8, 0xbc, 0xba,
        0xbe, 0xb9, 0xbd, 0xbb, 0xbf, 0x180, 0x184, 0x182, 0x186, 0x181, 0x185, 0x183, 0x187,
        0x1a0, 0x1a4, 0x1a2, 0x1a6, 0x1a1, 0x1a5, 0x1a3, 0x1a7, 0x190, 0x194, 0x192, 0x196, 0x191,
        0x195, 0x193, 0x197, 0x1b0, 0x1b4, 0x1b2, 0x1b6, 0x1b1, 0x1b5, 0x1b3, 0x1b7, 0x188, 0x18c,
        0x18a, 0x18e, 0x189, 0x18d, 0x18b, 0x18f, 0x1a8, 0x1ac, 0x1aa, 0x1ae, 0x1a9, 0x1ad, 0x1ab,
        0x1af, 0x198, 0x19c, 0x19a, 0x19e, 0x199, 0x19d, 0x19b, 0x19f, 0x1b8, 0x1bc, 0x1ba, 0x1be,
        0x1b9, 0x1bd, 0x1bb, 0x1bf, 0x40, 0x44, 0x42, 0x46, 0x41, 0x45, 0x43, 0x47, 0x60, 0x64,
        0x62, 0x66, 0x61, 0x65, 0x63, 0x67, 0x50, 0x54, 0x52, 0x56, 0x51, 0x55, 0x53, 0x57, 0x70,
        0x74, 0x72, 0x76, 0x71, 0x75, 0x73, 0x77, 0x48, 0x4c, 0x4a, 0x4e, 0x49, 0x4d, 0x4b, 0x4f,
        0x68, 0x6c, 0x6a, 0x6e, 0x69, 0x6d, 0x6b, 0x6f, 0x58, 0x5c, 0x5a, 0x5e, 0x59, 0x5d, 0x5b,
        0x5f, 0x78, 0x7c, 0x7a, 0x7e, 0x79, 0x7d, 0x7b, 0x7f, 0x140, 0x144, 0x142, 0x146, 0x141,
        0x145, 0x143, 0x147, 0x160, 0x164, 0x162, 0x166, 0x161, 0x165, 0x163, 0x167, 0x150, 0x154,
        0x152, 0x156, 0x151, 0x155, 0x153, 0x157, 0x170, 0x174, 0x172, 0x176, 0x171, 0x175, 0x173,
        0x177, 0x148, 0x14c, 0x14a, 0x14e, 0x149, 0x14d, 0x14b, 0x14f, 0x168, 0x16c, 0x16a, 0x16e,
        0x169, 0x16d, 0x16b, 0x16f, 0x158, 0x15c, 0x15a, 0x15e, 0x159, 0x15d, 0x15b, 0x15f, 0x178,
        0x17c, 0x17a, 0x17e, 0x179, 0x17d, 0x17b, 0x17f, 0xc0, 0xc4, 0xc2, 0xc6, 0xc1, 0xc5, 0xc3,
        0xc7, 0xe0, 0xe4, 0xe2, 0xe6, 0xe1, 0xe5, 0xe3, 0xe7, 0xd0, 0xd4, 0xd2, 0xd6, 0xd1, 0xd5,
        0xd3, 0xd7, 0xf0, 0xf4, 0xf2, 0xf6, 0xf1, 0xf5, 0xf3, 0xf7, 0xc8, 0xcc, 0xca, 0xce, 0xc9,
        0xcd, 0xcb, 0xcf, 0xe8, 0xec, 0xea, 0xee, 0xe9, 0xed, 0xeb, 0xef, 0xd8, 0xdc, 0xda, 0xde,
        0xd9, 0xdd, 0xdb, 0xdf, 0xf8, 0xfc, 0xfa, 0xfe, 0xf9, 0xfd, 0xfb, 0xff, 0x1c0, 0x1c4,
        0x1c2, 0x1c6, 0x1c1, 0x1c5, 0x1c3, 0x1c7, 0x1e0, 0x1e4, 0x1e2, 0x1e6, 0x1e1, 0x1e5, 0x1e3,
        0x1e7, 0x1d0, 0x1d4, 0x1d2, 0x1d6, 0x1d1, 0x1d5, 0x1d3, 0x1d7, 0x1f0, 0x1f4, 0x1f2, 0x1f6,
        0x1f1, 0x1f5, 0x1f3, 0x1f7, 0x1c8, 0x1cc, 0x1ca, 0x1ce, 0x1c9, 0x1cd, 0x1cb, 0x1cf, 0x1e8,
        0x1ec, 0x1ea, 0x1ee, 0x1e9, 0x1ed, 0x1eb, 0x1ef, 0x1d8, 0x1dc, 0x1da, 0x1de, 0x1d9, 0x1dd,
        0x1db, 0x1df, 0x1f8, 0x1fc, 0x1fa, 0x1fe, 0x1f9, 0x1fd, 0x1fb, 0x1ff,
    ]),
    Permutation([
        0x0, 0x1, 0x8, 0x9, 0x40, 0x41, 0x48, 0x49, 0x2, 0x3, 0xa, 0xb, 0x42, 0x43, 0x4a, 0x4b,
        0x10, 0x11, 0x18, 0x19, 0x50, 0x51, 0x58, 0x59, 0x12, 0x13, 0x1a, 0x1b, 0x52, 0x53, 0x5a,
        0x5b, 0x80, 0x81, 0x88, 0x89, 0xc0, 0xc1, 0xc8, 0xc9, 0x82, 0x83, 0x8a, 0x8b, 0xc2, 0xc3,
        0xca, 0xcb, 0x90, 0x91, 0x98, 0x99, 0xd0, 0xd1, 0xd8, 0xd9, 0x92, 0x93, 0x9a, 0x9b, 0xd2,
        0xd3, 0xda, 0xdb, 0x4, 0x5, 0xc, 0xd, 0x44, 0x45, 0x4c, 0x4d, 0x6, 0x7, 0xe, 0xf, 0x46,
        0x47, 0x4e, 0x4f, 0x14, 0x15, 0x1c, 0x1d, 0x54, 0x55, 0x5c, 0x5d, 0x16, 0x17, 0x1e, 0x1f,
        0x56, 0x57, 0x5e, 0x5f, 0x84, 0x85, 0x8c, 0x8d, 0xc4, 0xc5, 0xcc, 0xcd, 0x86, 0x87, 0x8e,
        0x8f, 0xc6, 0xc7, 0xce, 0xcf, 0x94, 0x95, 0x9c, 0x9d, 0xd4, 0xd5, 0xdc, 0xdd, 0x96, 0x97,
        0x9e, 0x9f, 0xd6, 0xd7, 0xde, 0xdf, 0x20, 0x21, 0x28, 0x29, 0x60, 0x61, 0x68, 0x69, 0x22,
        0x23, 0x2a, 0x2b, 0x62, 0x63, 0x6a, 0x6b, 0x30, 0x31, 0x38, 0x39, 0x70, 0x71, 0x78, 0x79,
        0x32, 0x33, 0x3a, 0x3b, 0x72, 0x73, 0x7a, 0x7b, 0xa0, 0xa1, 0xa8, 0xa9, 0xe0, 0xe1, 0xe8,
        0xe9, 0xa2, 0xa3, 0xaa, 0xab, 0xe2, 0xe3, 0xea, 0xeb, 0xb0, 0xb1, 0xb8, 0xb9, 0xf0, 0xf1,
        0xf8, 0xf9, 0xb2, 0xb3, 0xba, 0xbb, 0xf2, 0xf3, 0xfa, 0xfb, 0x24, 0x25, 0x2c, 0x2d, 0x64,
        0x65, 0x6c, 0x6d, 0x26, 0x27, 0x2e, 0x2f, 0x66, 0x67, 0x6e, 0x6f, 0x34, 0x35, 0x3c, 0x3d,
        0x74, 0x75, 0x7c, 0x7d, 0x36, 0x37, 0x3e, 0x3f, 0x76, 0x77, 0x7e, 0x7f, 0xa4, 0xa5, 0xac,
        0xad, 0xe4, 0xe5, 0xec, 0xed, 0xa6, 0xa7, 0xae, 0xaf, 0xe6, 0xe7, 0xee, 0xef, 0xb4, 0xb5,
        0xbc, 0xbd, 0xf4, 0xf5, 0xfc, 0xfd, 0xb6, 0xb7, 0xbe, 0xbf, 0xf6, 0xf7, 0xfe, 0xff, 0x100,
        0x101, 0x108, 0x109, 0x140, 0x141, 0x148, 0x149, 0x102, 0x103, 0x10a, 0x10b, 0x142, 0x143,
        0x14a, 0x14b, 0x110, 0x111, 0x118, 0x119, 0x150, 0x151, 0x158, 0x159, 0x112, 0x113, 0x11a,
        0x11b, 0x152, 0x153, 0x15a, 0x15b, 0x180, 0x181, 0x188, 0x189, 0x1c0, 0x1c1, 0x1c8, 0x1c9,
        0x182, 0x183, 0x18a, 0x18b, 0x1c2, 0x1c3, 0x1ca, 0x1cb, 0x190, 0x191, 0x198, 0x199, 0x1d0,
        0x1d1, 0x1d8, 0x1d9, 0x192, 0x193, 0x19a, 0x19b, 0x1d2, 0x1d3, 0x1da, 0x1db, 0x104, 0x105,
        0x10c, 0x10d, 0x144, 0x145, 0x14c, 0x14d, 0x106, 0x107, 0x10e, 0x10f, 0x146, 0x147, 0x14e,
        0x14f, 0x114, 0x115, 0x11c, 0x11d, 0x154, 0x155, 0x15c, 0x15d, 0x116, 0x117, 0x11e, 0x11f,
        0x156, 0x157, 0x15e, 0x15f, 0x184, 0x185, 0x18c, 0x18d, 0x1c4, 0x1c5, 0x1cc, 0x1cd, 0x186,
        0x187, 0x18e, 0x18f, 0x1c6, 0x1c7, 0x1ce, 0x1cf, 0x194, 0x195, 0x19c, 0x19d, 0x1d4, 0x1d5,
        0x1dc, 0x1dd, 0x196, 0x197, 0x19e, 0x19f, 0x1d6, 0x1d7, 0x1de, 0x1df, 0x120, 0x121, 0x128,
        0x129, 0x160, 0x161, 0x168, 0x169, 0x122, 0x123, 0x12a, 0x12b, 0x162, 0x163, 0x16a, 0x16b,
        0x130, 0x131, 0x138, 0x139, 0x170, 0x171, 0x178, 0x179, 0x132, 0x133, 0x13a, 0x13b, 0x172,
        0x173, 0x17a, 0x17b, 0x1a0, 0x1a1, 0x1a8, 0x1a9, 0x1e0, 0x1e1, 0x1e8, 0x1e9, 0x1a2, 0x1a3,
        0x1aa, 0x1ab, 0x1e2, 0x1e3, 0x1ea, 0x1eb, 0x1b0, 0x1b1, 0x1b8, 0x1b9, 0x1f0, 0x1f1, 0x1f8,
        0x1f9, 0x1b2, 0x1b3, 0x1ba, 0x1bb, 0x1f2, 0x1f3, 0x1fa, 0x1fb, 0x124, 0x125, 0x12c, 0x12d,
        0x164, 0x165, 0x16c, 0x16d, 0x126, 0x127, 0x12e, 0x12f, 0x166, 0x167, 0x16e, 0x16f, 0x134,
        0x135, 0x13c, 0x13d, 0x174, 0x175, 0x17c, 0x17d, 0x136, 0x137, 0x13e, 0x13f, 0x176, 0x177,
        0x17e, 0x17f, 0x1a4, 0x1a5, 0x1ac, 0x1ad, 0x1e4, 0x1e5, 0x1ec, 0x1ed, 0x1a6, 0x1a7, 0x1ae,
        0x1af, 0x1e6, 0x1e7, 0x1ee, 0x1ef, 0x1b4, 0x1b5, 0x1bc, 0x1bd, 0x1f4, 0x1f5, 0x1fc, 0x1fd,
        0x1b6, 0x1b7, 0x1be, 0x1bf, 0x1f6, 0x1f7, 0x1fe, 0x1ff,
    ]),
    Permutation([
        0x0, 0x40, 0x80, 0xc0, 0x100, 0x140, 0x180, 0x1c0, 0x8, 0x48, 0x88, 0xc8, 0x108, 0x148,
        0x188, 0x1c8, 0x10, 0x50, 0x90, 0xd0, 0x110, 0x150, 0x190, 0x1d0, 0x18, 0x58, 0x98, 0xd8,
        0x118, 0x158, 0x198, 0x1d8, 0x20, 0x60, 0xa0, 0xe0, 0x120, 0x160, 0x1a0, 0x1e0, 0x28, 0x68,
        0xa8, 0xe8, 0x128, 0x168, 0x1a8, 0x1e8, 0x30, 0x70, 0xb0, 0xf0, 0x130, 0x170, 0x1b0, 0x1f0,
        0x38, 0x78, 0xb8, 0xf8, 0x138, 0x178, 0x1b8, 0x1f8, 0x1, 0x41, 0x81, 0xc1, 0x101, 0x141,
        0x181, 0x1c1, 0x9, 0x49, 0x89, 0xc9, 0x109, 0x149, 0x189, 0x1c9, 0x11, 0x51, 0x91, 0xd1,
        0x111, 0x151, 0x191, 0x1d1, 0x19, 0x59, 0x99, 0xd9, 0x119, 0x159, 0x199, 0x1d9, 0x21, 0x61,
        0xa1, 0xe1, 0x121, 0x161, 0x1a1, 0x1e1, 0x29, 0x69, 0xa9, 0xe9, 0x129, 0x169, 0x1a9, 0x1e9,
        0x31, 0x71, 0xb1, 0xf1, 0x131, 0x171, 0x1b1, 0x1f1, 0x39, 0x79, 0xb9, 0xf9, 0x139, 0x179,
        0x1b9, 0x1f9, 0x2, 0x42, 0x82, 0xc2, 0x102, 0x142, 0x182, 0x1c2, 0xa, 0x4a, 0x8a, 0xca,
        0x10a, 0x14a, 0x18a, 0x1ca, 0x12, 0x52, 0x92, 0xd2, 0x112, 0x152, 0x192, 0x1d2, 0x1a, 0x5a,
        0x9a, 0xda, 0x11a, 0x15a, 0x19a, 0x1da, 0x22, 0x62, 0xa2, 0xe2, 0x122, 0x162, 0x1a2, 0x1e2,
        0x2a, 0x6a, 0xaa, 0xea, 0x12a, 0x16a, 0x1aa, 0x1ea, 0x32, 0x72, 0xb2, 0xf2, 0x132, 0x172,
        0x1b2, 0x1f2, 0x3a, 0x7a, 0xba, 0xfa, 0x13a, 0x17a, 0x1ba, 0x1fa, 0x3, 0x43, 0x83, 0xc3,
        0x103, 0x143, 0x183, 0x1c3, 0xb, 0x4b, 0x8b, 0xcb, 0x10b, 0x14b, 0x18b, 0x1cb, 0x13, 0x53,
        0x93, 0xd3, 0x113, 0x153, 0x193, 0x1d3, 0x1b, 0x5b, 0x9b, 0xdb, 0x11b, 0x15b, 0x19b, 0x1db,
        0x23, 0x63, 0xa3, 0xe3, 0x123, 0x163, 0x1a3, 0x1e3, 0x2b, 0x6b, 0xab, 0xeb, 0x12b, 0x16b,
        0x1ab, 0x1eb, 0x33, 0x73, 0xb3, 0xf3, 0x133, 0x173, 0x1b3, 0x1f3, 0x3b, 0x7b, 0xbb, 0xfb,
        0x13b, 0x17b, 0x1bb, 0x1fb, 0x4, 0x44, 0x84, 0xc4, 0x104, 0x144, 0x184, 0x1c4, 0xc, 0x4c,
        0x8c, 0xcc, 0x10c, 0x14c, 0x18c, 0x1cc, 0x14, 0x54, 0x94, 0xd4, 0x114, 0x154, 0x194, 0x1d4,
        0x1c, 0x5c, 0x9c, 0xdc, 0x11c, 0x15c, 0x19c, 0x1dc, 0x24, 0x64, 0xa4, 0xe4, 0x124, 0x164,
        0x1a4, 0x1e4, 0x2c, 0x6c, 0xac, 0xec, 0x12c, 0x16c, 0x1ac, 0x1ec, 0x34, 0x74, 0xb4, 0xf4,
        0x134, 0x174, 0x1b4, 0x1f4, 0x3c, 0x7c, 0xbc, 0xfc, 0x13c, 0x17c, 0x1bc, 0x1fc, 0x5, 0x45,
        0x85, 0xc5, 0x105, 0x145, 0x185, 0x1c5, 0xd, 0x4d, 0x8d, 0xcd, 0x10d, 0x14d, 0x18d, 0x1cd,
        0x15, 0x55, 0x95, 0xd5, 0x115, 0x155, 0x195, 0x1d5, 0x1d, 0x5d, 0x9d, 0xdd, 0x11d, 0x15d,
        0x19d, 0x1dd, 0x25, 0x65, 0xa5, 0xe5, 0x125, 0x165, 0x1a5, 0x1e5, 0x2d, 0x6d, 0xad, 0xed,
        0x12d, 0x16d, 0x1ad, 0x1ed, 0x35, 0x75, 0xb5, 0xf5, 0x135, 0x175, 0x1b5, 0x1f5, 0x3d, 0x7d,
        0xbd, 0xfd, 0x13d, 0x17d, 0x1bd, 0x1fd, 0x6, 0x46, 0x86, 0xc6, 0x106, 0x146, 0x186, 0x1c6,
        0xe, 0x4e, 0x8e, 0xce, 0x10e, 0x14e, 0x18e, 0x1ce, 0x16, 0x56, 0x96, 0xd6, 0x116, 0x156,
        0x196, 0x1d6, 0x1e, 0x5e, 0x9e, 0xde, 0x11e, 0x15e, 0x19e, 0x1de, 0x26, 0x66, 0xa6, 0xe6,
        0x126, 0x166, 0x1a6, 0x1e6, 0x2e, 0x6e, 0xae, 0xee, 0x12e, 0x16e, 0x1ae, 0x1ee, 0x36, 0x76,
        0xb6, 0xf6, 0x136, 0x176, 0x1b6, 0x1f6, 0x3e, 0x7e, 0xbe, 0xfe, 0x13e, 0x17e, 0x1be, 0x1fe,
        0x7, 0x47, 0x87, 0xc7, 0x107, 0x147, 0x187, 0x1c7, 0xf, 0x4f, 0x8f, 0xcf, 0x10f, 0x14f,
        0x18f, 0x1cf, 0x17, 0x57, 0x97, 0xd7, 0x117, 0x157, 0x197, 0x1d7, 0x1f, 0x5f, 0x9f, 0xdf,
        0x11f, 0x15f, 0x19f, 0x1df, 0x27, 0x67, 0xa7, 0xe7, 0x127, 0x167, 0x1a7, 0x1e7, 0x2f, 0x6f,
        0xaf, 0xef, 0x12f, 0x16f, 0x1af, 0x1ef, 0x37, 0x77, 0xb7, 0xf7, 0x137, 0x177, 0x1b7, 0x1f7,
        0x3f, 0x7f, 0xbf, 0xff, 0x13f, 0x17f, 0x1bf, 0x1ff,
    ]),
    Permutation([
        0x0, 0x100, 0x20, 0x120, 0x4, 0x104, 0x24, 0x124, 0x80, 0x180, 0xa0, 0x1a0, 0x84, 0x184,
        0xa4, 0x1a4, 0x10, 0x110, 0x30, 0x130, 0x14, 0x114, 0x34, 0x134, 0x90, 0x190, 0xb0, 0x1b0,
        0x94, 0x194, 0xb4, 0x1b4, 0x2, 0x102, 0x22, 0x122, 0x6, 0x106, 0x26, 0x126, 0x82, 0x182,
        0xa2, 0x1a2, 0x86, 0x186, 0xa6, 0x1a6, 0x12, 0x112, 0x32, 0x132, 0x16, 0x116, 0x36, 0x136,
        0x92, 0x192, 0xb2, 0x1b2, 0x96, 0x196, 0xb6, 0x1b6, 0x40, 0x140, 0x60, 0x160, 0x44, 0x144,
        0x64, 0x164, 0xc0, 0x1c0, 0xe0, 0x1e0, 0xc4, 0x1c4, 0xe4, 0x1e4, 0x50, 0x150, 0x70, 0x170,
        0x54, 0x154, 0x74, 0x174, 0xd0, 0x1d0, 0xf0, 0x1f0, 0xd4, 0x1d4, 0xf4, 0x1f4, 0x42, 0x142,
        0x62, 0x162, 0x46, 0x146, 0x66, 0x166, 0xc2, 0x1c2, 0xe2, 0x1e2, 0xc6, 0x1c6, 0xe6, 0x1e6,
        0x52, 0x152, 0x72, 0x172, 0x56, 0x156, 0x76, 0x176, 0xd2, 0x1d2, 0xf2, 0x1f2, 0xd6, 0x1d6,
        0xf6, 0x1f6, 0x8, 0x108, 0x28, 0x128, 0xc, 0x10c, 0x2c, 0x12c, 0x88, 0x188, 0xa8, 0x1a8,
        0x8c, 0x18c, 0xac, 0x1ac, 0x18, 0x118, 0x38, 0x138, 0x1c, 0x11c, 0x3c, 0x13c, 0x98, 0x198,
        0xb8, 0x1b8, 0x9c, 0x19c, 0xbc, 0x1bc, 0xa, 0x10a, 0x2a, 0x12a, 0xe, 0x10e, 0x2e, 0x12e,
        0x8a, 0x18a, 0xaa, 0x1aa, 0x8e, 0x18e, 0xae, 0x1ae, 0x1a, 0x11a, 0x3a, 0x13a, 0x1e, 0x11e,
        0x3e, 0x13e, 0x9a, 0x19a, 0xba, 0x1ba, 0x9e, 0x19e, 0xbe, 0x1be, 0x48, 0x148, 0x68, 0x168,
        0x4c, 0x14c, 0x6c, 0x16c, 0xc8, 0x1c8, 0xe8, 0x1e8, 0xcc, 0x1cc, 0xec, 0x1ec, 0x58, 0x158,
        0x78, 0x178, 0x5c, 0x15c, 0x7c, 0x17c, 0xd8, 0x1d8, 0xf8, 0x1f8, 0xdc, 0x1dc, 0xfc, 0x1fc,
        0x4a, 0x14a, 0x6a, 0x16a, 0x4e, 0x14e, 0x6e, 0x16e, 0xca, 0x1ca, 0xea, 0x1ea, 0xce, 0x1ce,
        0xee, 0x1ee, 0x5a, 0x15a, 0x7a, 0x17a, 0x5e, 0x15e, 0x7e, 0x17e, 0xda, 0x1da, 0xfa, 0x1fa,
        0xde, 0x1de, 0xfe, 0x1fe, 0x1, 0x101, 0x21, 0x121, 0x5, 0x105, 0x25, 0x125, 0x81, 0x181,
        0xa1, 0x1a1, 0x85, 0x185, 0xa5, 0x1a5, 0x11, 0x111, 0x31, 0x131, 0x15, 0x115, 0x35, 0x135,
        0x91, 0x191, 0xb1, 0x1b1, 0x95, 0x195, 0xb5, 0x1b5, 0x3, 0x103, 0x23, 0x123, 0x7, 0x107,
        0x27, 0x127, 0x83, 0x183, 0xa3, 0x1a3, 0x87, 0x187, 0xa7, 0x1a7, 0x13, 0x113, 0x33, 0x133,
        0x17, 0x117, 0x37, 0x137, 0x93, 0x193, 0xb3, 0x1b3, 0x97, 0x197, 0xb7, 0x1b7, 0x41, 0x141,
        0x61, 0x161, 0x45, 0x145, 0x65, 0x165, 0xc1, 0x1c1, 0xe1, 0x1e1, 0xc5, 0x1c5, 0xe5, 0x1e5,
        0x51, 0x151, 0x71, 0x171, 0x55, 0x155, 0x75, 0x175, 0xd1, 0x1d1, 0xf1, 0x1f1, 0xd5, 0x1d5,
        0xf5, 0x1f5, 0x43, 0x143, 0x63, 0x163, 0x47, 0x147, 0x67, 0x167, 0xc3, 0x1c3, 0xe3, 0x1e3,
        0xc7, 0x1c7, 0xe7, 0x1e7, 0x53, 0x153, 0x73, 0x173, 0x57, 0x157, 0x77, 0x177, 0xd3, 0x1d3,
        0xf3, 0x1f3, 0xd7, 0x1d7, 0xf7, 0x1f7, 0x9, 0x109, 0x29, 0x129, 0xd, 0x10d, 0x2d, 0x12d,
        0x89, 0x189, 0xa9, 0x1a9, 0x8d, 0x18d, 0xad, 0x1ad, 0x19, 0x119, 0x39, 0x139, 0x1d, 0x11d,
        0x3d, 0x13d, 0x99, 0x199, 0xb9, 0x1b9, 0x9d, 0x19d, 0xbd, 0x1bd, 0xb, 0x10b, 0x2b, 0x12b,
        0xf, 0x10f, 0x2f, 0x12f, 0x8b, 0x18b, 0xab, 0x1ab, 0x8f, 0x18f, 0xaf, 0x1af, 0x1b, 0x11b,
        0x3b, 0x13b, 0x1f, 0x11f, 0x3f, 0x13f, 0x9b, 0x19b, 0xbb, 0x1bb, 0x9f, 0x19f, 0xbf, 0x1bf,
        0x49, 0x149, 0x69, 0x169, 0x4d, 0x14d, 0x6d, 0x16d, 0xc9, 0x1c9, 0xe9, 0x1e9, 0xcd, 0x1cd,
        0xed, 0x1ed, 0x59, 0x159, 0x79, 0x179, 0x5d, 0x15d, 0x7d, 0x17d, 0xd9, 0x1d9, 0xf9, 0x1f9,
        0xdd, 0x1dd, 0xfd, 0x1fd, 0x4b, 0x14b, 0x6b, 0x16b, 0x4f, 0x14f, 0x6f, 0x16f, 0xcb, 0x1cb,
        0xeb, 0x1eb, 0xcf, 0x1cf, 0xef, 0x1ef, 0x5b, 0x15b, 0x7b, 0x17b, 0x5f, 0x15f, 0x7f, 0x17f,
        0xdb, 0x1db, 0xfb, 0x1fb, 0xdf, 0x1df, 0xff, 0x1ff,
    ]),
];