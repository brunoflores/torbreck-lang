#[derive(Debug, Copy, Clone)]
pub struct Header {
  pub color: Color,
  pub tag: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Color {
  White,
}

impl Header {
  pub fn new() -> Self {
    Header {
      tag: 0,
      color: Color::White,
    }
  }

  pub fn newtag(t: usize) -> Self {
    Header {
      tag: t,
      color: Color::White,
    }
  }
}

// In Fish:
// for i in (seq 0 255);
//   echo "Header {
//     tag: $i,
//     color: Color::White,
//   },";
// end
pub static FIRST_ATOMS: [Header; 256] = {
  [
    Header {
      tag: 0,
      color: Color::White,
    },
    Header {
      tag: 1,
      color: Color::White,
    },
    Header {
      tag: 2,
      color: Color::White,
    },
    Header {
      tag: 3,
      color: Color::White,
    },
    Header {
      tag: 4,
      color: Color::White,
    },
    Header {
      tag: 5,
      color: Color::White,
    },
    Header {
      tag: 6,
      color: Color::White,
    },
    Header {
      tag: 7,
      color: Color::White,
    },
    Header {
      tag: 8,
      color: Color::White,
    },
    Header {
      tag: 9,
      color: Color::White,
    },
    Header {
      tag: 10,
      color: Color::White,
    },
    Header {
      tag: 11,
      color: Color::White,
    },
    Header {
      tag: 12,
      color: Color::White,
    },
    Header {
      tag: 13,
      color: Color::White,
    },
    Header {
      tag: 14,
      color: Color::White,
    },
    Header {
      tag: 15,
      color: Color::White,
    },
    Header {
      tag: 16,
      color: Color::White,
    },
    Header {
      tag: 17,
      color: Color::White,
    },
    Header {
      tag: 18,
      color: Color::White,
    },
    Header {
      tag: 19,
      color: Color::White,
    },
    Header {
      tag: 20,
      color: Color::White,
    },
    Header {
      tag: 21,
      color: Color::White,
    },
    Header {
      tag: 22,
      color: Color::White,
    },
    Header {
      tag: 23,
      color: Color::White,
    },
    Header {
      tag: 24,
      color: Color::White,
    },
    Header {
      tag: 25,
      color: Color::White,
    },
    Header {
      tag: 26,
      color: Color::White,
    },
    Header {
      tag: 27,
      color: Color::White,
    },
    Header {
      tag: 28,
      color: Color::White,
    },
    Header {
      tag: 29,
      color: Color::White,
    },
    Header {
      tag: 30,
      color: Color::White,
    },
    Header {
      tag: 31,
      color: Color::White,
    },
    Header {
      tag: 32,
      color: Color::White,
    },
    Header {
      tag: 33,
      color: Color::White,
    },
    Header {
      tag: 34,
      color: Color::White,
    },
    Header {
      tag: 35,
      color: Color::White,
    },
    Header {
      tag: 36,
      color: Color::White,
    },
    Header {
      tag: 37,
      color: Color::White,
    },
    Header {
      tag: 38,
      color: Color::White,
    },
    Header {
      tag: 39,
      color: Color::White,
    },
    Header {
      tag: 40,
      color: Color::White,
    },
    Header {
      tag: 41,
      color: Color::White,
    },
    Header {
      tag: 42,
      color: Color::White,
    },
    Header {
      tag: 43,
      color: Color::White,
    },
    Header {
      tag: 44,
      color: Color::White,
    },
    Header {
      tag: 45,
      color: Color::White,
    },
    Header {
      tag: 46,
      color: Color::White,
    },
    Header {
      tag: 47,
      color: Color::White,
    },
    Header {
      tag: 48,
      color: Color::White,
    },
    Header {
      tag: 49,
      color: Color::White,
    },
    Header {
      tag: 50,
      color: Color::White,
    },
    Header {
      tag: 51,
      color: Color::White,
    },
    Header {
      tag: 52,
      color: Color::White,
    },
    Header {
      tag: 53,
      color: Color::White,
    },
    Header {
      tag: 54,
      color: Color::White,
    },
    Header {
      tag: 55,
      color: Color::White,
    },
    Header {
      tag: 56,
      color: Color::White,
    },
    Header {
      tag: 57,
      color: Color::White,
    },
    Header {
      tag: 58,
      color: Color::White,
    },
    Header {
      tag: 59,
      color: Color::White,
    },
    Header {
      tag: 60,
      color: Color::White,
    },
    Header {
      tag: 61,
      color: Color::White,
    },
    Header {
      tag: 62,
      color: Color::White,
    },
    Header {
      tag: 63,
      color: Color::White,
    },
    Header {
      tag: 64,
      color: Color::White,
    },
    Header {
      tag: 65,
      color: Color::White,
    },
    Header {
      tag: 66,
      color: Color::White,
    },
    Header {
      tag: 67,
      color: Color::White,
    },
    Header {
      tag: 68,
      color: Color::White,
    },
    Header {
      tag: 69,
      color: Color::White,
    },
    Header {
      tag: 70,
      color: Color::White,
    },
    Header {
      tag: 71,
      color: Color::White,
    },
    Header {
      tag: 72,
      color: Color::White,
    },
    Header {
      tag: 73,
      color: Color::White,
    },
    Header {
      tag: 74,
      color: Color::White,
    },
    Header {
      tag: 75,
      color: Color::White,
    },
    Header {
      tag: 76,
      color: Color::White,
    },
    Header {
      tag: 77,
      color: Color::White,
    },
    Header {
      tag: 78,
      color: Color::White,
    },
    Header {
      tag: 79,
      color: Color::White,
    },
    Header {
      tag: 80,
      color: Color::White,
    },
    Header {
      tag: 81,
      color: Color::White,
    },
    Header {
      tag: 82,
      color: Color::White,
    },
    Header {
      tag: 83,
      color: Color::White,
    },
    Header {
      tag: 84,
      color: Color::White,
    },
    Header {
      tag: 85,
      color: Color::White,
    },
    Header {
      tag: 86,
      color: Color::White,
    },
    Header {
      tag: 87,
      color: Color::White,
    },
    Header {
      tag: 88,
      color: Color::White,
    },
    Header {
      tag: 89,
      color: Color::White,
    },
    Header {
      tag: 90,
      color: Color::White,
    },
    Header {
      tag: 91,
      color: Color::White,
    },
    Header {
      tag: 92,
      color: Color::White,
    },
    Header {
      tag: 93,
      color: Color::White,
    },
    Header {
      tag: 94,
      color: Color::White,
    },
    Header {
      tag: 95,
      color: Color::White,
    },
    Header {
      tag: 96,
      color: Color::White,
    },
    Header {
      tag: 97,
      color: Color::White,
    },
    Header {
      tag: 98,
      color: Color::White,
    },
    Header {
      tag: 99,
      color: Color::White,
    },
    Header {
      tag: 100,
      color: Color::White,
    },
    Header {
      tag: 101,
      color: Color::White,
    },
    Header {
      tag: 102,
      color: Color::White,
    },
    Header {
      tag: 103,
      color: Color::White,
    },
    Header {
      tag: 104,
      color: Color::White,
    },
    Header {
      tag: 105,
      color: Color::White,
    },
    Header {
      tag: 106,
      color: Color::White,
    },
    Header {
      tag: 107,
      color: Color::White,
    },
    Header {
      tag: 108,
      color: Color::White,
    },
    Header {
      tag: 109,
      color: Color::White,
    },
    Header {
      tag: 110,
      color: Color::White,
    },
    Header {
      tag: 111,
      color: Color::White,
    },
    Header {
      tag: 112,
      color: Color::White,
    },
    Header {
      tag: 113,
      color: Color::White,
    },
    Header {
      tag: 114,
      color: Color::White,
    },
    Header {
      tag: 115,
      color: Color::White,
    },
    Header {
      tag: 116,
      color: Color::White,
    },
    Header {
      tag: 117,
      color: Color::White,
    },
    Header {
      tag: 118,
      color: Color::White,
    },
    Header {
      tag: 119,
      color: Color::White,
    },
    Header {
      tag: 120,
      color: Color::White,
    },
    Header {
      tag: 121,
      color: Color::White,
    },
    Header {
      tag: 122,
      color: Color::White,
    },
    Header {
      tag: 123,
      color: Color::White,
    },
    Header {
      tag: 124,
      color: Color::White,
    },
    Header {
      tag: 125,
      color: Color::White,
    },
    Header {
      tag: 126,
      color: Color::White,
    },
    Header {
      tag: 127,
      color: Color::White,
    },
    Header {
      tag: 128,
      color: Color::White,
    },
    Header {
      tag: 129,
      color: Color::White,
    },
    Header {
      tag: 130,
      color: Color::White,
    },
    Header {
      tag: 131,
      color: Color::White,
    },
    Header {
      tag: 132,
      color: Color::White,
    },
    Header {
      tag: 133,
      color: Color::White,
    },
    Header {
      tag: 134,
      color: Color::White,
    },
    Header {
      tag: 135,
      color: Color::White,
    },
    Header {
      tag: 136,
      color: Color::White,
    },
    Header {
      tag: 137,
      color: Color::White,
    },
    Header {
      tag: 138,
      color: Color::White,
    },
    Header {
      tag: 139,
      color: Color::White,
    },
    Header {
      tag: 140,
      color: Color::White,
    },
    Header {
      tag: 141,
      color: Color::White,
    },
    Header {
      tag: 142,
      color: Color::White,
    },
    Header {
      tag: 143,
      color: Color::White,
    },
    Header {
      tag: 144,
      color: Color::White,
    },
    Header {
      tag: 145,
      color: Color::White,
    },
    Header {
      tag: 146,
      color: Color::White,
    },
    Header {
      tag: 147,
      color: Color::White,
    },
    Header {
      tag: 148,
      color: Color::White,
    },
    Header {
      tag: 149,
      color: Color::White,
    },
    Header {
      tag: 150,
      color: Color::White,
    },
    Header {
      tag: 151,
      color: Color::White,
    },
    Header {
      tag: 152,
      color: Color::White,
    },
    Header {
      tag: 153,
      color: Color::White,
    },
    Header {
      tag: 154,
      color: Color::White,
    },
    Header {
      tag: 155,
      color: Color::White,
    },
    Header {
      tag: 156,
      color: Color::White,
    },
    Header {
      tag: 157,
      color: Color::White,
    },
    Header {
      tag: 158,
      color: Color::White,
    },
    Header {
      tag: 159,
      color: Color::White,
    },
    Header {
      tag: 160,
      color: Color::White,
    },
    Header {
      tag: 161,
      color: Color::White,
    },
    Header {
      tag: 162,
      color: Color::White,
    },
    Header {
      tag: 163,
      color: Color::White,
    },
    Header {
      tag: 164,
      color: Color::White,
    },
    Header {
      tag: 165,
      color: Color::White,
    },
    Header {
      tag: 166,
      color: Color::White,
    },
    Header {
      tag: 167,
      color: Color::White,
    },
    Header {
      tag: 168,
      color: Color::White,
    },
    Header {
      tag: 169,
      color: Color::White,
    },
    Header {
      tag: 170,
      color: Color::White,
    },
    Header {
      tag: 171,
      color: Color::White,
    },
    Header {
      tag: 172,
      color: Color::White,
    },
    Header {
      tag: 173,
      color: Color::White,
    },
    Header {
      tag: 174,
      color: Color::White,
    },
    Header {
      tag: 175,
      color: Color::White,
    },
    Header {
      tag: 176,
      color: Color::White,
    },
    Header {
      tag: 177,
      color: Color::White,
    },
    Header {
      tag: 178,
      color: Color::White,
    },
    Header {
      tag: 179,
      color: Color::White,
    },
    Header {
      tag: 180,
      color: Color::White,
    },
    Header {
      tag: 181,
      color: Color::White,
    },
    Header {
      tag: 182,
      color: Color::White,
    },
    Header {
      tag: 183,
      color: Color::White,
    },
    Header {
      tag: 184,
      color: Color::White,
    },
    Header {
      tag: 185,
      color: Color::White,
    },
    Header {
      tag: 186,
      color: Color::White,
    },
    Header {
      tag: 187,
      color: Color::White,
    },
    Header {
      tag: 188,
      color: Color::White,
    },
    Header {
      tag: 189,
      color: Color::White,
    },
    Header {
      tag: 190,
      color: Color::White,
    },
    Header {
      tag: 191,
      color: Color::White,
    },
    Header {
      tag: 192,
      color: Color::White,
    },
    Header {
      tag: 193,
      color: Color::White,
    },
    Header {
      tag: 194,
      color: Color::White,
    },
    Header {
      tag: 195,
      color: Color::White,
    },
    Header {
      tag: 196,
      color: Color::White,
    },
    Header {
      tag: 197,
      color: Color::White,
    },
    Header {
      tag: 198,
      color: Color::White,
    },
    Header {
      tag: 199,
      color: Color::White,
    },
    Header {
      tag: 200,
      color: Color::White,
    },
    Header {
      tag: 201,
      color: Color::White,
    },
    Header {
      tag: 202,
      color: Color::White,
    },
    Header {
      tag: 203,
      color: Color::White,
    },
    Header {
      tag: 204,
      color: Color::White,
    },
    Header {
      tag: 205,
      color: Color::White,
    },
    Header {
      tag: 206,
      color: Color::White,
    },
    Header {
      tag: 207,
      color: Color::White,
    },
    Header {
      tag: 208,
      color: Color::White,
    },
    Header {
      tag: 209,
      color: Color::White,
    },
    Header {
      tag: 210,
      color: Color::White,
    },
    Header {
      tag: 211,
      color: Color::White,
    },
    Header {
      tag: 212,
      color: Color::White,
    },
    Header {
      tag: 213,
      color: Color::White,
    },
    Header {
      tag: 214,
      color: Color::White,
    },
    Header {
      tag: 215,
      color: Color::White,
    },
    Header {
      tag: 216,
      color: Color::White,
    },
    Header {
      tag: 217,
      color: Color::White,
    },
    Header {
      tag: 218,
      color: Color::White,
    },
    Header {
      tag: 219,
      color: Color::White,
    },
    Header {
      tag: 220,
      color: Color::White,
    },
    Header {
      tag: 221,
      color: Color::White,
    },
    Header {
      tag: 222,
      color: Color::White,
    },
    Header {
      tag: 223,
      color: Color::White,
    },
    Header {
      tag: 224,
      color: Color::White,
    },
    Header {
      tag: 225,
      color: Color::White,
    },
    Header {
      tag: 226,
      color: Color::White,
    },
    Header {
      tag: 227,
      color: Color::White,
    },
    Header {
      tag: 228,
      color: Color::White,
    },
    Header {
      tag: 229,
      color: Color::White,
    },
    Header {
      tag: 230,
      color: Color::White,
    },
    Header {
      tag: 231,
      color: Color::White,
    },
    Header {
      tag: 232,
      color: Color::White,
    },
    Header {
      tag: 233,
      color: Color::White,
    },
    Header {
      tag: 234,
      color: Color::White,
    },
    Header {
      tag: 235,
      color: Color::White,
    },
    Header {
      tag: 236,
      color: Color::White,
    },
    Header {
      tag: 237,
      color: Color::White,
    },
    Header {
      tag: 238,
      color: Color::White,
    },
    Header {
      tag: 239,
      color: Color::White,
    },
    Header {
      tag: 240,
      color: Color::White,
    },
    Header {
      tag: 241,
      color: Color::White,
    },
    Header {
      tag: 242,
      color: Color::White,
    },
    Header {
      tag: 243,
      color: Color::White,
    },
    Header {
      tag: 244,
      color: Color::White,
    },
    Header {
      tag: 245,
      color: Color::White,
    },
    Header {
      tag: 246,
      color: Color::White,
    },
    Header {
      tag: 247,
      color: Color::White,
    },
    Header {
      tag: 248,
      color: Color::White,
    },
    Header {
      tag: 249,
      color: Color::White,
    },
    Header {
      tag: 250,
      color: Color::White,
    },
    Header {
      tag: 251,
      color: Color::White,
    },
    Header {
      tag: 252,
      color: Color::White,
    },
    Header {
      tag: 253,
      color: Color::White,
    },
    Header {
      tag: 254,
      color: Color::White,
    },
    Header {
      tag: 255,
      color: Color::White,
    },
  ]
};
