!
!     module m_psf_case_table
!
      module m_psf_case_table
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
      integer (kind=kint) :: sf_config_list(0:255)
      integer (kind=kint) :: sf_config_order(0:255)
!
      integer(kind = kint) :: max_patch
      integer(kind = kint) :: num_patch(0:255)
      integer(kind = kint) :: iedge_4_patch(5,0:255,3)
!
!      integer (kind=kint), parameter  :: sf_config_list(0:255)         &
!     &     = (/ 0,  1,  1,  2,  1,  3,  2,  5,  1,  2,                 &
!     &          3,  5,  2,  5,  5,  8,  1,  2,  3,  5,                 &
!     &          4,  6,  6, 14,  3,  5,  7,  9,  6, 11,                 &
!     &         12, -5,  1,  3,  2,  5,  3,  7,  5,  9,                 &
!     &          4,  6,  6, 11,  6, 12, 14, -5,  2,  5,                 &
!     &          5,  8,  6, 12, 11, -5,  6, 14, 12, -5,                 &
!     &         10, -6, -6, -2,  1,  4,  3,  6,  2,  6,                 &
!     &          5, 11,  3,  6,  7, 12,  5, 14,  9, -5,                 &
!     &          3,  6,  7, 12,  6, 10, 12, -6,  7, 12,                 &
!     &         13, -7, 12, -6, -7, -3,  2,  6,  5, 14,                 &
!     &          5, 12,  8, -5,  6, 10, 12, -6, 11, -6,                 &
!     &         -5, -2,  5, 11,  9, -5, 14, -6, -5, -2,                 &
!     &         12, -6, -7, -3, -6, -4, -3, -1,  1,  3,                 &
!     &          4,  6,  3,  7,  6, 12,  2,  5,  6, 14,                 &
!     &          5,  9, 11, -5,  2,  5,  6, 11,  6, 12,                 &
!     &         10, -6,  5,  8, 12, -5, 14, -5, -6, -2,                 &
!     &          3,  7,  6, 12,  7, 13, 12, -7,  6, 12,                 &
!     &         10, -6, 12, -7, -6, -3,  5,  9, 14, -5,                 &
!     &         12, -7, -6, -3, 11, -5, -6, -2, -6, -3,                 &
!     &         -4, -1,  2,  6,  6, 10,  5, 12, 14, -6,                 &
!     &          5, 11, 12, -6,  8, -5, -5, -2,  5, 14,                 &
!     &         12, -6, 11, -6, -6, -4,  9, -5, -7, -3,                 &
!     &         -5, -2, -3, -1,  5, 12, 11, -6,  9, -7,                 &
!     &         -5, -3, 14, -6, -6, -4, -5, -3, -2, -1,                 &
!     &          8, -5, -5, -2, -5, -3, -2, -1, -5, -2,                 &
!     &         -3, -1, -2, -1, -1,  0 / )
!
!
!      integer (kind=kint), parameter  :: sf_config_order(0:255)        &
!     &     = (/ 1,  1,  2,  1,  3,  5,  2, 18,  4,  4,                 &
!     &         11, 19,  3, 20, 17,  5,  5,  9,  3, 11,                 &
!     &          3,  9,  2,  1,  7,  3,  1,  1, 15,  3,                 &
!     &         17, 21,  6,  9, 10, 12,  2,  2,  7,  2,                 &
!     &          5, 16, 10,  4,  3, 20,  2, 22,  5, 10,                 &
!     &          9,  3, 17, 10,  9, 16,  5, 13,  9, 13,                 &
!     &          3,  7, 19,  7,  7,  1,  8, 13, 11, 11,                 &
!     &          8,  1,  4,  4,  3, 19, 14,  3,  3, 23,                 &
!     &         13, 21,  6, 11, 23,  5,  8, 25,  9,  3,                 &
!     &          3,  7, 14, 22,  5,  6,  6,  6,  6,  9,                 &
!     &          5,  5,  2,  1, 18,  4,  6,  8, 10, 20,                 &
!     &          4,  8, 25,  6,  6, 15,  8, 12,  2, 13,                 &
!     &         25, 14,  4, 10,  1,  2,  1,  9,  9,  1,                 &
!     &          2,  1, 10,  4, 14, 18, 13,  2, 12,  4,                 &
!     &         15,  4,  2, 25,  8,  4, 20, 13,  8,  4,                 &
!     &          2, 18,  1,  1,  1,  5, 11,  6,  6,  6,                 &
!     &          6,  5, 22, 12,  7,  1,  7,  9, 25,  2,                 &
!     &          7, 23, 15,  6, 21, 13, 23,  5,  7, 14,                 &
!     &         23,  3,  4,  4,  5,  8, 11, 11, 13,  8,                 &
!     &          1,  7,  7, 19,  7,  1, 13, 13, 10,  5,                 &
!     &         16, 11, 16, 17,  4,  9, 10,  5, 22,  6,                 &
!     &         22,  3,  8, 10, 16,  5,  9,  7,  2,  2,                 &
!     &         12, 10,  9,  6, 21, 21,  7, 15,  7,  1,                 &
!     &          3,  7,  5,  2,  9,  3, 11,  3,  9,  5,                 &
!     &          7, 17, 20,  3, 19, 11,  4,  4, 18,  2,                 &
!     &          5,  3,  1,  2,  1,  3 /) 
!
!
!
!      integer(kind = kint), parameter :: num_patch(0:14)               &
!     &     = (/0, 1, 2, 4, 2, 3, 5, 5, 2, 4, 4, 4, 4, 4, 4/)
!
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_0(2) = (/0, 255/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_1p(nkind_etype_1)                          &
!     &     = (/1, 2, 4, 8, 16, 32, 64, 128/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_1n(nkind_etype_1)                          &
!     &     = (/254, 253, 251, 247, 239, 223, 191, 127/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_2p(nkind_etype_2)                          &
!     &     = (/  3,   6,  12,   9,  48,  96,                           &
!     &         192, 144,  17,  34,  68, 136/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_2n(nkind_etype_2)                          &
!     &     = (/252, 249, 243, 246, 207, 159,                           &
!     &          63, 111, 238, 221, 187, 119/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_3p(nkind_etype_3)                          &
!     &     = (/129,  36,  18,  72,   5, 160,                           &
!     &          24,  66,  33, 132,  10,  80/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_3n(nkind_etype_3)                          &
!     &     = (/126, 219, 237, 183, 250,  95,                           &
!     &         231, 189, 222, 123, 245, 175/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_4p(nkind_etype_4) = (/ 65, 130,  20,  40/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_4n(nkind_etype_4) = (/190, 125, 235, 215/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_5p(nkind_etype_5)                          &
!     &     = (/ 152, 137,  25,  145,   100,  98,  38,  70,             &
!     &           50,  49,  19,   35,   196,  76, 140, 200,             &
!     &           14,   7,  11,   13,   224, 208, 176, 112/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_5n(nkind_etype_5)                          &
!     &     = (/ 103, 118, 230, 110,    155, 157, 217, 185,             &
!     &          205, 206, 236, 220,     59, 179, 115,  55,             &
!     &          241, 248, 244, 242,     31,  47,  79, 143/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_6p(nkind_etype_6)                          &
!     &     = (/131,  22,  44,  73,    56,  97, 194, 148,               &
!     &          21,  42,  69, 138,    67, 134,  28,  41,               &
!     &          52, 104, 193, 146,    81, 162,  84, 168/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_6n(nkind_etype_6)                          &
!     &     = (/124, 233, 211, 182,   199, 158,  61, 107,               &
!     &         234, 213, 186, 117,   188, 121, 227, 214,               &
!     &         203, 151,  62, 109,   174,  93, 171,  87/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_7p(nkind_etype_7)                          &
!     &     = (/ 26,  37,  74, 133, 161,  82, 164,  88/)
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_7n(nkind_etype_7)                          &
!     &     = (/229, 218, 181, 122,  94, 173,  91, 167/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_8(nkind_etype_8)                           &
!     &     = (/153, 102,  51, 204, 15, 240/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_9(nkind_etype_9)                           &
!     &     = (/27, 39, 78, 141, 177, 114, 228, 216/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_10(nkind_etype_10)                         &
!     &     = (/195, 150, 60, 105, 85, 170/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_11(nkind_etype_11)                         &
!     &     = (/ 71, 142, 29,  43, 184, 113,                            &
!     &         226, 212, 54, 108, 201, 147/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_12(nkind_etype_12)                         &
!     &     = (/154, 169,  89, 149,    101, 106, 166,  86,              &
!     &          58,  53,  83, 163,    197,  92, 172, 202,              &
!     &          30, 135,  75,  45,    225, 210, 180, 120/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_12(nkind_etype_13) = (/165, 90/)
!
!      integer(kind = kint), parameter                                  &
!     &   :: iflag_psf_etype_14(nkind_etype_14)                         &
!     &     = (/ 23,  46,  77, 139,    232, 209, 178, 116,              &
!     &          99, 198, 156, 57/)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_case_table
!
      sf_config_list(0:255) = 0
      sf_config_order(0:255) = 0
!
      num_patch(0:255) = 0
      iedge_4_patch(1:5,0:255,1:3) = 0
!
      end subroutine init_case_table
!
!  ---------------------------------------------------------------------
!
      end module m_psf_case_table
