!>@file  t_psf_case_table.f90
!!       module t_psf_case_table
!!
!! @author H. Matsui
!! @date   Programmed on June, 2006
!
!> @brief List of local patch case table
!!
!!@verbatim
!!      subroutine init_psf_case_tables(case_tbls)
!!      subroutine dealloc_psf_case_table(case_tbls)
!!
!!      integer(kind = kint), parameter :: num_patch(0:14)              &
!!     &     = (/0, 1, 2, 4, 2, 3, 5, 5, 2, 4, 4, 4, 4, 4, 4/)
!!
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_0(2) = (/0, 255/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_1p(nkind_etype_1)                         &
!!     &     = (/1, 2, 4, 8, 16, 32, 64, 128/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_1n(nkind_etype_1)                         &
!!     &     = (/254, 253, 251, 247, 239, 223, 191, 127/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_2p(nkind_etype_2)                         &
!!     &     = (/  3,   6,  12,   9,  48,  96,                          &
!!     &         192, 144,  17,  34,  68, 136/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_2n(nkind_etype_2)                         &
!!     &     = (/252, 249, 243, 246, 207, 159,                          &
!!     &          63, 111, 238, 221, 187, 119/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_3p(nkind_etype_3)                         &
!!     &     = (/129,  36,  18,  72,   5, 160,                          &
!!     &          24,  66,  33, 132,  10,  80/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_3n(nkind_etype_3)                         &
!!     &     = (/126, 219, 237, 183, 250,  95,                          &
!!     &         231, 189, 222, 123, 245, 175/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_4p(nkind_etype_4) = (/ 65, 130,  20,  40/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_4n(nkind_etype_4) = (/190, 125, 235, 215/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_5p(nkind_etype_5)                         &
!!     &     = (/ 152, 137,  25,  145,   100,  98,  38,  70,            &
!!     &           50,  49,  19,   35,   196,  76, 140, 200,            &
!!     &           14,   7,  11,   13,   224, 208, 176, 112/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_5n(nkind_etype_5)                         &
!!     &     = (/ 103, 118, 230, 110,    155, 157, 217, 185,            &
!!     &          205, 206, 236, 220,     59, 179, 115,  55,            &
!!     &          241, 248, 244, 242,     31,  47,  79, 143/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_6p(nkind_etype_6)                         &
!!     &     = (/131,  22,  44,  73,    56,  97, 194, 148,              &
!!     &          21,  42,  69, 138,    67, 134,  28,  41,              &
!!     &          52, 104, 193, 146,    81, 162,  84, 168/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_6n(nkind_etype_6)                         &
!!     &     = (/124, 233, 211, 182,   199, 158,  61, 107,              &
!!     &         234, 213, 186, 117,   188, 121, 227, 214,              &
!!     &         203, 151,  62, 109,   174,  93, 171,  87/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_7p(nkind_etype_7)                         &
!!     &     = (/ 26,  37,  74, 133, 161,  82, 164,  88/)
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_7n(nkind_etype_7)                         &
!!     &     = (/229, 218, 181, 122,  94, 173,  91, 167/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_8(nkind_etype_8)                          &
!!     &     = (/153, 102,  51, 204, 15, 240/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_9(nkind_etype_9)                          &
!!     &     = (/27, 39, 78, 141, 177, 114, 228, 216/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_10(nkind_etype_10)                        &
!!     &     = (/195, 150, 60, 105, 85, 170/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_11(nkind_etype_11)                        &
!!     &     = (/ 71, 142, 29,  43, 184, 113,                           &
!!     &         226, 212, 54, 108, 201, 147/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_12(nkind_etype_12)                        &
!!     &     = (/154, 169,  89, 149,    101, 106, 166,  86,             &
!!     &          58,  53,  83, 163,    197,  92, 172, 202,             &
!!     &          30, 135,  75,  45,    225, 210, 180, 120/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_12(nkind_etype_13) = (/165, 90/)
!!
!!      integer(kind = kint), parameter                                 &
!!     &   :: iflag_psf_etype_14(nkind_etype_14)                        &
!!     &     = (/ 23,  46,  77, 139,    232, 209, 178, 116,             &
!!     &          99, 198, 156, 57/)
!!@endverbatim
!!
      module t_psf_case_table
!
      use m_precision
!
      implicit none
!
!>      Structure of triangle patch list for each section case
      type psf_each_case
!>        Number of patches in one element
        integer(kind = kint) :: npatch
!>        Local patch list in one element
        integer(kind = kint), allocatable :: iedge(:,:)
      end type psf_each_case
!
!
!>      Structure of triangle patch list for each section case
      type psf_cases
!>        Number of sectioning case (256-1)
        integer(kind = kint) :: num_case_tbl
!>        Structure for triangle patch list
        type(psf_each_case), allocatable :: psf_case_tbl(:)
      end type psf_cases
!
      private :: alloc_psf_case_tables, alloc_psf_case_tbl_item
      private :: set_psf_table_4_each_case, set_psf_table_4_each_case_0
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_psf_case_tables(case_tbls)
!
      use m_intersection_data_0
      use m_intersection_data_1
      use m_intersection_data_2
      use m_intersection_data_3
      use m_intersection_data_4
      use m_intersection_data_5
      use m_intersection_data_6
      use m_intersection_data_7
      use m_intersection_data_8
      use m_intersection_data_9
      use m_intersection_data_10
      use m_intersection_data_11
      use m_intersection_data_12
      use m_intersection_data_13
      use m_intersection_data_14
!
      type(psf_cases), intent(inout) :: case_tbls
!
!
      call alloc_psf_case_tables(case_tbls)
!
!      imark =  0
      call set_psf_table_4_each_case_0                                  &
     &   (nkind_etype_0,  num_patch_0, iflag_psf_etype_0,               &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  1
      call set_psf_table_4_each_case(nkind_etype_1,  num_patch_1,       &
     &    iflag_psf_etype_1p,  iedge_4_patch_1p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -1
      call set_psf_table_4_each_case(nkind_etype_1,  num_patch_1,       &
     &    iflag_psf_etype_1n,  iedge_4_patch_1n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  2
      call set_psf_table_4_each_case(nkind_etype_2,  num_patch_2,       &
     &    iflag_psf_etype_2p,  iedge_4_patch_2p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -2
      call set_psf_table_4_each_case(nkind_etype_2,  num_patch_2,       &
     &    iflag_psf_etype_2n,  iedge_4_patch_2n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  3
      call set_psf_table_4_each_case(nkind_etype_3,  num_patch_3,       &
     &    iflag_psf_etype_3p,  iedge_4_patch_3p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -3
      call set_psf_table_4_each_case(nkind_etype_3,  num_patch_3,       &
     &    iflag_psf_etype_3n,  iedge_4_patch_3n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  4
      call set_psf_table_4_each_case(nkind_etype_4,  num_patch_4,       &
     &    iflag_psf_etype_4p,  iedge_4_patch_4p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -4
      call set_psf_table_4_each_case(nkind_etype_4,  num_patch_4,       &
     &    iflag_psf_etype_4n,  iedge_4_patch_4n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  5
      call set_psf_table_4_each_case(nkind_etype_5,  num_patch_5,       &
     &    iflag_psf_etype_5p,  iedge_4_patch_5p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -5
      call set_psf_table_4_each_case(nkind_etype_5,  num_patch_5,       &
     &    iflag_psf_etype_5n,  iedge_4_patch_5n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  6
      call set_psf_table_4_each_case(nkind_etype_6,  num_patch_6,       &
     &    iflag_psf_etype_6p,  iedge_4_patch_6p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -6
      call set_psf_table_4_each_case(nkind_etype_6,  num_patch_6,       &
     &    iflag_psf_etype_6n,  iedge_4_patch_6n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  7
      call set_psf_table_4_each_case(nkind_etype_7,  num_patch_7,       &
     &    iflag_psf_etype_7p,  iedge_4_patch_7p,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!      imark = -7
      call set_psf_table_4_each_case(nkind_etype_7,  num_patch_7,       &
     &    iflag_psf_etype_7n,  iedge_4_patch_7n,                        &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!
!      imark =  8
      call set_psf_table_4_each_case(nkind_etype_8,  num_patch_8,       &
     &    iflag_psf_etype_8,  iedge_4_patch_8,                          &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark =  9
      call set_psf_table_4_each_case(nkind_etype_9,  num_patch_9,       &
     &    iflag_psf_etype_9,  iedge_4_patch_9,                          &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark = 10
      call set_psf_table_4_each_case(nkind_etype_10, num_patch_10,      &
     &    iflag_psf_etype_10, iedge_4_patch_10,                         &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark = 11
      call set_psf_table_4_each_case(nkind_etype_11, num_patch_11,      &
     &    iflag_psf_etype_11, iedge_4_patch_11,                         &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark = 12
      call set_psf_table_4_each_case(nkind_etype_12, num_patch_12,      &
     &    iflag_psf_etype_12, iedge_4_patch_12,                         &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark = 13
      call set_psf_table_4_each_case(nkind_etype_13, num_patch_13,      &
     &    iflag_psf_etype_13, iedge_4_patch_13,                         &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
!      imark = 14
      call set_psf_table_4_each_case(nkind_etype_14, num_patch_14,      &
     &    iflag_psf_etype_14, iedge_4_patch_14,                         &
     &    case_tbls%num_case_tbl, case_tbls%psf_case_tbl)
!
      end subroutine init_psf_case_tables
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_case_table(case_tbls)
!
      type(psf_cases), intent(inout) :: case_tbls
      integer(kind = kint) :: i
!
      do i = 0, case_tbls%num_case_tbl
        deallocate(case_tbls%psf_case_tbl(i)%iedge)
      end do
      deallocate(case_tbls%psf_case_tbl)
!
      end subroutine dealloc_psf_case_table
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_table_4_each_case                              &
     &         (nkind_etype, num_patch_n, iflag_psf_etype_x,            &
     &          iedge_4_patch_x, num_case_tbl, psf_case_tbl)
!
      use m_geometry_constants
!
      integer(kind=kint), intent(in) :: nkind_etype, num_patch_n
      integer(kind=kint), intent(in) :: iflag_psf_etype_x(nkind_etype)
      integer(kind=kint), intent(in)                                    &
     &        :: iedge_4_patch_x(num_triangle,num_patch_n,nkind_etype)
      integer(kind=kint), intent(in) :: num_case_tbl
!
      type(psf_each_case), intent(inout)                                &
     &                     :: psf_case_tbl(0:num_case_tbl)
!
      integer(kind = kint) :: i, id, j ,k
!
      do i = 1, nkind_etype
        id = iflag_psf_etype_x(i)
        psf_case_tbl(id)%npatch = num_patch_n
!
        call alloc_psf_case_tbl_item(psf_case_tbl(id))
!
        do j = 1, num_patch_n
          do k = 1, num_triangle
            psf_case_tbl(id)%iedge(j,k) = iedge_4_patch_x(k,j,i)
          end do
        end do
      end do
!
      end subroutine set_psf_table_4_each_case
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_table_4_each_case_0(nkind_etype, num_patch_0,  &
     &          iflag_psf_etype_x, num_case_tbl, psf_case_tbl)
!
      integer(kind=kint), intent(in) :: nkind_etype, num_patch_0
      integer(kind=kint), intent(in) :: iflag_psf_etype_x(nkind_etype)
!
      integer(kind=kint), intent(in) :: num_case_tbl
!
      type(psf_each_case), intent(inout)                                &
     &                     :: psf_case_tbl(0:num_case_tbl)
!
!
      integer(kind = kint) :: i, id
!
      do i = 1, nkind_etype
        id = iflag_psf_etype_x(i)
        psf_case_tbl(id)%npatch = num_patch_0
!
        call alloc_psf_case_tbl_item(psf_case_tbl(id))
      end do
!
      end subroutine set_psf_table_4_each_case_0
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_case_tables(case_tbls)
!
      type(psf_cases), intent(inout) :: case_tbls
!
!
      case_tbls%num_case_tbl = 256 - 1
      allocate(case_tbls%psf_case_tbl(0:case_tbls%num_case_tbl))
!
      end subroutine alloc_psf_case_tables
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_case_tbl_item(psf_case_tbl)
!
      type(psf_each_case), intent(inout) :: psf_case_tbl
!
!
      allocate(psf_case_tbl%iedge(psf_case_tbl%npatch,3))
      if(psf_case_tbl%npatch .gt.0) psf_case_tbl%iedge = 0
!
      end subroutine alloc_psf_case_tbl_item
!
!  ---------------------------------------------------------------------
!
      end module t_psf_case_table
