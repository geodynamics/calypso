!
!     module set_psf_case_table
!
!      Written by H. Matsui on june, 2006
!
!      subroutine set_sectioning_case_table
!
      module set_psf_case_table
!
      use m_precision
!
      use m_geometry_constants
      use m_psf_case_table
!
      implicit none
!
      private :: set_psf_table_4_each_case, set_psf_table_4_each_case_0
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_sectioning_case_table
!
      use m_control_data_sections
      use m_mpi_flags_4_section
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
!      integer(kind = kint) :: imark
!
      if ( (num_psf_ctl+num_iso_ctl) .le. 0) return
!
      call allocate_flags_4_comm_psf
!
      call allocate_psf_case_table
!
!      imark =  0
      call set_psf_table_4_each_case_0(nkind_etype_0,  num_patch_0,     &
     &    iflag_psf_etype_0)
!
!      imark =  1
      call set_psf_table_4_each_case(nkind_etype_1,  num_patch_1,       &
     &    iflag_psf_etype_1p,  iedge_4_patch_1p)
!      imark = -1
      call set_psf_table_4_each_case(nkind_etype_1,  num_patch_1,       &
     &    iflag_psf_etype_1n,  iedge_4_patch_1n)
!
!      imark =  2
      call set_psf_table_4_each_case(nkind_etype_2,  num_patch_2,       &
     &    iflag_psf_etype_2p,  iedge_4_patch_2p)
!      imark = -2
      call set_psf_table_4_each_case(nkind_etype_2,  num_patch_2,       &
     &    iflag_psf_etype_2n,  iedge_4_patch_2n)
!
!      imark =  3
      call set_psf_table_4_each_case(nkind_etype_3,  num_patch_3,       &
     &    iflag_psf_etype_3p,  iedge_4_patch_3p)
!      imark = -3
      call set_psf_table_4_each_case(nkind_etype_3,  num_patch_3,       &
     &    iflag_psf_etype_3n,  iedge_4_patch_3n)
!
!      imark =  4
      call set_psf_table_4_each_case(nkind_etype_4,  num_patch_4,       &
     &    iflag_psf_etype_4p,  iedge_4_patch_4p)
!      imark = -4
      call set_psf_table_4_each_case(nkind_etype_4,  num_patch_4,       &
     &    iflag_psf_etype_4n,  iedge_4_patch_4n)
!
!      imark =  5
      call set_psf_table_4_each_case(nkind_etype_5,  num_patch_5,       &
     &    iflag_psf_etype_5p,  iedge_4_patch_5p)
!      imark = -5
      call set_psf_table_4_each_case(nkind_etype_5,  num_patch_5,       &
     &    iflag_psf_etype_5n,  iedge_4_patch_5n)
!
!      imark =  6
      call set_psf_table_4_each_case(nkind_etype_6,  num_patch_6,       &
     &    iflag_psf_etype_6p,  iedge_4_patch_6p)
!      imark = -6
      call set_psf_table_4_each_case(nkind_etype_6,  num_patch_6,       &
     &    iflag_psf_etype_6n,  iedge_4_patch_6n)
!
!      imark =  7
      call set_psf_table_4_each_case(nkind_etype_7,  num_patch_7,       &
     &    iflag_psf_etype_7p,  iedge_4_patch_7p)
!      imark = -7
      call set_psf_table_4_each_case(nkind_etype_7,  num_patch_7,       &
     &    iflag_psf_etype_7n,  iedge_4_patch_7n)
!
!
!      imark =  8
      call set_psf_table_4_each_case(nkind_etype_8,  num_patch_8,       &
     &    iflag_psf_etype_8,  iedge_4_patch_8)
!
!      imark =  9
      call set_psf_table_4_each_case(nkind_etype_9,  num_patch_9,       &
     &    iflag_psf_etype_9,  iedge_4_patch_9)
!
!      imark = 10
      call set_psf_table_4_each_case(nkind_etype_10, num_patch_10,      &
     &    iflag_psf_etype_10, iedge_4_patch_10)
!
!      imark = 11
      call set_psf_table_4_each_case(nkind_etype_11, num_patch_11,      &
     &    iflag_psf_etype_11, iedge_4_patch_11)
!
!      imark = 12
      call set_psf_table_4_each_case(nkind_etype_12, num_patch_12,      &
     &    iflag_psf_etype_12, iedge_4_patch_12)
!
!      imark = 13
      call set_psf_table_4_each_case(nkind_etype_13, num_patch_13,      &
     &    iflag_psf_etype_13, iedge_4_patch_13)
!
!      imark = 14
      call set_psf_table_4_each_case(nkind_etype_14, num_patch_14,      &
     &    iflag_psf_etype_14, iedge_4_patch_14)
!
      end subroutine set_sectioning_case_table
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_table_4_each_case(nkind_etype, num_patch_n,    &
     &          iflag_psf_etype_x, iedge_4_patch_x)
!
      integer(kind=kint), intent(in) :: nkind_etype, num_patch_n
      integer(kind=kint), intent(in) :: iflag_psf_etype_x(nkind_etype)
      integer(kind=kint), intent(in)                                    &
     &        :: iedge_4_patch_x(num_triangle,num_patch_n,nkind_etype)
!
      integer(kind = kint) :: i, id, j ,k
!
      do i = 1, nkind_etype
        id = iflag_psf_etype_x(i)
        psf_case_tbl(id)%npatch = num_patch_n
!
        call alloc_psf_case_tbl_item(id)
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
     &          iflag_psf_etype_x)
!
      integer(kind=kint), intent(in) :: nkind_etype, num_patch_0
      integer(kind=kint), intent(in) :: iflag_psf_etype_x(nkind_etype)
!
      integer(kind = kint) :: i, id
!
      do i = 1, nkind_etype
        id = iflag_psf_etype_x(i)
        psf_case_tbl(id)%npatch = num_patch_0
!
        call alloc_psf_case_tbl_item(id)
      end do
!
      end subroutine set_psf_table_4_each_case_0
!
!  ---------------------------------------------------------------------
!
      end module set_psf_case_table
