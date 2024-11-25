!>@file   t_interpolate_tbl_dest.f90
!!@brief  module t_interpolate_tbl_dest
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Nov., 2008
!
!> @brief Structure of interpolation table for target mesh
!!
!!@verbatim
!!      subroutine set_num_org_domain(num_org_pe, tbl_dest)
!!      subroutine alloc_type_itp_num_dest(tbl_dest)
!!      subroutine alloc_itp_table_dest(tbl_dest)
!!      subroutine alloc_type_zero_itp_tbl_dest(tbl_dest)
!!
!!      subroutine dealloc_itp_num_dest(tbl_dest)
!!      subroutine dealloc_itp_table_dest(tbl_dest)
!!@endverbatim
!
      module t_interpolate_tbl_dest
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!> Structure of interpolation table for target grid
      type interpolate_table_dest
!
!>   number of subdomain to receive interpolated data
        integer(kind = kint) :: num_org_domain
!>   flag if original nodes have same prosess
        integer(kind = kint) :: iflag_self_itp_recv
!>   subdomain rank to receive interpolated data
        integer(kind = kint), allocatable :: id_org_domain(:)
!>   end address to receive interpolated data
        integer(kind = kint), allocatable :: istack_nod_tbl_dest(:)
!
!>   total number of interpolated node in target subdomain
        integer(kind = kint) :: ntot_table_dest
!>   local node ID to set interpolated data (import)
        integer(kind = kint), allocatable :: inod_dest_4_dest(:)
!>   Reverse ID to set interpolated data (import)
        integer(kind = kint), allocatable :: irev_dest_4_dest(:)
      end type interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_num_org_domain(num_org_pe, tbl_dest)
!
      integer(kind = kint), intent(in) :: num_org_pe
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      tbl_dest%num_org_domain = num_org_pe
!
      end subroutine set_num_org_domain
!
!-----------------------------------------------------------------------
!
      subroutine alloc_itp_num_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      allocate(tbl_dest%id_org_domain(tbl_dest%num_org_domain))
      allocate(tbl_dest%istack_nod_tbl_dest(0:tbl_dest%num_org_domain))
!
      if (tbl_dest%num_org_domain .gt. 0) tbl_dest%id_org_domain = 0
      tbl_dest%istack_nod_tbl_dest =       -1
!
      end subroutine alloc_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine alloc_itp_table_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      allocate(tbl_dest%inod_dest_4_dest(tbl_dest%ntot_table_dest))
      allocate(tbl_dest%irev_dest_4_dest(tbl_dest%ntot_table_dest))
      if (tbl_dest%ntot_table_dest .gt. 0) then
        tbl_dest%inod_dest_4_dest = 0
        tbl_dest%irev_dest_4_dest = 0
      end if
!
      end subroutine alloc_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_zero_itp_tbl_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      tbl_dest%ntot_table_dest = 0
      call set_num_org_domain(izero, tbl_dest)
      call alloc_itp_num_dest(tbl_dest)
      call alloc_itp_table_dest(tbl_dest)
!
      end subroutine alloc_type_zero_itp_tbl_dest
!
!------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_num_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      if(allocated(tbl_dest%id_org_domain) .eqv. .FALSE.) return
      deallocate( tbl_dest%id_org_domain )
      deallocate( tbl_dest%istack_nod_tbl_dest)
!
      end subroutine dealloc_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_table_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      if(allocated(tbl_dest%inod_dest_4_dest) .eqv. .FALSE.) return
      deallocate(tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest)
!
      end subroutine dealloc_itp_table_dest
!
!-----------------------------------------------------------------------
!
      end module t_interpolate_tbl_dest
