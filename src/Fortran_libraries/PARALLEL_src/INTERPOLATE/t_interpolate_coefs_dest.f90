!>@file   t_interpolate_coefs_dest.f90
!!@brief  module t_interpolate_coefs_dest
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in  Aug., 2006
!
!> @brief Interpolation coefficients on target mesh
!!@verbatim
!!      subroutine alloc_itp_coef_dest(itp_dest, coef_dest)
!!      subroutine dealloc_itp_coef_stack(coef_dest)
!!      subroutine dealloc_itp_coef_dest(coef_dest)
!!
!!      subroutine check_table_in_org_2(id_file, itp_dest, coef_dest)
!!@endverbatim
!
!
      module t_interpolate_coefs_dest
!
      use m_precision
      use t_interpolate_tbl_dest
!
      implicit none
!
!
!> Structure of interpolation coefficients for target grid
      type interpolate_coefs_dest
!>   global node ID for target domain
        integer(kind = kint_gl), allocatable :: inod_gl_dest(:)
!>   local element ID to make interpolation
        integer(kind = kint), allocatable :: iele_org_4_dest(:)
!>   interpolation type ID
        integer(kind = kint), allocatable :: itype_inter_dest(:)
!>   Coordinate of target node in element coordinate
        real(kind = kreal), allocatable :: coef_inter_dest(:,:)
!
!>   end address to receive interpolated data including interpolate type
        integer(kind = kint), allocatable                               &
     &            :: istack_nod_tbl_wtype_dest(:)
      end type interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_itp_coef_stack(num_org_pe, coef_dest)
!
      integer(kind = kint), intent(in) :: num_org_pe
      type(interpolate_coefs_dest), intent(inout) :: coef_dest
!
!
      allocate(coef_dest%istack_nod_tbl_wtype_dest(0:4*num_org_pe) )
      coef_dest%istack_nod_tbl_wtype_dest = -1
!
      end subroutine alloc_itp_coef_stack
!
!-----------------------------------------------------------------------
!
      subroutine alloc_itp_coef_dest(itp_dest, coef_dest)
!
      type(interpolate_table_dest), intent(in) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: coef_dest
!
!
      allocate(coef_dest%inod_gl_dest(itp_dest%ntot_table_dest) )
      allocate(coef_dest%iele_org_4_dest(itp_dest%ntot_table_dest) )
      allocate(coef_dest%itype_inter_dest(itp_dest%ntot_table_dest) )
      allocate(coef_dest%coef_inter_dest(itp_dest%ntot_table_dest,3) )
!
      if(itp_dest%ntot_table_dest .gt. 0) then
        coef_dest%inod_gl_dest = 0
        coef_dest%iele_org_4_dest = 0
        coef_dest%itype_inter_dest = -1
        coef_dest%coef_inter_dest = 0.0d0
      end if
!
      end subroutine alloc_itp_coef_dest
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_coef_stack(coef_dest)
!
      type(interpolate_coefs_dest), intent(inout) :: coef_dest
!
!
      deallocate(coef_dest%istack_nod_tbl_wtype_dest)
!
      end subroutine dealloc_itp_coef_stack
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_coef_dest(coef_dest)
!
      type(interpolate_coefs_dest), intent(inout) :: coef_dest
!
!
      deallocate(coef_dest%inod_gl_dest, coef_dest%iele_org_4_dest)
      deallocate(coef_dest%itype_inter_dest, coef_dest%coef_inter_dest)
!
      end subroutine dealloc_itp_coef_dest
!
!-----------------------------------------------------------------------
!
      subroutine check_table_in_org_2(id_file, itp_dest, coef_dest)
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: coef_dest
!
      integer(kind = kint) :: inod
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#  number of domain of target'
      write(id_file,'(a)') '#   domain IDs'
      write(id_file,'(a)') '#'
!
      write(id_file,'(i16)') itp_dest%num_org_domain
      write(id_file,'(10i16)')                                          &
     &       itp_dest%id_org_domain(1:itp_dest%num_org_domain)
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)')                                              &
     &                 '#  node, domain for original, belonged element'
      write(id_file,'(a)') '#   coefficients'
      write(id_file,'(a)') '#'
!
      write(id_file,'(10i16)')                                          &
     &        itp_dest%istack_nod_tbl_dest(1:itp_dest%num_org_domain)
      do inod = 1, itp_dest%ntot_table_dest
        write(id_file,'(2i16,1p3E25.15e3)')                             &
     &        itp_dest%inod_dest_4_dest(inod),                          &
     &        coef_dest%iele_org_4_dest(inod),                          &
     &        coef_dest%coef_inter_dest(inod,1:3)
      end do
!
      end subroutine check_table_in_org_2
!
!-----------------------------------------------------------------------
!
      end module t_interpolate_coefs_dest
