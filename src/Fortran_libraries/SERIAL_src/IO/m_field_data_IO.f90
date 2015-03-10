!> @file  m_field_data_IO.f90
!!      module m_field_data_IO
!!
!! @author  H. Matsui
!! @date Programmed in June, 2005
!! @n    Modified in Oct., 2007
!
!> @brief Data for field data IO
!!
!!@verbatim
!!      subroutine allocate_phys_data_name_IO
!!      subroutine allocate_phys_data_IO
!!
!!      subroutine deallocate_phys_data_name_IO
!!      subroutine deallocate_phys_data_IO
!!
!!      subroutine set_field_file_fmt_prefix(iflag_fmt, file_head)
!!      subroutine cal_istack_phys_comp_IO
!!@endverbatim
!!
!
      module m_field_data_IO
!
      use m_precision
!
      implicit none
!
!
!>      file ID for field data IO
      integer(kind = kint), parameter :: id_phys_file = 15
!
!>      file header for field data
      character(len=kchara) :: phys_file_head = "rst"
      integer(kind = kint) :: iflag_field_data_fmt =  0
!
!>      number of field for IO
      integer(kind = kint) :: num_phys_data_IO
!>      total number of component for IO
      integer(kind = kint) :: ntot_phys_data_IO
!>      number of component for each field
      integer(kind = kint), allocatable :: num_phys_comp_IO(:)
!>      end address of component for each field
      integer(kind = kint), allocatable :: istack_phys_comp_IO(:)
!
!>      field name
      character(len=kchara), allocatable :: phys_data_name_IO(:)
!
!>      number of data points
      integer(kind = kint) :: numgrid_phys_IO
!
!>      field data for IO
      real(kind = kreal), allocatable :: phys_data_IO(:,:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_data_name_IO
!
      allocate( num_phys_comp_IO(num_phys_data_IO) )
      allocate( istack_phys_comp_IO(0:num_phys_data_IO) )
      allocate( phys_data_name_IO(num_phys_data_IO) )
      if(num_phys_data_IO .gt. 0) num_phys_comp_IO = 0
      istack_phys_comp_IO = -1
!
      end subroutine allocate_phys_data_name_IO
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_data_IO
!
      allocate( phys_data_IO(numgrid_phys_IO, ntot_phys_data_IO) )
      if(numgrid_phys_IO .gt. 0) phys_data_IO = 0.0d0
!
      end subroutine allocate_phys_data_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine deallocate_phys_data_name_IO
!
      deallocate( num_phys_comp_IO )
      deallocate( istack_phys_comp_IO )
      deallocate( phys_data_name_IO )
!
      end subroutine deallocate_phys_data_name_IO
!
! -------------------------------------------------------------------
!
      subroutine deallocate_phys_data_IO
!
      deallocate( phys_data_IO )
!
      end subroutine deallocate_phys_data_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_field_file_fmt_prefix(iflag_fmt, file_head)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
!
      iflag_field_data_fmt = iflag_fmt
      write(phys_file_head,'(a)') trim(file_head)
!
      end subroutine set_field_file_fmt_prefix
!
! -------------------------------------------------------------------
!
      subroutine cal_istack_phys_comp_IO
!
      use m_constants
      use cal_minmax_and_stacks
!
!
      call s_cal_total_and_stacks(num_phys_data_IO, num_phys_comp_IO,   &
     &    izero, istack_phys_comp_IO, ntot_phys_data_IO)
!
      end subroutine cal_istack_phys_comp_IO
!
! -------------------------------------------------------------------
!
      end module m_field_data_IO
