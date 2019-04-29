!> @file  t_field_data_IO.f90
!!      module t_field_data_IO
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for field data IO
!!
!!@verbatim
!!      subroutine alloc_multi_field_data_IO(nloop, mul_f)
!!      subroutine dealloc_multi_field_data_IO(mul_f)
!!        type(multi_field_IO), intent(inout) :: mul_f
!!
!!      subroutine alloc_phys_name_IO(fld_IO)
!!      subroutine alloc_phys_data_IO(fld_IO)
!!      subroutine alloc_merged_field_stack(num_pe, fld_IO)
!!
!!      subroutine dealloc_phys_IO(fld_IO)
!!      subroutine dealloc_phys_name_IO(fld_IO)
!!      subroutine dealloc_phys_data_IO(fld_IO)
!!      subroutine dealloc_merged_field_stack(fld_IO)
!!
!!      subroutine cal_istack_phys_comp_IO(fld_IO)
!!
!!      subroutine check_field_name_4_IO(id_field, fld_IO)
!!@endverbatim
!!
!
      module t_field_data_IO
!
      use m_precision
!
      implicit none
!
!
!>      file ID for field data IO
      integer(kind = kint), parameter :: id_phys_file = 15
!
!>      Structure for field data IO
      type field_IO
!>        file header for field data
!        character(len=kchara) :: file_prefix = "rst"
!>        file format flag for field data
!        integer(kind = kint) :: iflag_file_fmt =  0
!>        number of field for IO (num_phys_data_IO)
        integer(kind = kint) :: num_field_IO
!>        total number of component for IO (ntot_phys_data_IO)
        integer(kind = kint) :: ntot_comp_IO
!>        number of component for each field (num_comp_IO)
        integer(kind = kint), allocatable :: num_comp_IO(:)
!>        end address of component for each field (istack_phys_comp_IO)
        integer(kind = kint), allocatable :: istack_comp_IO(:)
!
!>        field name (phys_data_name_IO)
        character(len=kchara), allocatable :: fld_name(:)
!
!>        number of data points (numgrid_phys_IO)
        integer(kind = kint) :: nnod_IO
!
!>        field data for IO  (d_IO(:,:))
        real(kind = kreal), allocatable :: d_IO(:,:)
!
!>        end point for number of node for each subdomain
        integer(kind = kint_gl), allocatable :: istack_numnod_IO(:)
      end type field_IO
!
!
!>      Structure for field data IO for multiple domains
      type multi_field_IO
!>        Number of subdomains in each process
        integer(kind = kint) :: nloop_IO
!>        Structure for field data IO
        type(field_IO), allocatable :: fld_IO(:)
      end type multi_field_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_multi_field_data_IO(nloop, mul_f)
!
      integer(kind = kint), intent(in) :: nloop
      type(multi_field_IO), intent(inout) :: mul_f
!
      mul_f%nloop_IO = nloop
      allocate(mul_f%fld_IO(mul_f%nloop_IO))
!
      end subroutine alloc_multi_field_data_IO
!
! -------------------------------------------------------------------
!
      subroutine dealloc_multi_field_data_IO(mul_f)
!
      type(multi_field_IO), intent(inout) :: mul_f
!
      deallocate(mul_f%fld_IO)
!
      end subroutine dealloc_multi_field_data_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine alloc_phys_name_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      allocate( fld_IO%num_comp_IO(fld_IO%num_field_IO) )
      allocate( fld_IO%istack_comp_IO(0:fld_IO%num_field_IO) )
      allocate( fld_IO%fld_name(fld_IO%num_field_IO) )
      if(fld_IO%num_field_IO .gt. 0) fld_IO%num_comp_IO = 0
      fld_IO%istack_comp_IO = -1
!
      end subroutine alloc_phys_name_IO
!
! -------------------------------------------------------------------
!
      subroutine alloc_phys_data_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      allocate( fld_IO%d_IO(fld_IO%nnod_IO, fld_IO%ntot_comp_IO) )
      if(fld_IO%nnod_IO .gt. 0) fld_IO%d_IO = 0.0d0
!
      end subroutine alloc_phys_data_IO
!
! -------------------------------------------------------------------
!
      subroutine alloc_merged_field_stack(num_pe, fld_IO)
!
      integer, intent(in) :: num_pe
      type(field_IO), intent(inout) :: fld_IO
!
!
      allocate(fld_IO%istack_numnod_IO(0:num_pe))
      fld_IO%istack_numnod_IO = 0
!
      end subroutine alloc_merged_field_stack
!
! -----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call dealloc_merged_field_stack(fld_IO)
      call dealloc_phys_data_IO(fld_IO)
      call dealloc_phys_name_IO(fld_IO)
!
      end subroutine dealloc_phys_IO
!
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_name_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(allocated(fld_IO%num_comp_IO)) then 
        deallocate( fld_IO%num_comp_IO )
        deallocate( fld_IO%istack_comp_IO )
        deallocate( fld_IO%fld_name )
      end if
!
      end subroutine dealloc_phys_name_IO
!
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_data_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(allocated(fld_IO%d_IO)) deallocate(fld_IO%d_IO)
!
      end subroutine dealloc_phys_data_IO
!
! -------------------------------------------------------------------
!
      subroutine dealloc_merged_field_stack(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      if(allocated(fld_IO%istack_numnod_IO))                            &
     &                       deallocate(fld_IO%istack_numnod_IO)
!
      end subroutine dealloc_merged_field_stack
!
! -----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_istack_phys_comp_IO(fld_IO)
!
      use m_constants
      use cal_minmax_and_stacks
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call s_cal_total_and_stacks                                       &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO, izero,               &
     &    fld_IO%istack_comp_IO, fld_IO%ntot_comp_IO)
!
      end subroutine cal_istack_phys_comp_IO
!
! -------------------------------------------------------------------
!
      subroutine check_field_name_4_IO(id_field, fld_IO)
!
      integer(kind = kint), intent(in) :: id_field
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: k
!
!
      write(id_field,*) 'fld_IO%num_field_IO', fld_IO%num_field_IO
      write(id_field,*) 'fld_IO%num_comp_IO', fld_IO%num_comp_IO
      write(id_field,*) 'fld_IO%fld_name'
      do k = 1, fld_IO%num_field_IO
        write(id_field,*) k, trim(fld_IO%fld_name(k))
      end do
!
      end subroutine check_field_name_4_IO
!
! -----------------------------------------------------------------------
!
      end module t_field_data_IO
