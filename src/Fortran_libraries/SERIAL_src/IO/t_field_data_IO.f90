!> @file  t_field_data_IO.f90
!!      module t_field_data_IO
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for field data IO
!!
!!@verbatim
!!      subroutine copy_rst_prefix_and_fmt(file_head, i_format, fld_IO)
!!
!!      subroutine alloc_phys_name_IO(fld_IO)
!!      subroutine alloc_phys_data_IO(fld_IO)
!!      subroutine alloc_merged_field_stack(nprocs, fld_IO)
!!
!!      subroutine dealloc_phys_IO(fld_IO)
!!      subroutine dealloc_phys_name_IO(fld_IO)
!!      subroutine dealloc_phys_data_IO(fld_IO)
!!      subroutine dealloc_merged_field_stack(fld_IO)
!!
!!      subroutine set_field_file_fmt_prefix                            &
!!     &         (iflag_fmt, file_head, fld_IO)
!!      subroutine cal_istack_phys_comp_IO(fld_IO)
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
        character(len=kchara) :: file_prefix = "rst"
!>        file format flag for field data
        integer(kind = kint) :: iflag_file_fmt =  0
!>        number of field for IO (num_phys_data_IO)
        integer(kind = kint) :: num_field_IO
!>        total number of component for IO (ntot_phys_data_IO)
        integer(kind = kint) :: ntot_comp_IO
!>        number of component for each field (num_comp_IO)
        integer(kind = kint), pointer :: num_comp_IO(:)
!>        end address of component for each field (istack_phys_comp_IO)
        integer(kind = kint), pointer :: istack_comp_IO(:)
!
!>        field name (phys_data_name_IO)
        character(len=kchara), pointer :: fld_name(:)
!
!>        number of data points (numgrid_phys_IO)
        integer(kind = kint) :: nnod_IO
!
!>        field data for IO  (d_IO(:,:))
        real(kind = kreal), pointer :: d_IO(:,:)
!
!>        end point for number of node for each subdomain
        integer(kind = kint_gl), pointer :: istack_numnod_IO(:)
      end type field_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rst_prefix_and_fmt(file_head, i_format, fld_IO)
!
      character(len=kchara), intent(in) :: file_head
      integer(kind=kint), intent(in)  :: i_format
      type(field_IO), intent(inout) :: fld_IO
!
      fld_IO%file_prefix    = file_head
      fld_IO%iflag_file_fmt = i_format
!
      end subroutine copy_rst_prefix_and_fmt
!
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
      subroutine alloc_merged_field_stack(nprocs, fld_IO)
!
      integer(kind = kint), intent(in) :: nprocs
      type(field_IO), intent(inout) :: fld_IO
!
!
      allocate(fld_IO%istack_numnod_IO(0:nprocs))
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
      deallocate( fld_IO%num_comp_IO )
      deallocate( fld_IO%istack_comp_IO )
      deallocate( fld_IO%fld_name )
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
      deallocate( fld_IO%d_IO )
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
      deallocate(fld_IO%istack_numnod_IO)
!
      end subroutine dealloc_merged_field_stack
!
! -----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_field_file_fmt_prefix                              &
     &         (iflag_fmt, file_head, fld_IO)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
      type(field_IO), intent(inout) :: fld_IO
!
      fld_IO%iflag_file_fmt = iflag_fmt
      write(fld_IO%file_prefix,'(a)') trim(file_head)
!
      end subroutine set_field_file_fmt_prefix
!
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
      end module t_field_data_IO
