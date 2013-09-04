!field_IO_select.F90
!      module field_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_read_step_FEM_field_file(my_rank, istep_fld)
!      subroutine sel_read_step_SPH_field_file(my_rank, istep_fld)
!
!      subroutine sel_write_step_FEM_field_file(my_rank, istep_fld)
!      subroutine sel_write_step_SPH_field_file(my_rank, istep_fld)
!
!      subroutine check_step_FEM_field_file(my_rank, istep_fld, ierr)
!      subroutine sel_read_alloc_step_FEM_file(my_rank, istep_fld)
!      subroutine sel_read_alloc_step_SPH_file(my_rank, istep_fld)
!
!      subroutine sel_read_alloc_FEM_fld_head(my_rank, istep_fld)
!      subroutine sel_read_alloc_SPH_fld_head(my_rank, istep_fld)
!
      module field_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use m_field_data_IO
      use field_file_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine check_step_FEM_field_file(my_rank, istep_fld, ierr)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      integer(kind=kint), intent(inout) :: ierr
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      open (id_phys_file, file=file_name, status='old', err=99)
      close(id_phys_file)
!
      ierr = 0
      return
!
  99  continue
      ierr = 1
      return
!
      end subroutine check_step_FEM_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_FEM_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call write_step_field_file(file_name, my_rank)
!
      end subroutine sel_write_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_SPH_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call write_step_field_file(file_name, my_rank)
!
      end subroutine sel_write_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_FEM_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_FEM_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_and_allocate_step_field(file_name, my_rank)
!
      end subroutine sel_read_alloc_step_FEM_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_and_allocate_step_field(file_name, my_rank)
!
      end subroutine sel_read_alloc_step_SPH_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_FEM_fld_head(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_and_allocate_step_head(file_name, my_rank)
!
      end subroutine sel_read_alloc_FEM_fld_head
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld_head(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call read_and_allocate_step_head(file_name, my_rank)
!
      end subroutine sel_read_alloc_SPH_fld_head
!
!------------------------------------------------------------------
!
      end module field_IO_select
