!> @file  rst_data_IO_by_fld.f90
!!      module rst_data_IO_by_fld
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief read restart file
!!
!!@verbatim
!!      subroutine read_rst_file(id_rank, file_name, t_IO, fld_IO)
!!      subroutine read_rst_data_comps(id_rank, file_name, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module rst_data_IO_by_fld
!
      use m_precision
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use time_data_IO
!
      implicit none
!
      private :: read_rst_field_comps
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_rst_file(id_rank, file_name, t_IO, fld_IO)
!
      use set_parallel_file_name
      use field_data_IO
      use skip_comment_f
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read ascii restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='formatted')
!
      call read_step_data(id_phys_file, t_IO)
!
      call skip_comment(character_4_read,id_phys_file)
      read(character_4_read,*) fld_IO%num_field_IO
!
      call read_field_data(id_phys_file, cast_long(fld_IO%nnod_IO),     &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
      close (id_phys_file)
!
      end subroutine read_rst_file
!
!------------------------------------------------------------------
!
      subroutine read_rst_data_comps(id_rank, file_name, t_IO, fld_IO)
!
      use set_parallel_file_name
      use field_data_IO
      use skip_comment_f
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='formatted')
!
      call read_step_data(id_phys_file, t_IO)
!
      call skip_comment(character_4_read,id_phys_file)
      read(character_4_read,*) fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
!
      call read_rst_field_comps(fld_IO)
      close (id_phys_file)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_rst_data_comps
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rst_field_comps(fld_IO)
!
      use skip_comment_f
      use set_restart_data
      use skip_comment_f
!
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: character_4_read
      integer(kind = kint) :: i, inod
      real(kind = kreal) :: rtmp
!
!
      do i = 1, fld_IO%num_field_IO
        call skip_comment(character_4_read,id_phys_file)
        read(character_4_read,*) fld_IO%fld_name(i)
!
        call set_num_comps_4_rst(fld_IO%fld_name(i),                    &
     &      fld_IO%num_comp_IO(i) )
!
        do inod = 1, fld_IO%nnod_IO
          read(id_phys_file,*)  rtmp
        end do
      end do
!
      end subroutine read_rst_field_comps
!
! -------------------------------------------------------------------
!
      end module rst_data_IO_by_fld
