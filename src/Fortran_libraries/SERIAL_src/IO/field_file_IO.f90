!> @file  field_file_IO.f90
!!      module field_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2007
!!@date   modified in May, 2015
!
!>@brief Rountines for field file IO
!!
!!@verbatim
!!      subroutine write_step_field_file                                &
!!     &         (file_name, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine read_and_allocate_field_file                         &
!!     &         (file_name, id_rank, fld_IO, iend)
!!
!!      subroutine read_step_field_file                                 &
!!     &         (file_name, id_rank, t_IO, fld_IO, iend)
!!      subroutine read_and_alloc_step_field                            &
!!     &         (file_name, id_rank, t_IO, fld_IO, iend)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine read_and_allocate_step_head                          &
!!     &         (file_name, id_rank, t_IO, fld_IO, iend)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_time_data
      use t_field_data_IO
      use time_data_IO
      use field_data_IO
      use set_parallel_file_name
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_step_field_file                                  &
     &         (file_name, id_rank, t_IO, fld_IO)
!
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'formatted')
!
      call write_step_data(id_phys_file, id_rank, t_IO)
      call write_field_data(id_phys_file, cast_long(fld_IO%nnod_IO),    &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO)
!
      close (id_phys_file)
!
      end subroutine write_step_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_and_allocate_field_file                           &
     &         (file_name, id_rank, fld_IO, iend)
!
      use skip_comment_f
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'formatted')
!
      call skip_comment(id_phys_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file,*) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data(id_phys_file, cast_long(fld_IO%nnod_IO),     &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, iend)
      if(iend .gt. 0) return
      close (id_phys_file)
!
      end subroutine read_and_allocate_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_step_field_file                                   &
     &         (file_name, id_rank, t_IO, fld_IO, iend)
!
      use skip_comment_f
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_phys_file, t_IO, iend)
      if(iend .gt. 0) return
!
      call skip_comment(id_phys_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) fld_IO%nnod_IO, fld_IO%num_field_IO
      read(id_phys_file,*) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call read_field_data(id_phys_file, cast_long(fld_IO%nnod_IO),     &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, iend)
      if(iend .gt. 0) return
      close (id_phys_file)
!
      end subroutine read_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_and_alloc_step_field                              &
     &         (file_name, id_rank, t_IO, fld_IO, iend)
!
      use skip_comment_f
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_phys_file, t_IO, iend)
      if(iend .gt. 0) return
!
      call skip_comment(id_phys_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file,*) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data(id_phys_file, cast_long(fld_IO%nnod_IO),     &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO, iend)
      if(iend .gt. 0) return
      close (id_phys_file)
!
      end subroutine read_and_alloc_step_field
!
!------------------------------------------------------------------
!
      subroutine read_and_allocate_step_head                            &
     &         (file_name, id_rank, t_IO, fld_IO, iend)
!
      use skip_comment_f
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii data file: ', trim(file_name)
      end if
!
      open(id_phys_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_phys_file, t_IO, iend)
      if(iend .gt. 0) return
!
      call skip_comment(id_phys_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) fld_IO%nnod_IO, fld_IO%num_field_IO
!
      call alloc_phys_name_IO(fld_IO)
      read(id_phys_file,*) fld_IO%num_comp_IO(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call read_field_name(id_phys_file, cast_long(fld_IO%nnod_IO),     &
     &                     fld_IO%num_field_IO, fld_IO%num_comp_IO,     &
     &                     fld_IO%fld_name, iend)
      if(iend .gt. 0) return
     close (id_phys_file)
!
      end subroutine read_and_allocate_step_head
!
!------------------------------------------------------------------
!
      end module field_file_IO
