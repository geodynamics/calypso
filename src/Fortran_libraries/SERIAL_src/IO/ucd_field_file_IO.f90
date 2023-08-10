!>@file  ucd_field_file_IO.f90
!!       module ucd_field_file_IO
!!
!! @author H. Matsui
!! @date   Programmed in July, 2006
!
!> @brief ascii format data IO
!!
!!@verbatim
!!      subroutine write_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
!!        type(time_data), intent(in) :: t_IO
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_ucd_2_fld_file                                  &
!!     &         (id_rank, file_name, t_IO, ucd, iend)
!!      subroutine read_alloc_ucd_2_fld_file                            &
!!     &         (id_rank, file_name, t_IO, ucd, iend)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!        integer(kind = kint), intent(inout) :: iend
!!@endverbatim
!!
!!@param id_rank    process ID
!!@param file_name  File name
!!@param t_IO      Structure for time information
!!@param ucd       Structure for FEM field data IO
!
      module ucd_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_field_file_format
!
      use t_time_data
      use t_ucd_data
!
      use time_data_IO
      use field_data_IO
!
      implicit none
!
!>      file ID for field file IO
      integer(kind = kint), parameter, private :: id_fld_file = 16
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file(id_rank, file_name, t_IO, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write ascii step data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call write_step_data(id_fld_file, id_rank, t_IO)
      call write_field_data(id_fld_file, ucd%nnod, ucd%num_field,       &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close (id_fld_file)
!
      end subroutine write_ucd_2_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file                                    &
     &         (id_rank, file_name, t_IO, ucd, iend)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_fld_file, t_IO, iend)
      if(iend .gt. 0) return
!
      call skip_comment(id_fld_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) ucd%nnod, ucd%num_field
      read(id_fld_file,*) ucd%num_comp(1:ucd%num_field)
!
      call read_field_data(id_fld_file, ucd%nnod, ucd%num_field,        &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd, iend)
      if(iend .gt. 0) return
!
      close (id_fld_file)
!
      end subroutine read_ucd_2_fld_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file                              &
     &         (id_rank, file_name, t_IO, ucd, iend)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(inout) :: iend
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(id_fld_file, file = file_name, form = 'formatted')
!
      call read_step_data(id_fld_file, t_IO, iend)
      if(iend .gt. 0) return
!
      call skip_comment(id_fld_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) ucd%nnod, ucd%num_field
!
      call allocate_ucd_phys_name(ucd)
      read(id_fld_file,*) ucd%num_comp(1:ucd%num_field)
!
      call cal_istack_ucd_component(ucd)
      call allocate_ucd_phys_data(ucd)
!
      call read_field_data(id_fld_file,                                 &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp,                       &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, iend)
      if(iend .gt. 0) return
!
      close (id_fld_file)
!
      end subroutine read_alloc_ucd_2_fld_file
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO
