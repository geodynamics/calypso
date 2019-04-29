!>@file   time_data_IO.f90
!!@brief  module time_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
!!@verbatim
!!      function step_data_buffer(id_rank, t_IO)
!!      subroutine read_step_data_buffer(textbuf, id_rank, t_IO)
!!
!!      subroutine write_step_data(id_file, id_rank, t_IO)
!!      subroutine read_step_data(id_file, t_IO)
!!        type(time_data), intent(inout) :: t_IO
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  id_file   file ID for data IO
!
      module time_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_time_data
!
      implicit none
!
!
      character(len=12), parameter :: TIME_HD1 = '!  domain ID'
      character(len=19), parameter :: TIME_HD2 = '!  time step number'
      character(len=16), parameter :: TIME_HD3 = '!  time, Delta t'
!
      integer, parameter :: l_hd = 12 + 19 + 16 + 3
      integer, parameter :: l_dt = 2*16 + 2*25 + 3
!
      integer, parameter :: len_step_data_buf = l_hd+l_dt
!
      private :: TIME_HD1, TIME_HD2, TIME_HD3
      private :: l_hd, l_dt
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      function step_data_buffer(id_rank, t_IO)
!
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: t_IO
!
      character(len=len_step_data_buf) :: step_data_buffer
!
      character(len=16) :: buf_pe, buf_step
      character(len=2*25) :: buf_time
!
!
      write(buf_pe,'(i16)')      id_rank
      write(buf_step,'(i16)')         t_IO%i_time_step
      write(buf_time,'(1p2E25.15e3)') t_IO%time, t_IO%dt
!
      step_data_buffer =   TIME_HD1 // char(10)                         &
     &                  // buf_pe   // char(10)                         &
     &                  // TIME_HD2 // char(10)                         &
     &                  // buf_step // char(10)                         &
     &                  // TIME_HD3 // char(10)                         &
     &                  // buf_time // char(10)
!
      end function step_data_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_step_data_buffer(textbuf, id_rank, t_IO)
!
      character(len=len_step_data_buf), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: id_rank
      type(time_data), intent(inout) :: t_IO
!
      character(len=16) :: tmp2
      character(len=16) :: tmp4
      character(len=50) :: tmp6
!
      write(tmp2,'(a16)') textbuf(14:29)
      write(tmp4,'(a16)') textbuf(51:66)
      write(tmp6,'(a50)') textbuf(85:134)
      read(tmp2,*) id_rank
      read(tmp4,*) t_IO%i_time_step
      read(tmp6,*) t_IO%time, t_IO%dt
!
      end subroutine read_step_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_step_data(id_file, id_rank, t_IO)
!
      integer(kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(time_data), intent(in) :: t_IO
!
!
      write(id_file,'(a)'   )   TIME_HD1
      write(id_file,'(i16)') id_rank
      write(id_file,'(a)'   )   TIME_HD2
      write(id_file,'(i16)') t_IO%i_time_step
      write(id_file,'(a)'   )   TIME_HD3
      write(id_file,'(1p20E25.15e3)') t_IO%time, t_IO%dt
!
      end subroutine write_step_data
!
! -------------------------------------------------------------------
!
      subroutine read_step_data(id_file, t_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(time_data), intent(inout) :: t_IO
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) t_IO%i_time_step
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*,err=99, end=99)                           &
     &                        t_IO%time, t_IO%dt
!
      go to 10
  99    write(*,*) 'no delta t data... continue'
        t_IO%dt = 0.0d0
  10  continue
!
      end subroutine read_step_data
!
! -------------------------------------------------------------------
!
      end module time_data_IO
