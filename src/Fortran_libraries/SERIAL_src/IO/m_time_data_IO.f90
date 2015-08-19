!>@file   m_time_data_IO.f90
!!@brief  module m_time_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
!!@verbatim
!!      integer(kind = kint) function len_step_data_buf()
!!      function step_data_buffer(my_rank)
!!      subroutine read_step_data_buffer(textbuf, id_rank)
!!
!!      subroutine write_step_data(id_file, my_rank)
!!      subroutine read_step_data(id_file)
!!
!!      subroutine write_step_data_b(id_file, my_rank)
!!      subroutine read_step_data_b(id_file, id_rank, ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  id_file   file ID for data IO
!
      module m_time_data_IO
!
      use m_precision
!
      implicit none
!
!>      Time step
      integer(kind = kint) :: i_time_step_IO
!>      Time                  @f$ t @f$
      real(kind = kreal) :: time_IO
!>      Length of time step   @f$ \Delta t @f$
      real(kind = kreal) :: delta_t_IO
!
!>      Endian check integer
      integer(kind = kint), parameter :: i_UNIX = ichar('U') * 256**3   &
     &                                           +ichar('N') * 256**2   &
     &                                           +ichar('I') * 256      &
     &                                           +ichar('X')
!
      character(len=12), parameter :: TIME_HD1 = '!  domain ID'
      character(len=19), parameter :: TIME_HD2 = '!  time step number'
      character(len=16), parameter :: TIME_HD3 = '!  time, Delta t'
!
      integer(kind = kint), parameter :: l_hd = 12 + 19 + 16 + 3
      integer(kind = kint), parameter :: l_dt = 2*16 + 2*25 + 3
!
      integer(kind = kint), parameter :: len_step_data_buf = l_hd+l_dt
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
      function step_data_buffer(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len=len_step_data_buf) :: step_data_buffer
!
      character(len=16) :: buf_pe, buf_step
      character(len=2*25) :: buf_time
!
!
      write(buf_pe,'(i16)')      my_rank
      write(buf_step,'(i16)')    i_time_step_IO
      write(buf_time,'(1p2E25.15e3)') time_IO, delta_t_IO
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
      subroutine read_step_data_buffer(textbuf, id_rank)
!
      character(len=len_step_data_buf), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: id_rank
!
      character(len=16) :: tmp2
      character(len=16) :: tmp4
      character(len=50) :: tmp6
!
      write(tmp2,'(a16)') textbuf(14:29)
      write(tmp4,'(a16)') textbuf(51:66)
      write(tmp6,'(a50)') textbuf(85:134)
      read(tmp2,*) id_rank
      read(tmp4,*) i_time_step_IO
      read(tmp6,*) time_IO, delta_t_IO
!
      end subroutine read_step_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_step_data(id_file, my_rank)
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)'   )   TIME_HD1
      write(id_file,'(i16)') my_rank
      write(id_file,'(a)'   )   TIME_HD2
      write(id_file,'(i16)') i_time_step_IO
      write(id_file,'(a)'   )   TIME_HD3
      write(id_file,'(1p20E25.15e3)') time_IO, delta_t_IO
!
      end subroutine write_step_data
!
! -------------------------------------------------------------------
!
      subroutine read_step_data(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: itmp
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) i_time_step_IO
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*,err=99, end=99)  time_IO, delta_t_IO
!
      go to 10
  99    write(*,*) 'no delta t data... continue'
        delta_t_IO = 0.0d0
  10  continue
!
      end subroutine read_step_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_step_data_b(id_file, my_rank)
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file)  i_UNIX
      write(id_file)  my_rank
      write(id_file)  i_time_step_IO
      write(id_file)  time_IO, delta_t_IO
!
      end subroutine write_step_data_b
!
! -------------------------------------------------------------------
!
      subroutine read_step_data_b(id_file, id_rank, ierr)
!
      integer(kind = kint), intent(in) :: id_file, id_rank
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: itmp1, itmp2
!
!
      ierr =     0
!
      read(id_file) itmp1
      if(itmp1 .ne. i_UNIX) then
        ierr = -100
        return
      end if
!
      read(id_file) itmp2
      if(itmp2 .ne. id_rank) then
        ierr =     1
        return
      end if
!
      read(id_file) i_time_step_IO
      read(id_file) time_IO, delta_t_IO
!
!
      end subroutine read_step_data_b
!
! -------------------------------------------------------------------
!
      end module m_time_data_IO
