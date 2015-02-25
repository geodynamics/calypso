!>@file   m_time_data_IO.f90
!!@brief  module m_time_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
!!@verbatim
!!      subroutine write_step_data(id_file, my_rank)
!!      subroutine read_step_data(id_file)
!!
!!      subroutine write_step_data_b(id_file, my_rank)
!!      subroutine read_step_data_b(id_file)
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
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine write_step_data(id_file, my_rank)
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)'   )   '!  domain ID'
      write(id_file,'(i16)') my_rank
      write(id_file,'(a)'   )   '!  time step number'
      write(id_file,'(i16)') i_time_step_IO
      write(id_file,'(a)'   )   '!  time, Delta t'
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
      write(id_file) my_rank
      write(id_file) i_time_step_IO
      write(id_file)  time_IO, delta_t_IO
!
      end subroutine write_step_data_b
!
! -------------------------------------------------------------------
!
      subroutine read_step_data_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: itmp
!
!
      read(id_file) itmp
      read(id_file) i_time_step_IO
      read(id_file) time_IO, delta_t_IO
!
      end subroutine read_step_data_b
!
! -------------------------------------------------------------------
!
      end module m_time_data_IO
