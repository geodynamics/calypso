!>@file   m_gauss_coefs_monitor_data.f90
!!@brief  module m_gauss_coefs_monitor_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!      subroutine allocate_pick_gauss
!!      subroutine allocate_pick_gauss_l
!!      subroutine allocate_pick_gauss_m
!!      subroutine allocate_gauss_coef_monitor
!!
!!      subroutine deallocate_pick_gauss
!!      subroutine deallocate_gauss_coef_monitor
!!
!!      subroutine write_gauss_coefs_4_monitor(my_rank, i_step, time)
!!
!!      subroutine open_gauss_coefs_read_monitor(id_pick)
!!      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,    &
!!     &          ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module m_gauss_coefs_monitor_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      File ID for Gauss coefficients IO
      integer(kind = kint), parameter :: id_gauss_coef = 23
!>      File prefix for Gauss coefficients file
      character(len = kchara) :: gauss_coefs_file_head
!
!>      Number of modes of Gauss coefficients to be evaluated
      integer(kind = kint) :: num_pick_gauss_coefs = 0
!>      Degree and Order ID of Gauss coefficients to be evaluated
      integer(kind = kint), allocatable :: idx_pick_gauss_mode(:,:)
!>      Number of degrees of Gauss coefficients to be evaluated
      integer(kind = kint) :: num_pick_gauss_l = 0
!>      Degree ID of Gauss coefficients to be evaluated
      integer(kind = kint), allocatable :: idx_pick_gauss_l(:)
!>      Number of orders of Gauss coefficients to be evaluated
      integer(kind = kint) :: num_pick_gauss_m = 0
!>      Order ID of Gauss coefficients to be evaluated
      integer(kind = kint), allocatable :: idx_pick_gauss_m(:)
!
!>      Number of modes of Gauss coefficients to be evaluated
      integer(kind = kint) :: num_pick_gauss_mode
!>      Global spherical harmonics ID to evaluate Gauss coefficients
      integer(kind = kint), allocatable :: idx_pick_gauss_coef_gl(:,:)
!>      Local spherical harmonics ID to evaluate Gauss coefficients
      integer(kind = kint), allocatable :: idx_pick_gauss_coef_lc(:)
!>      Gauss coefficients
      real(kind = kreal), allocatable :: gauss_coef_gl(:)
!>      Localy evaluated Gauss coefficients
      real(kind = kreal), allocatable :: gauss_coef_lc(:)
!>      Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
      character(len=kchara), allocatable :: gauss_mode_name(:)
!
!>      Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
      real(kind = kreal) :: r_4_gauss_coefs = 2.91
!
      private :: open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss
!
      allocate( idx_pick_gauss_mode(num_pick_gauss_coefs,2) )
      if(num_pick_gauss_coefs .gt. 0) idx_pick_gauss_mode = -1
!
      end subroutine allocate_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_l
!
      allocate( idx_pick_gauss_l(num_pick_gauss_l) )
      if(num_pick_gauss_l .gt. 0) idx_pick_gauss_l = -1
!
      end subroutine allocate_pick_gauss_l
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_m
!
      allocate( idx_pick_gauss_m(num_pick_gauss_m) )
      if(num_pick_gauss_m .gt. 0) idx_pick_gauss_m = -1
!
      end subroutine allocate_pick_gauss_m
!
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_coef_monitor
!
!
      allocate( idx_pick_gauss_coef_gl(num_pick_gauss_mode,3) )
      allocate( idx_pick_gauss_coef_lc(num_pick_gauss_mode) )
      allocate( gauss_coef_lc(num_pick_gauss_mode) )
      allocate( gauss_coef_gl(num_pick_gauss_mode) )
      allocate( gauss_mode_name(num_pick_gauss_mode) )
!
      if(num_pick_gauss_mode .gt. 0) then
        idx_pick_gauss_coef_gl = -1
        idx_pick_gauss_coef_lc =  0
        gauss_coef_lc = 0.0d0
        gauss_coef_gl = 0.0d0
      end if
!
      end subroutine allocate_gauss_coef_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss
!
      deallocate( idx_pick_gauss_mode )
      deallocate( idx_pick_gauss_l, idx_pick_gauss_m )
!
      end subroutine deallocate_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_gauss_coef_monitor
!
!
      deallocate(idx_pick_gauss_coef_gl, gauss_coef_gl)
      deallocate(idx_pick_gauss_coef_lc, gauss_coef_lc)
      deallocate(gauss_mode_name)
!
      end subroutine deallocate_gauss_coef_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_4_monitor
!
      use set_parallel_file_name
      use write_field_labels
!
      character(len = kchara) :: gauss_coefs_file_name
!
!
      call add_dat_extension(gauss_coefs_file_head,                     &
     &    gauss_coefs_file_name)
      open(id_gauss_coef, file = gauss_coefs_file_name,                 &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_gauss_coef, file = gauss_coefs_file_name,                 &
     &    form='formatted', status='replace')
!
!
      write(id_gauss_coef,'(a)')    'num_spectr, reference_radius'
      write(id_gauss_coef,'(i16,1pe25.15e3)')                           &
     &     num_pick_gauss_mode, r_4_gauss_coefs
!
      write(id_gauss_coef,'(a)',advance='NO')    't_step    time    '
!
      call write_multi_labels(id_gauss_coef, num_pick_gauss_mode,       &
     &    gauss_mode_name)
      write(id_gauss_coef,'(a)') ''
!
      end subroutine open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_gauss_coefs_4_monitor(my_rank, i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum
!
!
      if(num_pick_gauss_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_gauss_coefs_4_monitor
!
      write(id_gauss_coef,'(i16,1pe23.14e3)', advance='NO')             &
     &       i_step, time
      do inum = 1, num_pick_gauss_mode
        write(id_gauss_coef,'(1pe23.14e3)', advance='NO')               &
     &       gauss_coef_gl(inum)
      end do
      write(id_gauss_coef,'(a)') ''
!
      close(id_gauss_coef)
!
      end subroutine write_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_read_monitor(id_pick)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: id_pick
!
      integer(kind = kint) :: i
      character(len = kchara) :: gauss_coefs_file_name
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(gauss_coefs_file_head,                     &
     &    gauss_coefs_file_name)
      open(id_pick, file = gauss_coefs_file_name)
!
      call skip_comment(tmpchara,id_pick)
      read(id_pick,*) num_pick_gauss_mode
!
      call allocate_gauss_coef_monitor
!
      read(id_pick,*) (tmpchara,i=1,2),                                 &
     &                 gauss_mode_name(1:num_pick_gauss_mode)
!
      end subroutine open_gauss_coefs_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,      &
     &          ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99) i_step, time,                       &
     &       gauss_coef_gl(1:num_pick_gauss_mode)
      write(*,*) 'i_step', i_step, time
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module m_gauss_coefs_monitor_data
