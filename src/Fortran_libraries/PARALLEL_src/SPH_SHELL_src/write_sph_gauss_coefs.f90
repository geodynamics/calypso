!>@file   write_sph_gauss_coefs.f90
!!@brief  module write_sph_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_sph_gauss_coefs_file                          &
!!     &         (time_d, sph_params, sph_rj, ipol, rj_fld,             &
!!     &          gauss, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: gauss
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!     integer(kind = kint) function check_gauss_coefs_num(gauss)
!!        type(picked_spectrum_data), intent(in) :: gauss
!!@endverbatim
!!
      module write_sph_gauss_coefs
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use m_monitor_file_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_gauss_coef = 23
!
      private :: write_sph_gauss_coefes, picked_gauss_head
      private :: check_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_sph_gauss_coefs_file                            &
     &         (time_d, sph_params, sph_rj, ipol, rj_fld,               &
     &          gauss, SR_sig)
!
      use t_solver_SR
      use set_parallel_file_name
      use delete_data_files
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(send_recv_status), intent(inout) :: SR_sig
!
      real(kind=kreal), allocatable :: d_rj_out(:)
!
      character(len=kchara) :: file_name
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: i
!
!
      if(gauss%num_sph_mode .le. 0) return
!
      if(my_rank .eq. 0) then
        allocate(d_rj_out(gauss%istack_picked_spec_lc(nprocs)))
!$omp parallel workshare
        d_rj_out(1:gauss%istack_picked_spec_lc(nprocs)) = 0.0d0
!$omp end parallel workshare
      else
        allocate(d_rj_out(0))
      end if
!
      call write_sph_gauss_coefes(sph_params, sph_rj, ipol, rj_fld,     &
     &    gauss, gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
!
      if(my_rank .eq. 0) then
        file_name = add_dat_extension(gauss%file_prefix)
        if(check_file_exist(file_name)) then 
          open(id_gauss_coef,file=file_name, status='old',              &
      &        form='formatted',position='append')
        else
          open(id_gauss_coef,file=file_name, status='new',              &
     &         form='formatted')
          write(id_gauss_coef,'(a,i16,1pe25.15e3,a1,a)', ADVANCE='NO')  &
     &          hd_pick_gauss_head(),                                   &
     &          gauss%num_sph_mode, gauss%radius_gl(1), char(10),       &
     &          hd_time_label()
          do i = 1, gauss%istack_picked_spec_lc(nprocs)
            write(id_gauss_coef,'(a,a4)', ADVANCE='NO')                 &
     &          trim(gauss%gauss_mode_name_out(i)), '    '
          end do
          write(id_gauss_coef,'(a1)', ADVANCE='NO') char(10)
        end if
!
        write(fmt_txt,'(a1,i8,a13)')                                    &
     &      '(', gauss%istack_picked_spec_lc(nprocs), '(1pE25.14e3))'
!
        write(id_gauss_coef,'(a)',ADVANCE='NO')                         &
     &         picked_gauss_head(time_d%i_time_step, time_d%time)
        write(id_gauss_coef,fmt_txt)                                    &
     &         d_rj_out(1:gauss%istack_picked_spec_lc(nprocs))
        close(id_gauss_coef)
      end if
      deallocate(d_rj_out)
!
      end subroutine append_sph_gauss_coefs_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_coefes(sph_params, sph_rj,             &
     &          ipol, rj_fld, gauss, ntot_gauss, d_rj_out, SR_sig)
!
      use t_solver_SR
      use pickup_gauss_coefficients
      use collect_SR_N
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
      integer(kind = kint), intent(in) :: ntot_gauss
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_gauss)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint_gl) :: num
      real(kind=kreal), allocatable :: d_rj_lc(:)
!
!
      num = gauss%istack_picked_spec_lc(my_rank+1)                      &
     &     - gauss%istack_picked_spec_lc(my_rank)
      allocate(d_rj_lc(gauss%num_sph_mode_lc))
      if(num .gt. 0) then
        call gauss_coefficients_4_write                                 &
     &     (sph_params, sph_rj, ipol, rj_fld, gauss, d_rj_lc)
      end if
!
      call collect_small_send_recv(gauss%istack_picked_spec_lc,         &
     &    gauss%num_sph_mode_lc, d_rj_lc,                               &
     &    gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
      deallocate(d_rj_lc)
!
      end subroutine write_sph_gauss_coefes
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      character(len = 16+25) function picked_gauss_head(i_step, time)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      write(picked_gauss_head,'(i16,1pe25.14e3)') i_step, time
!
      end function  picked_gauss_head
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     integer(kind = kint) function check_gauss_coefs_num(gauss)
!
      use set_parallel_file_name
!
      type(picked_spectrum_data), intent(in) :: gauss
!!
      character(len = kchara) :: file_name
!
!
      check_gauss_coefs_num = 0
      if(gauss%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      file_name = add_dat_extension(gauss%file_prefix)
      open(id_gauss_coef, file = file_name,                             &
     &    form='formatted', status='old', err = 99)
!
      check_gauss_coefs_num = check_gauss_coefs_4_monitor(gauss)
      close(id_gauss_coef)
      return
!
  99  continue
      write(*,*) 'No Gauss coefficient file'
      return
!
      end function check_gauss_coefs_num
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &      function check_gauss_coefs_4_monitor(gauss)
!
      use skip_comment_f
!
      type(picked_spectrum_data), intent(in) :: gauss
!
      integer(kind = kint) :: nmode_read
      real(kind = kreal) :: radius_read
!
      character(len=255) :: tmpchara
!
!
      call skip_comment(tmpchara,id_gauss_coef)
      read(id_gauss_coef,*) nmode_read, radius_read
!      write(*,*) 'num_mode', gauss%num_sph_mode, nmode_read
!      write(*,*) 'radius_gauss', gauss%radius_gl(1), radius_read
      if(gauss%num_sph_mode .ne. nmode_read) then
        write(*,*) 'Number of Gauss coefficients does not match ',      &
     &             'with the data in the file'
        check_gauss_coefs_4_monitor = 1
        return
      end if
      if(abs(gauss%radius_gl(1) - radius_read) .gt. 1.0E-8) then
        write(*,*) 'Radius of Gauss coefficients does not match ',      &
     &             'with the data in the file',                         &
     &              gauss%radius_gl(1), radius_read
        check_gauss_coefs_4_monitor = 1
        return
      end if
!
      check_gauss_coefs_4_monitor = 0
      return
!
      end function check_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module write_sph_gauss_coefs
