!>@file   init_sph_MHD_elapsed_label.f90
!!@brief  module init_sph_MHD_elapsed_label
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine set_sph_MHD_elapsed_label
!!      subroutine reset_elapse_4_init_sph_mhd
!!      subroutine write_resolution_data(sph)
!!        type(sph_grids), intent(in) :: sph
!!@endverbatim
!
      module init_sph_MHD_elapsed_label
!
      use m_precision
!
      implicit none
!
      private :: check_num_of_process_4_sph, write_resolution_info
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_MHD_elapsed_label
!
      use m_work_time
!
!
      num_elapsed = 84
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                 '
      elapse_labels(2) = 'Initialization time        '
      elapse_labels(3) = 'Time evolution loop time   '
      elapse_labels(4) = 'Data IO time               '
      elapse_labels(5) = 'Evolution excluding IO     '
!
      elapse_labels( 6) = 'Linear time               '
      elapse_labels( 7) = 'Solver time               '
      elapse_labels( 8) = 'Nonlinear terms           '

      elapse_labels( 9) = 'Obtain field to output    '
      elapse_labels(10) = 'output_sph_restart_control'
      elapse_labels(11) = 'output_rms_sph_mhd_control'
      elapse_labels(12) = 'Visualizatio time         '
!
      elapse_labels(13) = 'Coriolis term             '
      elapse_labels(14) = 'sph backward transform    '
      elapse_labels(15) = 'cal nonlinear terms       '
      elapse_labels(16) = 'sph forward transform     '
      elapse_labels(17) = 'obtain explicit terms     '
!
      elapse_labels(18) = 'transfer rj  => rlm        '
      elapse_labels(19) = 'transfer rtm => rtp        '
      elapse_labels(20) = 'transfer rtp => rtm        '
      elapse_labels(21) = 'transfer rlm => rj         '
      elapse_labels(22) = 'Legendre backward transform'
      elapse_labels(23) = 'Legendre forward transform '
      elapse_labels(24) = 'Fourier transform          '
!
      elapse_labels(25) = 'order_b_trans_vector    '
      elapse_labels(26) = 'clear_b_trans_vector    '
      elapse_labels(27) = 'legendre_b_trans_vector '
      elapse_labels(28) = 'back_b_trans_vector     '
      elapse_labels(29) = 'order_f_trans_vector    '
      elapse_labels(30) = 'clear_f_trans_vector    '
      elapse_labels(31) = 'legendre_f_trans_vector '
      elapse_labels(32) = 'back_f_trans_vector     '
!
      elapse_labels(33) = 'copy_FFT_real       '
      elapse_labels(34) = 'dfftw_execute       '
      elapse_labels(35) = 'copy_FFT_complex    '
!
      elapse_labels(36) = 'set_to_send_buf_N    '
      elapse_labels(37) = 'calypso_send_recv_core    '
      elapse_labels(38) = 'set_from_recv_buf_rev_N    '
      elapse_labels(39) = 'unused    '
      elapse_labels(40) = 'unused    '
!
      elapse_labels(41) = 'Copy P_lm for bwd. trans.   '
      elapse_labels(42) = 'Copy spectrum for bwd. trans.    '
      elapse_labels(43) = 'mat product for bwd. trans.    '
      elapse_labels(44) = 'Copy fields to bwd. trans.    '
      elapse_labels(45) = 'Equator for bwd. trans.    '
      elapse_labels(46) = 'Copy P_lm for fwd. trans.    '
      elapse_labels(47) = 'Copy field for fwd. trans.    '
      elapse_labels(48) = 'mat product for fwd. trans.    '
      elapse_labels(49) = 'Copy spectrum to fwd. trans.    '
!
      elapse_labels(51) = 'copy_mhd_spectr_to_send.   '
      elapse_labels(52) = 'copy_mhd_field_from_trans.    '
      elapse_labels(53) = 'copy_mhd_spectr_from_recv.    '
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
!
      elapse_labels(71) = 'fieldline.    '
!
      elapse_labels(81) = 'Filtering fields   '
      elapse_labels(82) = 'Scale similarity   '
      elapse_labels(83) = 'Dynamic scheme     '
      elapse_labels(84) = 'SGS Buoyancy       '
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
      end subroutine set_sph_MHD_elapsed_label
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_4_init_sph_mhd
!
      use m_work_time
!
      integer(kind = kint) :: i
!
      do i = 3, 54
        call reset_elapsed_time(i)
      end do
!
      end subroutine reset_elapse_4_init_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine write_resolution_data(sph)
!
      use calypso_mpi
      use m_work_time
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph
!
      integer(kind = kint) :: nproc_rj_IO(2),  nproc_rlm_IO(2)
      integer(kind = kint) :: nproc_rtm_IO(3), nproc_rtp_IO(3)
!
!
      call check_num_of_process_4_sph                                   &
     &   (sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj,            &
     &    nproc_rj_IO, nproc_rlm_IO, nproc_rtm_IO, nproc_rtp_IO)
!
      if(my_rank .ne. 0) return
!
      call write_resolution_info                                        &
     &   (nprocs, sph%sph_params, sph%sph_rtp, sph%sph_rtm,             &
     &    nproc_rj_IO, nproc_rlm_IO, nproc_rtm_IO, nproc_rtp_IO)
!
      end subroutine write_resolution_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_num_of_process_4_sph                             &
     &         (sph_rtp, sph_rtm, sph_rlm, sph_rj,                      &
     &          nproc_rj_IO, nproc_rlm_IO, nproc_rtm_IO, nproc_rtp_IO)
!
      use calypso_mpi
      use m_work_time
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      integer(kind = kint), intent(inout) :: nproc_rj_IO(2)
      integer(kind = kint), intent(inout) :: nproc_rlm_IO(2)
      integer(kind = kint), intent(inout) :: nproc_rtm_IO(3)
      integer(kind = kint), intent(inout) :: nproc_rtp_IO(3)
!
!
!
      call MPI_REDUCE(sph_rj%irank_sph_rj, nproc_rj_IO, itwo,           &
     &    CALYPSO_INTEGER, MPI_MAX, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE(sph_rlm%irank_sph_rlm, nproc_rlm_IO, itwo,        &
     &    CALYPSO_INTEGER, MPI_MAX, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE(sph_rtm%irank_sph_rtm, nproc_rtm_IO, ithree,      &
     &    CALYPSO_INTEGER, MPI_MAX, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE(sph_rtp%irank_sph_rtp, nproc_rtp_IO, ithree,      &
     &    CALYPSO_INTEGER, MPI_MAX, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) return
      nproc_rj_IO(1:2) =  nproc_rj_IO(1:2) +  1
      nproc_rlm_IO(1:2) = nproc_rlm_IO(1:2) + 1
      nproc_rtm_IO(1:3) = nproc_rtm_IO(1:3) + 1
      nproc_rtp_IO(1:3) = nproc_rtp_IO(1:3) + 1
!
      end subroutine check_num_of_process_4_sph
!
! ----------------------------------------------------------------------
!
      subroutine write_resolution_info                                  &
     &         (nprocs, sph_params, sph_rtp, sph_rtm,                   &
     &          nproc_rj_IO, nproc_rlm_IO, nproc_rtm_IO, nproc_rtp_IO)
!
      use m_work_time
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: nprocs
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
!
      integer(kind = kint), intent(in) :: nproc_rj_IO(2)
      integer(kind = kint), intent(in) :: nproc_rlm_IO(2)
      integer(kind = kint), intent(in) :: nproc_rtm_IO(3)
      integer(kind = kint), intent(in) :: nproc_rtp_IO(3)
!
!
      open(id_timer_file,file=time_file_name,position='append')
!
      write(id_timer_file,*)
      write(id_timer_file,*) '=========================================='
      write(id_timer_file,*) 'Truncation level:      ',                 &
     &                      sph_params%l_truncation
      write(id_timer_file,*) 'Longitudinal symmetry: ',                 &
     &                      sph_params%m_folding
      write(id_timer_file,*) 'N_r for fluid shell:   ',                 &
     &            (sph_params%nlayer_CMB - sph_params%nlayer_ICB)
      write(id_timer_file,*) 'N_theta:               ',                 &
     &                      sph_rtm%nidx_rtm(2)
      write(id_timer_file,*) 'N_phi:                 ',                 &
     &                      sph_rtp%nidx_rtp(3)
!
      write(id_timer_file,*) 'Total MPI processes: ',  nprocs
      write(id_timer_file,*)                                            &
     &   'Processes for spetr (r, l and m):        ', nproc_rj_IO(1:2)
      write(id_timer_file,*)                                            &
     &   'Processes for Legendre trans. (r, l, m): ', nproc_rlm_IO(1:2)
      write(id_timer_file,*)                                            &
     &   'Processes for Legendre trans. (r, t, m): ', nproc_rtm_IO(1:3)
      write(id_timer_file,*)                                            &
     &   'Processes for physical space. (r, t, p): ', nproc_rtp_IO(1:3)
      write(id_timer_file,*)
!
      close(id_timer_file)
!
      end subroutine write_resolution_info
!
! ----------------------------------------------------------------------
!
      end module  init_sph_MHD_elapsed_label
