!>@file   init_sph_MHD_elapsed_label.F90
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
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_SPH_TRNS
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_gen_SPH
!      use m_work_time_4_sleeve_extend
!
!
      call elapsed_label_4_MHD
      call elapsed_label_4_SPH_MHD
!
      call elpsed_label_4_sph_trns
      call elpsed_label_4_sph_detail
      call elpsed_label_4_fft_detail
!
      call elpsed_label_calypso_send_recv
!      call elapsed_label_4_Legendre_trans
!
      call elapsed_label_4_SGS_model
!      call elapsed_label_4_ele_comm_tbl
!      call elpsed_label_4_sleeve_ext
!
      call elpsed_label_gen_sph_grid
!
      end subroutine set_sph_MHD_elapsed_label
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_4_init_sph_mhd
!
      use m_work_time
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_SPH_TRNS
      use m_elapsed_labels_4_MHD
!
!
      call reset_elapse_after_init_SPH
      call reset_elapse_after_init_SDT
      call reset_elapse_after_init_FFT
!
      call reset_elapse_after_init_SR
      call reset_elapse_after_init_SPH_MHD
      call reset_elapse_after_init_SGS
      call reset_elapse_after_init_LEG
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
      call write_resolution_info(nprocs, sph%sph_params,                &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj,            &
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
      use calypso_mpi_int
      use m_work_time
      use t_spheric_parameter
      use transfer_to_long_integers
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
      call calypso_mpi_reduce_int                                       &
     &   (sph_rj%irank_sph_rj, nproc_rj_IO, cast_long(2), MPI_MAX, 0)
      call calypso_mpi_reduce_int                                       &
     &   (sph_rlm%irank_sph_rlm, nproc_rlm_IO, cast_long(2),            &
     &    MPI_MAX, 0)
      call calypso_mpi_reduce_int                                       &
     &   (sph_rtm%irank_sph_rtm, nproc_rtm_IO, cast_long(3),            &
     &    MPI_MAX, izero)
      call calypso_mpi_reduce_int                                       &
     &   (sph_rtp%irank_sph_rtp, nproc_rtp_IO, cast_long(3),            &
     &    MPI_MAX, izero)
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
     &         (num_pe, sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,  &
     &          nproc_rj_IO, nproc_rlm_IO, nproc_rtm_IO, nproc_rtp_IO)
!
      use m_work_time
      use t_spheric_parameter
      use set_parallel_file_name
!
      character(len=kchara) ::  file_name
      integer, intent(in) :: num_pe
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid),  intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: nproc_rj_IO(2)
      integer(kind = kint), intent(in) :: nproc_rlm_IO(2)
      integer(kind = kint), intent(in) :: nproc_rtm_IO(3)
      integer(kind = kint), intent(in) :: nproc_rtp_IO(3)
!
      character(len=kchara) ::  tmpchara
!
#ifdef _OPENMP
      integer, external :: omp_get_max_threads
#endif
!
!
      file_name = add_dat_extension(time_file_prefix)
      open(id_timer_file,file=file_name,position='append')
!
      write(id_timer_file,*)
      write(id_timer_file,*) '========================================'
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
      write(id_timer_file,*) 'Total MPI processes: ',  num_pe
#ifdef _OPENMP
        write(id_timer_file,*) 'Total OopenMP threads: ',               &
     &                        omp_get_max_threads()
#endif
!
      write(id_timer_file,*)                                            &
     &   'Processes for spectr (r, l and m):        ', nproc_rj_IO(1:2)
      write(id_timer_file,*)                                            &
     &   'Processes for Legendre trans. (r, l, m): ', nproc_rlm_IO(1:2)
      write(id_timer_file,*)                                            &
     &   'Processes for Legendre trans. (r, t, m): ', nproc_rtm_IO(1:3)
      write(id_timer_file,*)                                            &
     &   'Processes for physical space. (r, t, p): ', nproc_rtp_IO(1:3)
      write(id_timer_file,*)
!
      if(sph_rj%istep_rj(1) .eq. 1) then
        write(tmpchara,'(a)') 'radius'
      else
        write(tmpchara,'(a)') 'modes'
      end if
      write(id_timer_file,*)                                            &
     &   'Innermost loop for spectr (r, l and m):        ',             &
     &   trim(tmpchara)
!
      if(sph_rlm%istep_rlm(1) .eq. 1) then
        write(tmpchara,'(a)') 'radius'
      else
        write(tmpchara,'(a)') 'degree'
      end if
      write(id_timer_file,*)                                            &
     &   'Innermost loop for Legendre trans. (r, l, m): ',              &
     &   trim(tmpchara)
!
      if(sph_rtm%istep_rtm(1) .eq. 1) then
        write(tmpchara,'(a)') 'radius'
      else
        write(tmpchara,'(a)') 'meridian'
      end if
      write(id_timer_file,*)                                            &
     &   'Innermost loop for Legendre trans. (r, t, m): ',              &
     &   trim(tmpchara)
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        write(tmpchara,'(a)') 'phi'
      else
        write(tmpchara,'(a)') 'radius'
      end if
      write(id_timer_file,*)                                            &
     &   'Innermost loop for physical space. (r, t, p): ',              &
     &   trim(tmpchara)
      write(id_timer_file,*)
!
      close(id_timer_file)
!
      end subroutine write_resolution_info
!
! ----------------------------------------------------------------------
!
      end module  init_sph_MHD_elapsed_label
