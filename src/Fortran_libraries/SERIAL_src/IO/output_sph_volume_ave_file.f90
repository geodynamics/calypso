!> @file  output_sph_volume_ave_file.f90
!!      module output_sph_volume_ave_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_sph_vol_ave_file                               &
!!     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_mean_squares), intent(in) :: pwr
!!@endverbatim
!!
!!@n @param id_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
      module output_sph_volume_ave_file
!
      use m_precision
      use m_constants
      use t_time_data
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_energy_label_parameters
      use sph_mean_spectr_header_IO
!
      implicit none
!
!>      File ID for mean square data
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ave_file                                 &
     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use set_parallel_file_name
      use output_sph_pwr_volume_file
      use skip_comment_f
!
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: i
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      do i = 1, pwr%num_vol_spectr
        if(pwr%v_spectr(i)%iflag_volume_ave_sph .eq. 0)  cycle
!
        if(no_file_flag(pwr%v_spectr(i)%fhead_ave)) cycle
        fname_rms = add_dat_extension(pwr%v_spectr(i)%fhead_ave)
        write(mode_label,'(a)') 'EMPTY'
        call write_sph_volume_pwr_file(fname_rms, mode_label,           &
     &      ene_labels, time_d, sph_params, sph_rj,                     &
     &     pwr%v_spectr(i), pwr%v_spectr(i)%v_ave(1))
      end do
!
      end subroutine write_sph_vol_ave_file
!
! -----------------------------------------------------------------------
!
      end module output_sph_volume_ave_file
