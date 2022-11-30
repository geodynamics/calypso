!>@file   sph_mean_spectr_header_IO.f90
!!@brief  module sph_mean_spectr_header_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine dup_sph_vol_spectr_header                            &
!!     &         (mode_label, ltr, nlayer_ICB, nlayer_CMB,              &
!!     &          ene_labels, sph_rj, v_pwr, sph_OUT)
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!      subroutine dup_sph_layer_spectr_header(mode_label,              &
!!     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!@endverbatim
!!
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module sph_mean_spectr_header_IO
!
      use m_precision
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_energy_label_parameters
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dup_sph_vol_spectr_header                              &
     &         (mode_label, ltr, nlayer_ICB, nlayer_CMB,                &
     &          ene_labels, sph_rj, v_pwr, sph_OUT)
!
      use t_read_sph_spectra
      use m_time_labels
!
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i, icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = sph_rj%nidx_rj(1)
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = v_pwr%kr_inside
      sph_OUT%kr_outer = v_pwr%kr_outside
      sph_OUT%r_inner =  v_pwr%r_inside
      sph_OUT%r_outer =  v_pwr%r_outside
!
      sph_OUT%nfield_sph_spec = v_pwr%num_fld_sq
      sph_OUT%ntot_sph_spec =   v_pwr%ntot_comp_sq
      sph_OUT%num_time_labels = 2
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%num_time_labels = sph_OUT%num_time_labels + 1
        call alloc_sph_espec_name(sph_OUT)
      else
        call alloc_sph_espec_name(sph_OUT)
      end if
!
      sph_OUT%ncomp_sph_spec(1:v_pwr%num_fld_sq)                        &
     &      = v_pwr%num_comp_sq(1:v_pwr%num_fld_sq)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%ene_sph_spec_name(sph_OUT%num_time_labels)              &
     &                                       = trim(mode_label)
      end if
!
      icou = sph_OUT%num_time_labels
      do i = 1, v_pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels,                             &
     &      v_pwr%num_comp_sq(i), v_pwr%pwr_name(i),                    &
     &      sph_OUT%ene_sph_spec_name(icou+1))
        icou = icou + v_pwr%num_comp_sq(i)
      end do
!
      end subroutine dup_sph_vol_spectr_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_sph_layer_spectr_header(mode_label,                &
     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
!
      use t_read_sph_spectra
      use m_time_labels
!
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      integer(kind = kint) :: i, icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = pwr%nri_rms
      sph_OUT%nri_dat = pwr%nri_rms
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
!
      sph_OUT%nfield_sph_spec = pwr%num_fld_sq
      sph_OUT%ntot_sph_spec =   pwr%ntot_comp_sq
      sph_OUT%num_time_labels = 4
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%num_time_labels = sph_OUT%num_time_labels + 1
        call alloc_sph_espec_name(sph_OUT)
      else
        call alloc_sph_espec_name(sph_OUT)
      end if
!
      sph_OUT%ncomp_sph_spec(1:pwr%num_fld_sq)                          &
     &      = pwr%num_comp_sq(1:pwr%num_fld_sq)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Radius_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Radius'
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%ene_sph_spec_name(sph_OUT%num_time_labels)              &
     &                                       = trim(mode_label)
      end if
!
      icou = sph_OUT%num_time_labels
      do i = 1, pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels,                             &
     &      pwr%num_comp_sq(i), pwr%pwr_name(i),                        &
     &      sph_OUT%ene_sph_spec_name(icou+1))
        icou = icou + pwr%num_comp_sq(i)
      end do
!
      end subroutine dup_sph_layer_spectr_header
!
! -----------------------------------------------------------------------
!
      end module sph_mean_spectr_header_IO

