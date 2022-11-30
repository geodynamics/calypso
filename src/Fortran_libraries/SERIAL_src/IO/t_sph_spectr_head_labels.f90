!>@file   t_sph_spectr_head_labels.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine copy_read_ene_head_labels(sph_lbl_IN, sph_lbl_OUT)
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_OUT
!!
!!      logical function cmp_layer_monitor_head_labels(sph_lbl_IN,      &
!!     &                                               sph_lbl_OUT)
!!      logical function cmp_volume_monitor_head_labels(sph_lbl_IN,     &
!!     &                                                sph_lbl_OUT)
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!!@endverbatim
      module t_sph_spectr_head_labels
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type sph_spectr_head_labels
!>        Lebal of the first item (truncation, Number of radial grid)
        character(len = kchara) :: hdr_nri, hdr_ltr
!>        Lebal of the second item (Id of ICB and CMB)
        character(len = kchara) :: hdr_ICB_id, hdr_CMB_id
!>        Lebal of the third item (Inner boundary)
        character(len = kchara) :: hdr_kr_in, hdr_r_in
!>        Lebal of the forth item (Outer boundary)
        character(len = kchara) :: hdr_kr_out, hdr_r_out
!>        Lebal of the field item (Field information)
        character(len = kchara) :: hdr_num_field, hdr_num_comp
      end type sph_spectr_head_labels
!
      type(sph_spectr_head_labels), parameter :: sph_pwr_labels         &
     &  = sph_spectr_head_labels(hdr_nri = 'Radial_layers',             &
     &                           hdr_ltr = 'Truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Lower_boundary_ID',      &
     &                           hdr_r_in =   'Lower_boundary_radius',  &
     &                           hdr_kr_out = 'Upper_boundary_ID',      &
     &                           hdr_r_out =  'Upper_boundary_radius',  &
     &                           hdr_num_field = 'Number_of_fields',    &
     &                           hdr_num_comp = 'Number_of_components')
!
      logical, parameter :: flag_current_fmt = .FALSE.
      logical, parameter :: spectr_on =        .TRUE.
      logical, parameter :: spectr_off =       .FALSE.
      logical, parameter :: volume_on =        .TRUE.
      logical, parameter :: volume_off =       .FALSE.
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_head_labels(sph_lbl_IN, sph_lbl_OUT)
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_OUT
!
!
      sph_lbl_OUT%hdr_num_field = sph_lbl_IN%hdr_num_field
      sph_lbl_OUT%hdr_num_comp =  sph_lbl_IN%hdr_num_comp
      sph_lbl_OUT%hdr_nri =    sph_lbl_IN%hdr_nri
      sph_lbl_OUT%hdr_ltr =    sph_lbl_IN%hdr_ltr
      sph_lbl_OUT%hdr_ICB_id = sph_lbl_IN%hdr_ICB_id
      sph_lbl_OUT%hdr_CMB_id = sph_lbl_IN%hdr_CMB_id
      sph_lbl_OUT%hdr_kr_in =  sph_lbl_IN%hdr_kr_in
      sph_lbl_OUT%hdr_kr_out = sph_lbl_IN%hdr_kr_out
      sph_lbl_OUT%hdr_r_in =   sph_lbl_IN%hdr_r_in
      sph_lbl_OUT%hdr_r_out =  sph_lbl_IN%hdr_r_out
!
      end subroutine copy_read_ene_head_labels
!
!   --------------------------------------------------------------------
!
      logical function cmp_layer_monitor_head_labels(sph_lbl_IN,        &
     &                                               sph_lbl_OUT)
!
      use skip_comment_f
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_layer_monitor_head_labels                                     &
     &        = cmp_no_case(sph_lbl_IN%hdr_num_field,                   &
     &                      sph_lbl_OUT%hdr_num_field)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'Field label does not match',                        &
     &      trim(sph_lbl_IN%hdr_num_field), ',   ',                     &
     &      trim(sph_lbl_OUT%hdr_num_field)
        return
      end if
!
      cmp_layer_monitor_head_labels                                     &
     &  = cmp_no_case(sph_lbl_IN%hdr_num_comp,                          &
     &                sph_lbl_OUT%hdr_num_comp)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'Field label does not match',                        &
     &      trim(sph_lbl_IN%hdr_num_comp), ',   ',                      &
     &      trim(sph_lbl_OUT%hdr_num_comp)
        return
      end if
!
      cmp_layer_monitor_head_labels                                     &
     &        = cmp_no_case(sph_lbl_IN%hdr_nri, sph_lbl_OUT%hdr_nri)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'first label does not match: ',                      &
     &      trim(sph_lbl_IN%hdr_nri), ',   ', trim(sph_lbl_OUT%hdr_nri)
        return
      end if
!
      cmp_layer_monitor_head_labels                                     &
     &        = cmp_no_case(sph_lbl_IN%hdr_ltr, sph_lbl_OUT%hdr_ltr)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'first label does not match: ',                      &
     &      trim(sph_lbl_IN%hdr_ltr), ',   ', trim(sph_lbl_OUT%hdr_ltr)
        return
      end if
!
      cmp_layer_monitor_head_labels                                     &
     &        = cmp_no_case(sph_lbl_IN%hdr_ICB_id,                      &
     &                      sph_lbl_OUT%hdr_ICB_id)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'second label does not match: ',                     &
     &      trim(sph_lbl_IN%hdr_ICB_id), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_ICB_id)
        return
      end if
!
      cmp_layer_monitor_head_labels                                     &
     &        = cmp_no_case(sph_lbl_IN%hdr_CMB_id,                      &
     &                      sph_lbl_OUT%hdr_CMB_id)
      if(.not. cmp_layer_monitor_head_labels) then
        write(*,*) 'second label does not match: ',                     &
     &      trim(sph_lbl_IN%hdr_CMB_id), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_CMB_id)
        return
      end if
!
      end function cmp_layer_monitor_head_labels
!
!   --------------------------------------------------------------------
!
      logical function cmp_volume_monitor_head_labels(sph_lbl_IN,       &
     &                                                sph_lbl_OUT)
!
      use skip_comment_f
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_volume_monitor_head_labels                                    &
     &        = cmp_no_case(sph_lbl_IN%hdr_kr_in,                       &
     &                      sph_lbl_OUT%hdr_kr_in)
      if(.not. cmp_volume_monitor_head_labels) then
        write(*,*) 'third label does not match',                        &
     &      trim(sph_lbl_IN%hdr_kr_in), ',   ',                         &
     &      trim(sph_lbl_OUT%hdr_kr_in)
        return
      end if
!
      cmp_volume_monitor_head_labels                                    &
     &        = cmp_no_case(sph_lbl_IN%hdr_r_in, sph_lbl_OUT%hdr_r_in)
      if(.not. cmp_volume_monitor_head_labels) then
        write(*,*) 'third label does not match',                        &
     &      trim(sph_lbl_IN%hdr_r_in), ',   ',                          &
     &      trim(sph_lbl_OUT%hdr_r_in)
        return
      end if
!
      cmp_volume_monitor_head_labels                                    &
     &        = cmp_no_case(sph_lbl_IN%hdr_kr_out,                      &
     &                      sph_lbl_OUT%hdr_kr_out)
      if(.not. cmp_volume_monitor_head_labels) then
        write(*,*) 'forth label does not match',                        &
     &      trim(sph_lbl_IN%hdr_kr_out), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_kr_out)
        return
      end if
!
      cmp_volume_monitor_head_labels                                    &
     &        = cmp_no_case(sph_lbl_IN%hdr_r_out,                       &
     &                      sph_lbl_OUT%hdr_r_out)
      if(.not. cmp_volume_monitor_head_labels) then
        write(*,*) 'forth label does not match',                        &
     &      trim(sph_lbl_IN%hdr_r_out), ',   ',                         &
     &      trim(sph_lbl_OUT%hdr_r_out)
        return
      end if
!
      end function cmp_volume_monitor_head_labels
!
!   --------------------------------------------------------------------
!
      end module t_sph_spectr_head_labels
