!>@file   dup_dynamobench_data_to_IO.f90
!!@brief  module dup_dynamobench_data_to_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine dup_dynamobench_monitor_data(sph_bc_U, sph_bc_B,     &
!!     &          ipol_base, bench, num_out, data_out)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(dynamobench_monitor), intent(in) :: bench
!!        integer(kind = kint), intent(in) :: num_out
!!        real(kind = kreal), intent(inout) :: data_out(num_out)
!!
!!      subroutine count_dynamobench_monitor_name(sph_bc_U, sph_bc_B,   &
!!     &         ipol_base, nfield_sph_spec, ntot_sph_spec)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
!!        type(base_field_address), intent(in) :: ipol_base
!!        integer(kind = kint), intent(inout) :: nfield_sph_spec
!!        integer(kind = kint), intent(inout) :: ntot_sph_spec
!!      subroutine copy_dynamobench_monitor_name                        &
!!     &         (sph_bc_U, sph_bc_B, ipol_base,                        &
!!     &          nfield_sph_spec, num_labels,                          &
!!     &          ncomp_sph_spec, ene_sph_spec_name)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
!!        type(base_field_address), intent(in) :: ipol_base
!!        integer(kind = kint), intent(in) :: nfield_sph_spec
!!        integer(kind = kint), intent(in) :: num_labels
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ncomp_sph_spec(nfield_sph_spec)
!!        character(len = kchara), intent(inout)                        &
!!       &                     :: ene_sph_spec_name(num_labels)
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module dup_dynamobench_data_to_IO
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_phys_address
      use t_base_field_labels
      use t_sph_volume_mean_square
      use t_field_4_dynamobench
      use t_read_sph_spectra
      use t_time_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dup_dynamobench_monitor_data(sph_bc_U, sph_bc_B,       &
     &          ipol_base, bench, num_out, data_out)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      type(dynamobench_monitor), intent(in) :: bench
!
      integer(kind = kint), intent(in) :: num_out
      real(kind = kreal), intent(inout) :: data_out(num_out)
!
      integer(kind = kint) :: jcou
!
      jcou = 0
      data_out(jcou+1) = bench%KE_bench(3)
      jcou = jcou + 1
!
      if(ipol_base%i_magne .gt. 0) then
        data_out(jcou+1) = bench%ME_bench(3)
        jcou = jcou + 1
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        data_out(jcou+1) = bench%mene_icore(3)
        jcou = jcou + 1
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        data_out(jcou+1) = bench%rotate_icore(0)
        jcou = jcou + 1
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        data_out(jcou+1) = bench%m_torque_icore(0)
        jcou = jcou + 1
      end if
!
      data_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_velo+2)
      jcou = jcou + 1
!
      if(ipol_base%i_magne .gt. 0) then
        data_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_magne+1)
        jcou = jcou + 1
      end if
      if(ipol_base%i_temp .gt. 0) then
        data_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_temp)
        jcou = jcou + 1
      end if
      if(ipol_base%i_light .gt. 0) then
        data_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_light)
        jcou = jcou + 1
      end if
!
      data_out(jcou+1) = bench%ave_phase_vr
      data_out(jcou+2:jcou+3) = bench%omega_vm4(1:2)
      jcou = jcou + 3
!
      end subroutine dup_dynamobench_monitor_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_dynamobench_monitor_name(sph_bc_U, sph_bc_B,    &
     &         ipol_base, nfield_sph_spec, ntot_sph_spec)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(inout) :: nfield_sph_spec
      integer(kind = kint), intent(inout) :: ntot_sph_spec
!
!
      nfield_sph_spec =  1 + 3 + 1
      ntot_sph_spec =    1 + 3 + 1
!
      if(ipol_base%i_magne .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1 + 1
        ntot_sph_spec =   ntot_sph_spec + 1 + 1
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      if(ipol_base%i_light .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      end subroutine count_dynamobench_monitor_name
!
! ----------------------------------------------------------------------
!
      subroutine copy_dynamobench_monitor_name                          &
     &         (sph_bc_U, sph_bc_B, ipol_base,                          &
     &          nfield_sph_spec, num_labels,                            &
     &          ncomp_sph_spec, ene_sph_spec_name)
!
      use m_time_labels
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(in) :: nfield_sph_spec
      integer(kind = kint), intent(in) :: num_labels
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
!
      integer(kind = kint) :: icou, jcou, i
!
      icou = 0
      ene_sph_spec_name(1) = fhd_t_step
      ene_sph_spec_name(2) = fhd_time
      jcou = 2
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'KE_total'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      if(ipol_base%i_magne .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'ME_total'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'ME_total_icore'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'omega_ic_z'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'MAG_torque_ic_z'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'v_phi'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      if(ipol_base%i_magne .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'B_theta'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'temperature'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(ipol_base%i_light .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'composition'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'Average_drift_vr'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'omega_vp44'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'omega_vt54'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      end subroutine copy_dynamobench_monitor_name
!
! ----------------------------------------------------------------------
!
      end module dup_dynamobench_data_to_IO
