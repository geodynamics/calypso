!>@file   sph_rtp_zonal_rms_data.f90
!!@brief  module sph_rtp_zonal_rms_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine zonal_mean_all_rtp_field(sph_rtp, node, nod_fld)
!!      subroutine zonal_rms_all_rtp_field(sph_rtp, node, nod_fld)
!!      subroutine zonal_cyl_rms_all_rtp_field(sph_rtp, node, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(inout) :: nod_fld
!!@endverbatim
!!
!!@n @param  numdir     Number of component of field
!!@n @param  irtp_fld   Start address for field @f$ f(\r,\theta\phi) @f$
!
      module sph_rtp_zonal_rms_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine zonal_mean_all_rtp_field(sph_rtp, node, nod_fld)
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_data
      use coordinate_convert_4_sph
      use cal_sph_zonal_ave_rms_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call overwrite_nodal_xyz_2_sph(node, nod_fld)
!
      if(sph_rtp%istep_rtp(1) .eq. 1) then
        call cal_sph_zonal_ave_data_rin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      else
        call cal_sph_zonal_ave_data_pin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      end if
!
      call overwrite_nodal_sph_2_xyz(node, nod_fld)
!
      end subroutine zonal_mean_all_rtp_field
!
! -------------------------------------------------------------------
!
      subroutine zonal_rms_all_rtp_field(sph_rtp, node, nod_fld)
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_data
      use coordinate_convert_4_sph
      use cal_sph_zonal_ave_rms_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call overwrite_nodal_xyz_2_sph(node, nod_fld)
!
      if(sph_rtp%istep_rtp(1) .eq. 1) then
        call cal_sph_zonal_rms_data_rin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      else
        call cal_sph_zonal_rms_data_pin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      end if
!
      call overwrite_nodal_sph_2_xyz(node, nod_fld)
!
      end subroutine zonal_rms_all_rtp_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine zonal_cyl_rms_all_rtp_field(sph_rtp, node, nod_fld)
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_data
      use coordinate_convert_4_sph
      use cal_sph_zonal_ave_rms_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call overwrite_nodal_xyz_2_cyl(node, nod_fld)
!
      if(sph_rtp%istep_rtp(1) .eq. 1) then
        call cal_sph_zonal_rms_data_rin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      else
        call cal_sph_zonal_rms_data_pin(sph_rtp%nidx_rtp,               &
     &     nod_fld%n_point, nod_fld%ntot_phys, ione, nod_fld%d_fld)
      end if
!
      call overwrite_nodal_cyl_2_xyz(node, nod_fld)
!
      end subroutine zonal_cyl_rms_all_rtp_field
!
! -------------------------------------------------------------------
!
      end module sph_rtp_zonal_rms_data
