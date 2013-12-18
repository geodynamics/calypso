!>@file   pole_sph_transform.f90
!!@brief  module pole_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transdorm at poles and center
!!
!!@verbatim
!!      subroutine init_pole_transform
!!      subroutine pole_b_trans_vector(nvector, nscalar, ntensor)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module pole_sph_transform
!
      use m_precision
!
      use calypso_mpi
      use m_phys_constants
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_pole_transform
!
      use m_geometry_parameter
      use m_work_pole_sph_trans
      use sum_b_trans_at_pole
!
!
      call set_pole_flag_4_sph_trans(numnod, internal_node)
      call allocate_work_pole_sph_trans
!
      end subroutine init_pole_transform
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pole_b_trans_vector(nvector, nscalar, ntensor)
!
      use spherical_SRs_N
      use legendre_bwd_trans_pole
      use legendre_bwd_trans_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
      integer(kind = kint) :: nfld, ncomp
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      nfld = nscalar + n_sym_tensor*ntensor
      ncomp = 3*nvector + nfld
      if (iflag_debug.gt.0)  write(*,*) 'send_recv_rj_2_rlm_N', ncomp
      call send_recv_rj_2_rlm_N(ncomp, sp_rj, sp_rlm)
!
      if(nvector .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &          write(*,*) 'leg_b_trans_pole_vector', nvector
        call leg_b_trans_pole_vector(ncomp, nvector)
      end if
      if(nfld .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &          write(*,*) 'leg_b_trans_pole_scalar', nfld
        call leg_b_trans_pole_scalar(ncomp, nvector, nfld)
      end if
!
      call sum_b_trans_pole_vect(ncomp)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(nvector .gt. 0) then
          call leg_b_trans_center_vector(ncomp, nvector)
        end if
        if(nfld .gt. 0) then
          call leg_b_trans_center_scalar(ncomp, nvector, nfld)
        end if
!
        call sum_b_trans_center_vector(ncomp)
      end if
!
      end subroutine pole_b_trans_vector
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform
