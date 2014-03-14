!>@file   pole_sph_transform.f90
!!@brief  module pole_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Spherical transform for poles
!!
!!@verbatim
!!      subroutine pole_b_trans_scalar(ncomp_trans)
!!      subroutine pole_b_trans_vector(ncomp_trans)
!!      subroutine pole_b_trans_tensor(ncomp_trans)
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!
      module pole_sph_transform
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
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
      if(iflag_debug .gt. 0) write(*,*) 'set_pole_flag_4_sph_trans'
      call set_pole_flag_4_sph_trans(numnod, internal_node)
      call allocate_work_pole_sph_trans
!
      end subroutine init_pole_transform
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pole_b_trans_scalar(ncomp_trans)
!
      use spherical_SRs_N
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
!
      if (iflag_debug.gt.0)  write(*,*) 'sum_b_trans_pole_scalar',      &
     &                                 ncomp_trans
      call schmidt_b_trans_pole_scalar(ncomp_trans)
      call sum_b_trans_pole_scalar(ncomp_trans)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_scalar(ncomp_trans)
        call sum_b_trans_center_scalar(ncomp_trans)
      end if
!
      end subroutine pole_b_trans_scalar
!
! -----------------------------------------------------------------------
!
      subroutine pole_b_trans_vector(ncomp_trans)
!
      use spherical_SRs_N
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint) :: nb
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      nb = ncomp_trans / 3
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
!
      if (iflag_debug.gt.0)  write(*,*) 'sum_b_trans_pole_vect',        &
     &                     ncomp_trans
      call schmidt_b_trans_pole_vect(nb)
      call sum_b_trans_pole_vect(nb)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_vect(nb)
        call sum_b_trans_center_vect(nb)
      end if
!
      end subroutine pole_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine pole_b_trans_tensor(ncomp_trans)
!
      use spherical_SRs_N
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: ncomp_trans
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
!
      if(iflag_debug.gt.0) write(*,*) 'schmidt_b_trans_pole_scalar',    &
     &                               ncomp_trans
      call schmidt_b_trans_pole_scalar(ncomp_trans)
      call sum_b_trans_pole_scalar(ncomp_trans)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_scalar(ncomp_trans)
        call sum_b_trans_center_scalar(ncomp_trans)
      end if
!
      end subroutine pole_b_trans_tensor
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform
