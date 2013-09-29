!>@file   sum_b_trans_at_pole.f90
!!@brief      module sum_b_trans_at_pole
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine set_pole_flag_4_sph_trans(numnod, internal_node)
!!
!!      subroutine sum_b_trans_pole_scalar(nb)
!!      subroutine sum_b_trans_pole_vect(nb)
!!      subroutine sum_b_trans_center_scalar(nb)
!!      subroutine sum_b_trans_center_vect(nb)
!!
!!------------------------------------------------------------------
!!
!!      vr =  l*(l+1)*Y(l,0)* S(l,0) / r**2
!!      vt =  (dYdt(l,1s)*dSdr(l,1s)
!!           + dYdt(l,1c)*dSdr(l,1c))  / r
!!         + cos(theta) * (d2Ydtdp(l,1s)*T(l,1s) 
!!                       + d2Ydtdp(l,1c)*T(l,1c))  / r
!!      vp = cos(theta) * (d2Ydtdp(l,1s)*dSdr(l,1s)
!!                       + d2Ydtdp(l,1c)*dSdr(l,1c))  / r
!!           -(dYdt(l,1s)*T(l,1s)
!!           + dYdt(l,1c)*T(l,1c))  / r
!!
!!  if phi = 0
!!
!!      vr =  l*(l+1)*P(l,0)* S(l,0) / r**2
!!      vt =  dPdt(l,1)*dSdr(l,1c)  / r
!!         + cos(theta) * dPdt(l,1)*T(l,1s) / r
!!      vp = cos(theta) * dPdt(l,1)*dSdr(l,1s) / r
!!           - dPdt(l,1)*T(l,1c)  / r
!!
!! if z > 0 (North pole)
!!
!!      vx = vt
!!      vy = vp
!!      vz = vr
!!
!! if z < 0 (South pole)
!!
!!      vx = -vt
!!      vy =  vp
!!      vz = -vr
!!
!!------------------------------------------------------------------
!!------------------------------------------------------------------
!!
!! if r= 0 (Center)
!!
!!      vz =  2 * P(1,0) * S(1,0) / r_c**2
!!         =  2 * S(1,0) / r_c**2
!!      vx =  2 * dPdt(l,1) * S(1,1c) / r_c**2
!!         = - 2 * S(1,1c) / r_c**2
!!      vy =  2 * dPdt(l,1) * S(1,1s) / r_c**2
!!         = - 2 * S(1,1s) / r_c**2
!!------------------------------------------------------------------
!!@verbatim
!!
!! @param  numnod           number of node for FEM mesh
!! @param  internal_node    number of internal node for FEM mesh
!! @param  nb               number of field
!
      module sum_b_trans_at_pole
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_phys_constants
!
      use m_work_pole_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_pole_flag_4_sph_trans(numnod, internal_node)
!
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint) :: iflag_shell_local
!
!
      iflag_shell_mode = 0
      if(internal_node .le. nnod_rtp) then
        iflag_shell_local = iflag_MESH_same
      else if(internal_node .eq. nnod_rtp+nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(internal_node .eq. nnod_rtp+2*nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(internal_node .eq. nnod_rtp+nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      else if(internal_node .eq. nnod_rtp+2*nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      end if
!
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_local',   &
     &     my_rank, iflag_shell_local, internal_node, nnod_rtp
      call MPI_allreduce(iflag_shell_local, iflag_shell_mode, ione,     &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_mode',    &
     &     my_rank, iflag_shell_mode
!
      if(iflag_shell_mode .ne. iflag_MESH_same) then
        nnod_rtp_pole = numnod
      end if
!
      end subroutine set_pole_flag_4_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_b_trans_pole_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ncomp
!
!
!
      v_n_pole =   zero
      v_s_pole =   zero
!
      ncomp = nb*nidx_rj(1)
      call MPI_allreduce(v_np_local, v_n_pole, ncomp,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allreduce(v_sp_local, v_s_pole, ncomp,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_b_trans_pole_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sum_b_trans_pole_vect(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ncomp
!
!
      v_n_pole =   zero
      v_s_pole =   zero
!
      ncomp = n_vector*nb*nidx_rj(1)
      call MPI_allreduce(v_np_local, v_n_pole, ncomp,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allreduce(v_sp_local, v_s_pole, ncomp,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_b_trans_pole_vect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_b_trans_center_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      v_center =   zero
!
      call MPI_allreduce(v_ct_local, v_center, nb,                      &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_b_trans_center_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sum_b_trans_center_vect(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ncomp
!
!
      v_center =   zero
!
      ncomp = n_vector*nb
      call MPI_allreduce(v_ct_local, v_center, ncomp,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine sum_b_trans_center_vect
!
! -----------------------------------------------------------------------
!
      end module sum_b_trans_at_pole
