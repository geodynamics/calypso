!pole_sph_transform.f90
!      module pole_sph_transform
!
!     Written by H. Matsui on June, 2012
!
!      subroutine pole_b_trans_scalar(nb)
!      subroutine pole_b_trans_vector(nb)
!      subroutine pole_b_trans_tensor(nb)
!
      module pole_sph_transform
!
      use m_precision
!
      use m_parallel_var_dof
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
      subroutine pole_b_trans_scalar(nb)
!
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: nb
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      if (iflag_debug.gt.0)  write(*,*) 'sph_b_trans_scalar', nb
      call schmidt_b_trans_pole_scalar(nb)
      call sum_b_trans_pole_scalar(nb)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_scalar(nb)
        call sum_b_trans_center_scalar(nb)
      end if
!
      end subroutine pole_b_trans_scalar
!
! -----------------------------------------------------------------------
!
      subroutine pole_b_trans_vector(nb)
!
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: nb
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      if (iflag_debug.gt.0)  write(*,*) 'sph_b_trans_vector', nb
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
      subroutine pole_b_trans_tensor(nb)
!
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: num
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      num = n_sym_tensor * nb
!
      if (iflag_debug.gt.0) write(*,*) 'sph_b_trans_tensor', nb
      call schmidt_b_trans_pole_scalar(num)
      call sum_b_trans_pole_scalar(num)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_scalar(num)
        call sum_b_trans_center_scalar(num)
      end if
!
      end subroutine pole_b_trans_tensor
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform

