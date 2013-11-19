!>@file   const_coriolis_sph.f90
!!@brief  module const_coriolis_sph
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Evaluate Coriolis term
!!
!!
!!@verbatim
!!      subroutine init_sum_coriolis_sph
!!      subroutine sum_coriolis_rj_sph
!!@endverbatim
!
!
      module const_coriolis_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_physical_property
      use m_spherical_harmonics
      use m_int_4_sph_coriolis_IO
!
      use spherical_harmonics
!
      implicit none
!
!>     Integer flag to read integration data file
      integer(kind = kint) :: iflag_sph_coriolis_file = 0
!
      integer(kind = kint), allocatable, private  :: idx_gl(:,:)
      private :: set_global_sph_index_4_cor, dealloc_gl_sph_index_cor
      private :: set_tri_int_sph_coriolis
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sum_coriolis_sph
!
      use m_spheric_parameter
      use m_integrals_4_sph_coriolis
      use set_comm_tbl_sph_coriolis
      use set_integral_sph_coriolis
      use set_coriolis_tri_sph
!
!    load Gaunt integrals for Coriolis term
!
      if (iflag_debug.eq.1) write(*,*)  'set_tri_int_sph_coriolis'
      call set_tri_int_sph_coriolis
!
!    construct communication table for Coriolis term
!
      if (iflag_debug.eq.1) write(*,*)  'set_sr_address_4_coriolis'
      call set_sr_address_4_coriolis
!
!    construct integration for Coriolis term
!
      if (iflag_debug.eq.1) write(*,*)  'set_local_sph_coriolis_address'
      call set_local_sph_coriolis_address(jmax_tri_sph)
!
      call dealloc_gl_sph_index_cor
      call s_set_coriolis_tri_sph
      call deallocate_index_4_sph
!
      end subroutine init_sum_coriolis_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_coriolis_rj_sph
!
      use m_boundary_params_sph_MHD
      use trans_sph_velo_4_coriolis
      use sum_rot_coriolis_rj_sph
      use cal_inner_core_rotation
!
!
      if (iflag_debug.eq.1) write(*,*) 's_trans_sph_velo_4_coriolis'
      call s_trans_sph_velo_4_coriolis
!
      call s_sum_rot_coriolis_rj_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_cor)
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_coriolis_explicit(sph_bc_U%kr_in)
      end if
!
      end subroutine sum_coriolis_rj_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_index_4_cor(ltr)
!
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint) :: l, m, j
!
!
      allocate(idx_gl(jmax_tri_sph,3))
!
      do l = 1, ltr
        do m = -l, l
          j = l*(l+1) + m
          idx_gl(j,1) = j
          idx_gl(j,2) = l
          idx_gl(j,3) = m
        end do
      end do
!
      end subroutine set_global_sph_index_4_cor
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_gl_sph_index_cor
!
      deallocate(idx_gl)
!
      end subroutine dealloc_gl_sph_index_cor
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_tri_int_sph_coriolis
!
      use calypso_mpi
      use m_spheric_parameter
      use m_integrals_4_sph_coriolis
      use set_integral_sph_coriolis
      use sph_file_IO_select
!
!
      call allocate_index_4_sph(l_truncation)
      call idx28
!*
      call set_global_sph_index_4_cor(l_truncation)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &              'iflag_sph_coriolis_file', iflag_sph_coriolis_file
      if(iflag_sph_coriolis_file .gt. 0) then
        call sel_read_int_4_sph_coriolis
        if(iflag_debug.eq.1) write(*,*) 'copy_int_sph_coriolis_from_IO'
        call copy_int_sph_coriolis_from_IO(l_truncation, jmax_tri_sph)
      else
        call allocate_int_sph_coriolis(jmax_tri_sph)
!
        call copy_global_sph_id_4_sph_cor(jmax_tri_sph, idx_gl)
        if (iflag_debug.eq.1) write(*,*)  's_set_integral_sph_coriolis'
        call s_set_integral_sph_coriolis(l_truncation, jmax_tri_sph,    &
     &      idx_gl(1,1) )
      end if
!
      end subroutine set_tri_int_sph_coriolis
!
!  ---------------------------------------------------------------------
!
      end module  const_coriolis_sph
