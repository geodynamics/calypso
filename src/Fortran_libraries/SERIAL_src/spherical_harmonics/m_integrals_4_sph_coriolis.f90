!m_integrals_4_sph_coriolis.f90
!      module m_integrals_4_sph_coriolis
!
!     Written by H. Matsui on March, 2010
!
!      subroutine allocate_int_sph_coriolis(jmax)
!      subroutine deallocate_int_sph_coriolis
!
!      subroutine copy_global_sph_id_4_sph_cor(jmax, idx_j)
!
      module m_integrals_4_sph_coriolis
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable ::  gk_cor(:,:,:)
      real(kind = kreal), allocatable ::  el_cor(:,:,:)
!
      integer(kind = kint), allocatable :: jgl_kcor(:,:,:)
      integer(kind = kint), allocatable :: jgl_lcor(:,:,:)
      integer(kind = kint), allocatable :: jlc_kcor(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_sph_coriolis(jmax)
!
      integer(kind = kint), intent(in) :: jmax
!
!
      allocate( gk_cor(jmax,4,3) )
      allocate( el_cor(jmax,2,3) )
      allocate( jgl_kcor(jmax,4,3) )
      allocate( jgl_lcor(jmax,2,3) )
!
      jgl_kcor = 0
      jgl_lcor = 0
      gk_cor = 0.0d0
      el_cor = 0.0d0
!
      end subroutine allocate_int_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_int_sph_coriolis
!
!
      deallocate(gk_cor, el_cor)
      deallocate(jgl_kcor, jgl_lcor)
!
      end subroutine deallocate_int_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_global_sph_id_4_sph_cor(jmax, idx_j)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_j(jmax)
      integer(kind = kint) :: jnod
!
!
      do jnod = 1, jmax
        jgl_kcor(jnod,1,1) = idx_j(jnod)
        jgl_kcor(jnod,2,1) = idx_j(jnod)
        jgl_kcor(jnod,3,1) = idx_j(jnod)
        jgl_kcor(jnod,4,1) = idx_j(jnod)
        jgl_kcor(jnod,1,2) = idx_j(jnod)
        jgl_kcor(jnod,2,2) = idx_j(jnod)
        jgl_kcor(jnod,1,3) = idx_j(jnod)
        jgl_kcor(jnod,2,3) = idx_j(jnod)
        jgl_kcor(jnod,3,3) = idx_j(jnod)
        jgl_kcor(jnod,4,3) = idx_j(jnod)
!
        jgl_lcor(jnod,1,1) = idx_j(jnod)
        jgl_lcor(jnod,2,1) = idx_j(jnod)
        jgl_lcor(jnod,1,2) = idx_j(jnod)
        jgl_lcor(jnod,1,3) = idx_j(jnod)
        jgl_lcor(jnod,2,3) = idx_j(jnod)
      end do
!
      end subroutine copy_global_sph_id_4_sph_cor
!
! -----------------------------------------------------------------------
!
      end module m_integrals_4_sph_coriolis
