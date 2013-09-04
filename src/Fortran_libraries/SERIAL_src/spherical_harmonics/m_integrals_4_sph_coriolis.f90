!m_integrals_4_sph_coriolis.f90
!      module m_integrals_4_sph_coriolis
!
!     Written by H. Matsui on March, 2010
!
!      subroutine allocate_sph_coriolis_data(nri, jmax)
!      subroutine allocate_int_sph_coriolis(jmax)
!      subroutine deallocate_sph_coriolis_data
!      subroutine deallocate_int_sph_coriolis
!
!      subroutine copy_global_sph_id_4_sph_cor(jmax, idx_j)
!      subroutine set_local_sph_coriolis_address(jmax)
!
!      subroutine check_int_4_sph_coriolis(id_file, jmax, idx_j)
!      subroutine check_idx_4_sph_coriolis(id_file)
!
      module m_integrals_4_sph_coriolis
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nidx_j_cor, nshift_j_cor
      integer(kind = kint), allocatable :: idx_gl_cor_j(:,:)
      real(kind = kreal), allocatable :: g_cor_j(:,:)
!
      real(kind = kreal), allocatable ::  gk_cor(:,:,:)
      real(kind = kreal), allocatable ::  el_cor(:,:,:)
!
!      gk_cor(j3,j2,j1) = K(j1,j2,j3)
!      el_cor(j3,j2,j1) = E(j1,j2,j3)
!
      integer(kind = kint) :: numdir_sph_cor = 5
      real(kind = kreal), allocatable :: d_sph_cor(:,:)
      real(kind = kreal), allocatable :: d_rj_cor(:,:)
!
      integer(kind = kint), allocatable :: jgl_kcor(:,:,:)
      integer(kind = kint), allocatable :: jgl_lcor(:,:,:)
      integer(kind = kint), allocatable :: jlc_kcor(:,:,:)
      integer(kind = kint), allocatable :: jlc_lcor(:,:,:)
!
      integer(kind = kint), parameter :: ic_vp =   1
      integer(kind = kint), parameter :: ic_dvp =  2
      integer(kind = kint), parameter :: ic_d2vp = 3
      integer(kind = kint), parameter :: ic_vt =   4
      integer(kind = kint), parameter :: ic_dvt =  5
      integer(kind = kint), parameter :: ic_d2vt = 6
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_coriolis_data(nri, jmax)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
!
      allocate( idx_gl_cor_j(nidx_j_cor,3) )
      allocate( g_cor_j(nidx_j_cor,3) )
!
      allocate( d_sph_cor(nidx_j_cor*nri,numdir_sph_cor) )
      allocate( d_rj_cor(jmax*nri,numdir_sph_cor) )
      idx_gl_cor_j = 0
      g_cor_j = 0.0d0
      d_sph_cor = 0.0d0
      d_rj_cor = 0.0d0
!
      end subroutine allocate_sph_coriolis_data
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
      allocate( jlc_kcor(jmax,4,3) )
      allocate( jlc_lcor(jmax,2,3) )
!
      jgl_kcor = 0
      jgl_lcor = 0
      jlc_kcor = 0
      jlc_lcor = 0
      gk_cor = 0.0d0
      el_cor = 0.0d0
!
      end subroutine allocate_int_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_coriolis_data
!
!
      deallocate( idx_gl_cor_j, g_cor_j )
      deallocate( d_sph_cor, d_rj_cor )
!
      end subroutine deallocate_sph_coriolis_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_int_sph_coriolis
!
!
      deallocate(gk_cor, el_cor)
      deallocate(jgl_kcor, jgl_lcor)
      deallocate(jlc_kcor, jlc_lcor)
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
      subroutine set_local_sph_coriolis_address(jmax)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint) :: jnod, jshift
!
!
      do jnod = 1, jmax
        jshift = -idx_gl_cor_j(1,1) + 1
        jlc_kcor(jnod,1,2) = jgl_kcor(jnod,1,2) + jshift
        jlc_kcor(jnod,2,2) = jgl_kcor(jnod,2,2) + jshift
        jlc_lcor(jnod,1,2) = jgl_lcor(jnod,1,2) + jshift
!
        jlc_kcor(jnod,1,1) = jgl_kcor(jnod,1,1) + jshift
        jlc_kcor(jnod,2,1) = jgl_kcor(jnod,2,1) + jshift
        jlc_kcor(jnod,3,1) = jgl_kcor(jnod,3,1) + jshift
        jlc_kcor(jnod,4,1) = jgl_kcor(jnod,4,1) + jshift
        jlc_lcor(jnod,1,1) = jgl_lcor(jnod,1,1) + jshift
        jlc_lcor(jnod,2,1) = jgl_lcor(jnod,2,1) + jshift
!
        jlc_kcor(jnod,1,3) = jgl_kcor(jnod,1,3) + jshift
        jlc_kcor(jnod,2,3) = jgl_kcor(jnod,2,3) + jshift
        jlc_kcor(jnod,3,3) = jgl_kcor(jnod,3,3) + jshift
        jlc_kcor(jnod,4,3) = jgl_kcor(jnod,4,3) + jshift
        jlc_lcor(jnod,1,3) = jgl_lcor(jnod,1,3) + jshift
        jlc_lcor(jnod,2,3) = jgl_lcor(jnod,2,3) + jshift
      end do
!
      end subroutine set_local_sph_coriolis_address
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_int_4_sph_coriolis(id_file, jmax, idx_j)
!
      integer(kind = kint), intent(in) :: id_file, jmax
      integer(kind = kint), intent(in) :: idx_j(jmax)
      integer(kind = kint) :: j, j1, j2, jc
!
      j1 = 2
      write(id_file,'(a)') 'j3, j1, j2_lc, j3_lc, j2_gl, j3_gl, Ki/pi'
      do j = 1 ,jmax
        jc = j + nshift_j_cor
        do j2 = 1, 2
          write(id_file,'(6i6,1pE25.15e3)') j, j1, jlc_kcor(j,j2,j1),   &
     &                jc, jgl_kcor(j,j2,j1), idx_j(j), gk_cor(j,j2,j1)
        end do
      end do
      write(id_file,'(a)') 'j3, j1, j2_lc, j3_lc, j2_gl, j3_gl, Li/pi'
      do j = 1 ,jmax
        jc = j + nshift_j_cor
        write(id_file,'(6i6,1pE25.15e3)') j, j1, jlc_lcor(j,1,j1),      &
     &                jc, jgl_lcor(j,1,j1), idx_j(j), el_cor(j,1,j1)
      end do
!*
!
      do j1 = 1, 3, 2
        write(id_file,'(a)')'j3, j1, j2_lc, j3_lc, j2_gl, j3_gl, Ki/pi'
        do j = 1 ,jmax
          jc = j + nshift_j_cor
          do j2 = 1, 4
            write(id_file,'(6i6,1pE25.15e3)') j, j1, jlc_kcor(j,j2,j1), &
     &                jc, jgl_kcor(j,j2,j1), idx_j(j), gk_cor(j,j2,j1)
          end do
        end do
!*
        write(id_file,'(a)')'j3, j1, j2_lc, j3_lc, j2_gl, j3_gl, Li/pi'
        do j = 1, jmax
          jc = j + nshift_j_cor
          do j2 = 1, 2
            write(id_file,'(6i6,1pE25.15e3)') j, j1, jlc_lcor(j,j2,j1), &
     &                jc, jgl_lcor(j,j2,j1), idx_j(j), el_cor(j,j2,j1)
          end do
        end do
      end do
!
      end subroutine check_int_4_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine check_idx_4_sph_coriolis(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inum
!
!
      write(id_file,*) 'idx_gl_cor_j address', nidx_j_cor
      do inum = 1, nidx_j_cor
        write(id_file,*) inum, idx_gl_cor_j(inum,1:3),  g_cor_j(inum,3)
      end do
!
      end subroutine check_idx_4_sph_coriolis
!
! -----------------------------------------------------------------------
!
      end module m_integrals_4_sph_coriolis
