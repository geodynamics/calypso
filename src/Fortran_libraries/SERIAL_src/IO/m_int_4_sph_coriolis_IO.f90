!m_int_4_sph_coriolis_IO.f90
!      module m_int_4_sph_coriolis_IO
!
!     Written by H. Matsui on March, 2010
!
!      subroutine allocate_int_sph_cor_IO
!      subroutine deallocate_int_sph_cor_IO
!
!      subroutine copy_int_sph_coriolis_from_IO(ltr, jmax)
!      subroutine copy_int_sph_coriolis_to_IO(ltr)
!
      module m_int_4_sph_coriolis_IO
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ltr_cor_IO, jmax_cor_IO
      real(kind = kreal), allocatable ::  gk_cor_IO(:,:,:)
      real(kind = kreal), allocatable ::  el_cor_IO(:,:,:)
!
!      gk_cor_IO(j3,j2,j1) = K(j1,j2,j3)
!      el_cor_IO(j3,j2,j1) = E(j1,j2,j3)
!
      integer(kind = kint), allocatable :: jgl_kcor_IO(:,:,:)
      integer(kind = kint), allocatable :: jgl_lcor_IO(:,:,:)
!
      character(len=kchara) :: sph_cor_file_name = 'rot_int.dat'
      integer(kind = kint) :: ifmt_cor_int_file = 0
      integer(kind = kint), parameter :: id_sph_cor = 9
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_sph_cor_IO
!
!
!
      jmax_cor_IO = ltr_cor_IO * (ltr_cor_IO + 2)
      allocate( gk_cor_IO(jmax_cor_IO,4,3) )
      allocate( el_cor_IO(jmax_cor_IO,2,3) )
      allocate( jgl_kcor_IO(jmax_cor_IO,4,3) )
      allocate( jgl_lcor_IO(jmax_cor_IO,2,3) )
!
      jgl_kcor_IO = 0
      jgl_lcor_IO = 0
      gk_cor_IO = 0.0d0
      el_cor_IO = 0.0d0
!
      end subroutine allocate_int_sph_cor_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_int_sph_cor_IO
!
!
      deallocate(gk_cor_IO, el_cor_IO)
      deallocate(jgl_kcor_IO, jgl_lcor_IO)
!
      end subroutine deallocate_int_sph_cor_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_int_sph_coriolis_from_IO(ltr, jmax)
!
      use m_integrals_4_sph_coriolis
!
      integer(kind = kint), intent(in) ::    ltr
      integer(kind = kint), intent(inout) :: jmax
      integer(kind = kint) :: jnod
!
!
      if(ltr.ne.ltr_cor_IO) write(*,*) 'integration data is not same'
      call allocate_int_sph_coriolis(jmax)
!
      do jnod = 1, jmax
!
        jgl_kcor(jnod,1,1) = jgl_kcor_IO(jnod,1,1)
        jgl_kcor(jnod,2,1) = jgl_kcor_IO(jnod,2,1)
        jgl_kcor(jnod,3,1) = jgl_kcor_IO(jnod,3,1)
        jgl_kcor(jnod,4,1) = jgl_kcor_IO(jnod,4,1)
!
        jgl_kcor(jnod,1,2) = jgl_kcor_IO(jnod,1,2)
        jgl_kcor(jnod,2,2) = jgl_kcor_IO(jnod,2,2)
!
        jgl_kcor(jnod,1,3) = jgl_kcor_IO(jnod,1,3)
        jgl_kcor(jnod,2,3) = jgl_kcor_IO(jnod,2,3)
        jgl_kcor(jnod,3,3) = jgl_kcor_IO(jnod,3,3)
        jgl_kcor(jnod,4,3) = jgl_kcor_IO(jnod,4,3)
!
        jgl_lcor(jnod,1,1) = jgl_lcor_IO(jnod,1,1)
        jgl_lcor(jnod,2,1) = jgl_lcor_IO(jnod,2,1)
!
        jgl_lcor(jnod,1,2) = jgl_lcor_IO(jnod,1,2)
!
        jgl_lcor(jnod,1,3) = jgl_lcor_IO(jnod,1,3)
        jgl_lcor(jnod,2,3) = jgl_lcor_IO(jnod,2,3)
!
!
        gk_cor(jnod,1,1) = gk_cor_IO(jnod,1,1)
        gk_cor(jnod,2,1) = gk_cor_IO(jnod,2,1)
        gk_cor(jnod,3,1) = gk_cor_IO(jnod,3,1)
        gk_cor(jnod,4,1) = gk_cor_IO(jnod,4,1)
!
        gk_cor(jnod,1,2) = gk_cor_IO(jnod,1,2)
        gk_cor(jnod,2,2) = gk_cor_IO(jnod,2,2)
!
        gk_cor(jnod,1,3) = gk_cor_IO(jnod,1,3)
        gk_cor(jnod,2,3) = gk_cor_IO(jnod,2,3)
        gk_cor(jnod,3,3) = gk_cor_IO(jnod,3,3)
        gk_cor(jnod,4,3) = gk_cor_IO(jnod,4,3)
!
        el_cor(jnod,1,1) = el_cor_IO(jnod,1,1)
        el_cor(jnod,2,1) = el_cor_IO(jnod,2,1)
!
        el_cor(jnod,1,2) = el_cor_IO(jnod,1,2)
!
        el_cor(jnod,1,3) = el_cor_IO(jnod,1,3)
        el_cor(jnod,2,3) = el_cor_IO(jnod,2,3)
      end do
!
      call deallocate_int_sph_cor_IO
!
      end subroutine copy_int_sph_coriolis_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_int_sph_coriolis_to_IO(ltr)
!
      use m_integrals_4_sph_coriolis
!
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint) :: jnod
!
!
      ltr_cor_IO = ltr
      call allocate_int_sph_cor_IO
!
      do jnod = 1, jmax_cor_IO
        jgl_kcor_IO(jnod,1,1) = jgl_kcor(jnod,1,1)
        jgl_kcor_IO(jnod,2,1) = jgl_kcor(jnod,2,1)
        jgl_kcor_IO(jnod,3,1) = jgl_kcor(jnod,3,1)
        jgl_kcor_IO(jnod,4,1) = jgl_kcor(jnod,4,1)
!
        jgl_kcor_IO(jnod,1,2) = jgl_kcor(jnod,1,2)
        jgl_kcor_IO(jnod,2,2) = jgl_kcor(jnod,2,2)
!
        jgl_kcor_IO(jnod,1,3) = jgl_kcor(jnod,1,3)
        jgl_kcor_IO(jnod,2,3) = jgl_kcor(jnod,2,3)
        jgl_kcor_IO(jnod,3,3) = jgl_kcor(jnod,3,3)
        jgl_kcor_IO(jnod,4,3) = jgl_kcor(jnod,4,3)
!
        jgl_lcor_IO(jnod,1,1) = jgl_lcor(jnod,1,1)
        jgl_lcor_IO(jnod,2,1) = jgl_lcor(jnod,2,1)
!
        jgl_lcor_IO(jnod,1,2) = jgl_lcor(jnod,1,2)
!
        jgl_lcor_IO(jnod,1,3) = jgl_lcor(jnod,1,3)
        jgl_lcor_IO(jnod,2,3) = jgl_lcor(jnod,2,3)
!
!
        gk_cor_IO(jnod,1,1) = gk_cor(jnod,1,1)
        gk_cor_IO(jnod,2,1) = gk_cor(jnod,2,1)
        gk_cor_IO(jnod,3,1) = gk_cor(jnod,3,1)
        gk_cor_IO(jnod,4,1) = gk_cor(jnod,4,1)
!
        gk_cor_IO(jnod,1,2) = gk_cor(jnod,1,2)
        gk_cor_IO(jnod,2,2) = gk_cor(jnod,2,2)
!
        gk_cor_IO(jnod,1,3) = gk_cor(jnod,1,3)
        gk_cor_IO(jnod,2,3) = gk_cor(jnod,2,3)
        gk_cor_IO(jnod,3,3) = gk_cor(jnod,3,3)
        gk_cor_IO(jnod,4,3) = gk_cor(jnod,4,3)
!
        el_cor_IO(jnod,1,1) = el_cor(jnod,1,1)
        el_cor_IO(jnod,2,1) = el_cor(jnod,2,1)
!
        el_cor_IO(jnod,1,2) = el_cor(jnod,1,2)
!
        el_cor_IO(jnod,1,3) = el_cor(jnod,1,3)
        el_cor_IO(jnod,2,3) = el_cor(jnod,2,3)
      end do
!
      end subroutine copy_int_sph_coriolis_to_IO
!
! -----------------------------------------------------------------------
!
      end module m_int_4_sph_coriolis_IO
