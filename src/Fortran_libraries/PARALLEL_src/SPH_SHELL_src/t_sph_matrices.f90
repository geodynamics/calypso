!>@file   t_sph_matrices.f90
!!@brief  module t_sph_matrices
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief structures of radial matrices
!!
!!@verbatim
!!      subroutine alloc_band_mat_sph(nband, sph_rj, smat)
!!      subroutine set_unit_on_diag(smat)
!!      subroutine dealloc_band_mat_sph(smat)
!!
!!      subroutine ludcmp_3band_mul_t(Msmp, Msmp_stack, smat)
!!      subroutine ludcmp_5band_mul_t(Msmp, Msmp_stack, smat)
!!      subroutine ludcmp_7band_mul_t(Msmp, Msmp_stack, smat)
!!
!!      subroutine lubksb_3band_mul_t(Msmp, Msmp_stack, smat, x)
!!      subroutine lubksb_5band_mul_t(Msmp, Msmp_stack, smat, x)
!!      subroutine lubksb_7band_mul_t(Msmp, Msmp_stack, smat, x)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(band_matrices_type), intent(inout) :: smat
!!
!!      subroutine check_radial_band_mat(my_rank, sph_rj, smat)
!!@endverbatim
!
      module t_sph_matrices
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>      Structure for band matrices
      type band_matrices_type
!>        name of matrices
        character(len = kchara) :: mat_name
!
!>        Length of matrices for spectr data
        integer(kind = kint) :: n_vect
!
!>        Band width of matrices for spectr data
        integer(kind = kint) :: n_band
!
!>        Band width of LU-decompositted matrices for spectr data
        integer(kind = kint) :: n_band_lu
!
!>        number of components of matrices for spectr data
        integer(kind = kint) :: n_comp
!
!>        Band matrices for spectr data
        real(kind = kreal), allocatable :: mat(:,:,:)
!
!>        LU-decompositted matrices for spectr data
        real(kind = kreal), allocatable :: lu(:,:,:)
!
!>        Determinant of matrices for spectr data
        real(kind = kreal), allocatable :: det(:)
!
!>        Pivot information for matrices for spectr data
        integer(kind = kint), allocatable :: i_pivot(:,:)
      end type band_matrices_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_band_mat_sph(nband, sph_rj, smat)
!
      use t_spheric_rj_data
!
      integer(kind = kint), intent(in) :: nband
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(band_matrices_type), intent(inout) :: smat
!
!
      smat%n_vect =      sph_rj%nidx_rj(1)
      smat%n_comp =      sph_rj%nidx_rj(2)
      smat%n_band =      nband
      smat%n_band_lu = 2*nband - 1
!
!
      allocate( smat%mat(smat%n_band,smat%n_vect,smat%n_comp) )
      allocate( smat%lu(smat%n_band_lu,smat%n_vect,smat%n_comp) )
      allocate( smat%det(smat%n_comp) )
      allocate( smat%i_pivot(smat%n_vect,smat%n_comp) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      end subroutine alloc_band_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_on_diag(smat)
!
      type(band_matrices_type), intent(inout) :: smat
!
      integer(kind = kint) :: l_diag
!
!
      l_diag = (smat%n_band + 1) / 2
      smat%mat(l_diag,1:smat%n_vect,1:smat%n_comp) = 1.0d0
!
      end subroutine set_unit_on_diag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_band_mat_sph(smat)
!
      type(band_matrices_type), intent(inout) :: smat
!
!
      deallocate( smat%mat, smat%lu )
      deallocate( smat%det, smat%i_pivot )
!
      end subroutine dealloc_band_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ludcmp_3band_mul_t(Msmp, Msmp_stack, smat)
!
      use ludcmp_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(inout) :: smat
!
!
      call ludcmp_3band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%mat, smat%lu, smat%i_pivot, smat%det)
!
      end subroutine ludcmp_3band_mul_t
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_5band_mul_t(Msmp, Msmp_stack, smat)
!
      use ludcmp_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(inout) :: smat
!
!
      call ludcmp_5band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%mat, smat%lu, smat%i_pivot, smat%det)
!
      end subroutine ludcmp_5band_mul_t
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_7band_mul_t(Msmp, Msmp_stack, smat)
!
      use ludcmp_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(inout) :: smat
!
!
      call ludcmp_7band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%mat, smat%lu, smat%i_pivot, smat%det)
!
      end subroutine ludcmp_7band_mul_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine lubksb_3band_mul_t(Msmp, Msmp_stack, smat, x)
!
      use lubksb_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_comp,smat%n_vect)
!
!
      call lubksb_3band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_3band_mul_t
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band_mul_t(Msmp, Msmp_stack, smat, x)
!
      use lubksb_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_comp,smat%n_vect)
!
!
      call lubksb_5band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_5band_mul_t
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band_mul_t(Msmp, Msmp_stack, smat, x)
!
      use lubksb_357band_mul
!
      integer (kind = kint), intent(in) :: Msmp
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      type(band_matrices_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_comp,smat%n_vect)
!
!
      call lubksb_7band_mul(Msmp, Msmp_stack, smat%n_comp, smat%n_vect, &
     &    smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_7band_mul_t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_radial_band_mat(my_rank, sph_rj, smat)
!
      use t_spheric_rj_data
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(band_matrices_type), intent(in) :: smat
!
!
      write(50+my_rank,'(a,a)') 'Matrix ', trim(smat%mat_name)
!
      if(smat%n_band .eq. ithree) then
        call check_radial_3band_mat(my_rank, smat%n_vect, smat%n_comp,  &
     &      sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r, smat%mat)
!
      else if(smat%n_band .eq. ifive) then
        call check_radial_5band_mat(my_rank, smat%n_vect, smat%n_comp,  &
     &      sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r, smat%mat)
!
      else if(smat%n_band .eq. iseven) then
        call check_radial_7band_mat(my_rank, smat%n_vect, smat%n_comp,  &
     &      sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r, smat%mat)
      end if
!
!
      end subroutine check_radial_band_mat
!
! -----------------------------------------------------------------------
!
      end module t_sph_matrices
