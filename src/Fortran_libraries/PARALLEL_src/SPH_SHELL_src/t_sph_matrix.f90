!>@file   t_sph_matrix.f90
!!@brief  module t_sph_matrix
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief structures of radial matrix
!!
!!@verbatim
!!      subroutine alloc_single_band_mat(nband, sph_rj, smat)
!!      subroutine set_unit_single_mat(smat)
!!      subroutine dealloc_band_matrix(smat)
!!
!!      subroutine ludcmp_3band_type(smat)
!!      subroutine ludcmp_5band_type(smat)
!!      subroutine ludcmp_7band_type(smat)
!!
!!      subroutine lubksb_3band_type(smat, x)
!!      subroutine lubksb_5band_type(smat, x)
!!      subroutine lubksb_7band_type(smat, x)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(band_matrix_type), intent(inout) :: smat
!!
!!      subroutine check_radial_band_matrix(my_rank, sph_rj, smat)
!!@endverbatim
!
      module t_sph_matrix
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>      Structure for band matrices
      type band_matrix_type
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
!>        Band matrices for spectr data
        real(kind = kreal), allocatable :: mat(:,:)
!
!>        LU-decompositted matrices for spectr data
        real(kind = kreal), allocatable :: lu(:,:)
!
!>        Determinant of matrices for spectr data
        real(kind = kreal) :: det
!
!>        Pivot information for matrices for spectr data
        integer(kind = kint), allocatable :: i_pivot(:)
      end type band_matrix_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_single_band_mat(nband, sph_rj, smat)
!
      use t_spheric_rj_data
!
      integer(kind = kint), intent(in) :: nband
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(band_matrix_type), intent(inout) :: smat
!
!
      smat%n_vect =      sph_rj%nidx_rj(1)
      smat%n_band =      nband
      smat%n_band_lu = 2*nband - 1
!
!
      allocate( smat%mat(smat%n_band,smat%n_vect) )
      allocate( smat%lu(smat%n_band_lu,smat%n_vect) )
      allocate( smat%i_pivot(smat%n_vect) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      end subroutine alloc_single_band_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_single_mat(smat)
!
      type(band_matrix_type), intent(inout) :: smat
!
      integer(kind = kint) :: l_diag
!
!
      l_diag = (smat%n_band + 1) / 2
      smat%mat(l_diag,1:smat%n_vect) = 1.0d0
!
      end subroutine set_unit_single_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_band_matrix(smat)
!
      type(band_matrix_type), intent(inout) :: smat
!
!
      deallocate( smat%mat, smat%lu, smat%i_pivot )
!
      end subroutine dealloc_band_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ludcmp_3band_type(smat)
!
      use m_ludcmp_3band
!
      type(band_matrix_type), intent(inout) :: smat
      integer(kind = kint) :: ierr_MP
!
!
      call ludcmp_3band(smat%n_vect, smat%mat,smat%i_pivot, ierr_MP,    &
     &    smat%lu, smat%det)
!
      end subroutine ludcmp_3band_type
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_5band_type(smat)
!
      use m_ludcmp_band
!
      type(band_matrix_type), intent(inout) :: smat
!
!
      call ludcmp_band(smat%n_vect, ifive, smat%mat, smat%lu,           &
     &    smat%i_pivot, smat%det)
!
      end subroutine ludcmp_5band_type
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_7band_type(smat)
!
      use m_ludcmp_band
!
      type(band_matrix_type), intent(inout) :: smat
!
!
      call ludcmp_band(smat%n_vect, iseven, smat%mat, smat%lu,          &
     &    smat%i_pivot, smat%det)
!
      end subroutine ludcmp_7band_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine lubksb_3band_type(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_vect)
!
!
      call lubksb_3band(smat%n_vect, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_3band_type
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band_type(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_vect)
!
!
      call lubksb_5band(smat%n_vect, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_5band_type
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band_type(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(smat%n_vect)
!
!
      call lubksb_7band(smat%n_vect, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_7band_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_radial_band_matrix(my_rank, sph_rj, smat)
!
      use t_spheric_rj_data
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(band_matrix_type), intent(in) :: smat
!
!
      if(smat%n_band .eq. ithree) then
        call check_single_radial_3band_mat(my_rank, smat%n_vect,        &
     &      sph_rj%radius_1d_rj_r, smat%mat)
!
      else if(smat%n_band .eq. ifive) then
        call check_single_radial_5band_mat(my_rank, smat%n_vect,        &
     &      sph_rj%radius_1d_rj_r, smat%mat)
!
      else if(smat%n_band .eq. iseven) then
        call check_single_radial_7band_mat(my_rank, smat%n_vect,        &
     &      sph_rj%radius_1d_rj_r, smat%mat)
      end if
!
!
      end subroutine check_radial_band_matrix
!
! -----------------------------------------------------------------------
!
      end module t_sph_matrix
