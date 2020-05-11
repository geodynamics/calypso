!>@file   t_sph_center_matrix.f90
!!@brief  module t_sph_center_matrix
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief structures of radial matrix
!!
!!@verbatim
!!      subroutine alloc_ctr_band_mat(nband, sph_rj, smat)
!!      subroutine set_unit_ctr_single_mat(smat)
!!
!!      subroutine ludcmp_3band_ctr(smat)
!!      subroutine ludcmp_5band_ctr(smat)
!!      subroutine ludcmp_7band_ctr(smat)
!!
!!      subroutine lubksb_3band_ctr(smat, x)
!!      subroutine lubksb_5band_ctr(smat, x)
!!      subroutine lubksb_7band_ctr(smat, x)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(band_matrix_type), intent(inout) :: smat
!!
!!      subroutine check_center_band_matrix(id_rank, sph_rj, smat)
!!@endverbatim
!
      module t_sph_center_matrix
!
      use m_precision
      use m_machine_parameter
      use t_sph_matrix
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ctr_band_mat(nband, sph_rj, smat)
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
      allocate( smat%mat(smat%n_band,0:smat%n_vect) )
      allocate( smat%lu(smat%n_band_lu,0:smat%n_vect) )
      allocate( smat%i_pivot(0:smat%n_vect) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      end subroutine alloc_ctr_band_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_ctr_single_mat(smat)
!
      type(band_matrix_type), intent(inout) :: smat
!
      integer(kind = kint) :: l_diag
!
!
      l_diag = (smat%n_band + 1) / 2
      smat%mat(l_diag,0:smat%n_vect) = 1.0d0
!
      end subroutine set_unit_ctr_single_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ludcmp_3band_ctr(smat)
!
      use m_ludcmp_3band
!
      type(band_matrix_type), intent(inout) :: smat
      integer(kind = kint) :: n_vect0, ierr_MP
!
!
      n_vect0 = smat%n_vect + 1
      call ludcmp_3band(n_vect0, smat%mat,smat%i_pivot, ierr_MP,        &
     &    smat%lu, smat%det)
!
      end subroutine ludcmp_3band_ctr
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_5band_ctr(smat)
!
      use m_ludcmp_band
!
      type(band_matrix_type), intent(inout) :: smat
      integer(kind = kint) :: n_vect0
!
!
      n_vect0 = smat%n_vect + 1
      call ludcmp_band(n_vect0, ifive, smat%mat, smat%lu,               &
     &    smat%i_pivot, smat%det)
!
      end subroutine ludcmp_5band_ctr
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_7band_ctr(smat)
!
      use m_ludcmp_band
!
      type(band_matrix_type), intent(inout) :: smat
      integer(kind = kint) :: n_vect0
!
!
      n_vect0 = smat%n_vect + 1
      call ludcmp_band(n_vect0, iseven, smat%mat, smat%lu,              &
     &    smat%i_pivot, smat%det)
!
      end subroutine ludcmp_7band_ctr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine lubksb_3band_ctr(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(0:smat%n_vect)
!
      integer(kind = kint) :: n_vect0
!
!
      n_vect0 = smat%n_vect + 1
      call lubksb_3band(n_vect0, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_3band_ctr
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band_ctr(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(0:smat%n_vect)
!
      integer(kind = kint) :: n_vect0
!
!
      n_vect0 = smat%n_vect + 1
      call lubksb_5band(n_vect0, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_5band_ctr
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band_ctr(smat, x)
!
      use lubksb_357band
!
      type(band_matrix_type), intent(in) :: smat
!
      real(kind = kreal), intent(inout) :: x(0:smat%n_vect)
!
      integer(kind = kint) :: n_vect0
!
!
      n_vect0 = smat%n_vect + 1
      call lubksb_7band(n_vect0, smat%lu, smat%i_pivot, x)
!
      end subroutine lubksb_7band_ctr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_center_band_matrix(id_rank, sph_rj, smat)
!
      use t_spheric_rj_data
      use check_sph_radial_mat
!
      integer, intent(in) :: id_rank
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(band_matrix_type), intent(in) :: smat
!
!
      write(50+id_rank,'(a)')  'Matrix for ', trim(smat%mat_name)
      call check_radial_3band_mat_w_ctr(id_rank, smat%n_vect,           &
     &    sph_rj%radius_1d_rj_r, smat%mat)
!
      end subroutine check_center_band_matrix
!
! -----------------------------------------------------------------------
!
      end module t_sph_center_matrix
