!>@file   t_radial_matrices_sph_MHD.f90
!!@brief  module t_radial_matrices_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief Radial band matrix for time evolutions
!!
!!@verbatim
!!      subroutine alloc_average_w_center(sph_rj, sph_MHD_mat)
!!      subroutine dealloc_average_w_center(sph_MHD_mat)
!!
!!      subroutine allocate_press_vpol_mat_sph(sph_rj, sph_MHD_mat)
!!      subroutine check_velocity_matrices_sph                          &
!!     &         (id_rank, sph_rj, sph_MHD_mat)
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!@endverbatim
!!
      module t_radial_matrices_sph_MHD
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_matrix
!
      implicit none
!
!>        Structure of band matrices for dynamo simulation
      type MHD_radial_matrices
!>        Structure of band matrices for poloidal velocity
        type(band_matrices_type) :: band_vp_evo
!>        Structure of band matrices for toroidal velocity
        type(band_matrices_type) :: band_vt_evo
!>        Structure of band matrices for toroidal vorticity
        type(band_matrices_type) :: band_wt_evo
!>        Structure of band matrices for poloidal velocity poisson
        type(band_matrices_type) :: band_vs_poisson
!>        Structure of band matrices for pressure poisson
        type(band_matrices_type) :: band_p_poisson
!
!>        Structure of band matrices for poloidal velocity
        type(band_matrices_type) :: band_vsp_evo
!
!>        Structure of band matrices for poloidal magnetic field
        type(band_matrices_type) :: band_bp_evo
!>        Structure of band matrices for toroidal magnetic field
        type(band_matrices_type) :: band_bt_evo
!
!>        Structure of band matrices for temperature
        type(band_matrices_type) :: band_temp_evo
!>        Structure of band matrices for composition
        type(band_matrices_type) :: band_comp_evo
!
!
!>        Structure of band matrices for pressure poisson
        type(band_matrix_type) :: band_p00_poisson
!>        Structure of band matrices for pressure poisson
        type(band_matrix_type) :: band_temp00_evo
!>        Structure of band matrices for pressure poisson
        type(band_matrix_type) :: band_comp00_evo
!
!>       Temporal space for average with center
        real(kind = kreal), allocatable :: x00_w_center(:)
      end type MHD_radial_matrices
!
      private :: alloc_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_press_vpol_mat_sph(sph_rj, smat)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(inout) :: smat
!
      smat%n_vect =    2*sph_rj%nidx_rj(1)
      smat%n_comp =      sph_rj%nidx_rj(2)
      smat%n_band =      iseven
      smat%n_band_lu = 2*smat%n_band - 1
!
!
      allocate( smat%mat(smat%n_band,smat%n_vect,smat%n_comp) )
      allocate( smat%lu(smat%n_band_lu ,smat%n_vect,smat%n_comp) )
      allocate( smat%det(smat%n_comp) )
      allocate( smat%i_pivot(smat%n_vect,smat%n_comp) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      smat%mat(4,1:2*smat%n_vect,1:smat%n_comp) = 1.0d0
!
      end subroutine alloc_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_average_w_center(sph_rj, sph_MHD_mat)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
!
      allocate( sph_MHD_mat%x00_w_center(0:sph_rj%nidx_rj(1)) )
      if(sph_rj%nidx_rj(1) .gt. 0) sph_MHD_mat%x00_w_center = 0.0d0
!
      end subroutine alloc_average_w_center
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_average_w_center(sph_MHD_mat)
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
!
      deallocate( sph_MHD_mat%x00_w_center )
!
      end subroutine dealloc_average_w_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_press_vpol_mat_sph(sph_rj, sph_MHD_mat)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
!
      call alloc_press_vpol_mat_sph(sph_rj, sph_MHD_mat%band_vsp_evo)
!
      end subroutine allocate_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_velocity_matrices_sph                            &
     &         (id_rank, sph_rj, sph_MHD_mat)
!
      use check_sph_radial_mat
!
      integer, intent(in) :: id_rank
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_radial_matrices), intent(in) :: sph_MHD_mat
!
      real(kind = kreal) :: rr(2*sph_rj%nidx_rj(1))
      integer(kind = kint) :: k
!
!
      do k = 1, sph_rj%nidx_rj(1)
        rr(k) = sph_rj%radius_1d_rj_r(k)
        rr(sph_rj%nidx_rj(1)+k) = sph_rj%radius_1d_rj_r(k)
      end do
!
      call check_radial_7band_mat                                       &
     &   (id_rank, (2*sph_rj%nidx_rj(1)), sph_rj%nidx_rj(2),            &
     &    sph_rj%idx_gl_1d_rj_j, rr, sph_MHD_mat%band_vsp_evo%mat)
!
      call check_radial_band_mat                                        &
     &   (id_rank, sph_rj, sph_MHD_mat%band_vt_evo)
!
      end subroutine check_velocity_matrices_sph
!
! -----------------------------------------------------------------------
!
      end module t_radial_matrices_sph_MHD
