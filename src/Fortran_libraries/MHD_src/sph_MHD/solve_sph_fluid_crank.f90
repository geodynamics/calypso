!>@file   solve_sph_fluid_crank.f90
!!@brief  module solve_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine solve_velo_by_vort_sph_crank                         &
!!     &         (sph_rj, band_vp_evo, band_vt_evo,                     &
!!     &          is_velo, it_velo, n_point, ntot_phys_rj, d_rj)
!!        Input address:    is_velo, it_velo
!!        Solution address: is_velo, it_velo
!!
!!      subroutine solve_pressure_by_div_v(sph_rj, band_p_poisson,      &
!!     &          is_press, n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine solve_magne_sph_crank                                &
!!     &         (sph_rj, band_bp_evo, band_bt_evo, is_magne, it_magne, &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        Input address:    is_magne, it_magne
!!        Solution address: is_magne, it_magne
!!
!!      subroutine solve_scalar_sph_crank                               &
!!     &         (sph_rj, band_s_evo, band_s00_evo, is_field,           &
!!     &          n_point, ntot_phys_rj, d_rj, sol_00)
!!        Input address:    is_field
!!        Solution address: is_field
!!@endverbatim
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module solve_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_spheric_rj_data
      use t_sph_matrices
!
      use set_reference_sph_mhd
      use lubksb_357band_mul
!
      implicit none
!
      private :: check_scalar_coefs, check_NaN_temperature
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine solve_velo_by_vort_sph_crank                           &
     &         (sph_rj, band_vp_evo, band_vt_evo,                       &
     &          is_velo, it_velo, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo, it_velo
      type(band_matrices_type), intent(in) :: band_vp_evo
      type(band_matrices_type), intent(in) :: band_vt_evo
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!      integer(kind = kint) :: inod, k, j
!
!
!      write(my_rank+70,*) 'k, j, inod, vp_rhs, vt_rhs'
!      do j = 1, sph_rj%nidx_rj(2)
!        j = 3
!        do k = 1, sph_bc_U%kr_out
!          inod = (k-1) * sph_rj%nidx_rj(2) + j
!          write(my_rank+70,*) k, j, inod,                              &
!     &                 d_rj(inod,is_velo), d_rj(inod,it_velo)
!        end do
!      end do
!
      call lubksb_5band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vp_evo, d_rj(1,is_velo))
!
      call lubksb_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vt_evo, d_rj(1,it_velo))
!
!
      end subroutine solve_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine solve_pressure_by_div_v(sph_rj, band_p_poisson,        &
     &          is_press, n_point, ntot_phys_rj, d_rj)
!
      use check_sph_radial_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(in) :: band_p_poisson
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_press
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: j
      integer, parameter :: id_offset = 100
      integer(kind = kint), parameter :: id_file = 50 + id_offset
!
!
      j = find_local_sph_address(sph_rj, 0, 0)
      if(i_debug*j .gt. 0) then
        open(id_file,file='ave_press_test.txt')
        write(id_file,*) 'Matrix for average pressure'
        call check_single_radial_3band_mat                              &
     &     (id_offset, sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,        &
     &      band_p_poisson%mat(1:band_p_poisson%n_band,                 &
     &                         1:band_p_poisson%n_vect,j))
        write(id_file,*) 'LU decomposition for average pressure'
        call check_single_radial_5band_mat                              &
     &     (id_offset, sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,        &
     &      band_p_poisson%lu(1:band_p_poisson%n_band_lu,               &
     &                        1:band_p_poisson%n_vect,j))
        write(id_file,*) 'RHS for average pressure'
        call check_scalar_coefs(id_file, izero, izero, sph_rj,          &
     &      is_press, n_point, ntot_phys_rj, d_rj)
      end if
!
      call lubksb_3band_mul_t(np_smp, sph_rj%istack_rj_j_smp,           &
     &    band_p_poisson, d_rj(1,is_press))
!
      if(i_debug*j .gt. 0) then
        write(id_file,*) 'Solution of average pressure'
        call check_scalar_coefs(id_file, izero, izero, sph_rj,          &
     &      is_press, n_point, ntot_phys_rj, d_rj)
        close(id_file)
      end if
!
      end subroutine solve_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine solve_magne_sph_crank                                  &
     &         (sph_rj, band_bp_evo, band_bt_evo, is_magne, it_magne,   &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(in) :: band_bp_evo
      type(band_matrices_type), intent(in) :: band_bt_evo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne, it_magne
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      call lubksb_3band_mul_t(np_smp, sph_rj%istack_rj_j_smp,           &
     &    band_bp_evo, d_rj(1,is_magne) )
!
      call lubksb_3band_mul_t(np_smp, sph_rj%istack_rj_j_smp,           &
     &    band_bt_evo, d_rj(1,it_magne) )
!
      end subroutine solve_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine solve_scalar_sph_crank                                 &
     &         (sph_rj, band_s_evo, band_s00_evo, is_field,             &
     &          n_point, ntot_phys_rj, d_rj, sol_00)
!
      use t_sph_center_matrix
      use cal_sph_exp_center
      use check_sph_radial_mat
      use fill_scalar_field
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(in) :: band_s_evo
      type(band_matrix_type), intent(in) :: band_s00_evo
!
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(inout) :: sol_00(0:sph_rj%nidx_rj(1))
!
!      integer(kind = kint) :: j
!
!
      if(sph_rj%inod_rj_center .gt. 0) then
        call copy_degree0_comps_to_sol                                  &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_field, n_point, ntot_phys_rj, d_rj, sol_00)
      end if
!
!      j = find_local_sph_address(sph_rj, 0,0)
!      if(j.gt.0) then
!        write(*,*) 'matrix'
!        call check_single_radial_3band_mat(my_rank, sph_rj%nidx_rj(1), &
!     &      sph_rj%radius_1d_rj_r, band_s_evo%mat(1,1,j))
!      end if
!
      call lubksb_3band_mul_t(np_smp, sph_rj%istack_rj_j_smp,           &
     &    band_s_evo, d_rj(1,is_field))
!
!       write(6,*) 'solution'
!       call check_scalar_coefs                                         &
!     &    (6, 30,-23, sph_rj, is_field, ntot_phys_rj, d_rj)
!       write(*,*) 'check_NaN_temperature'
!       call check_NaN_temperature                                      &
!     &    (is_field, sph_rj, n_point, ntot_phys_rj, d_rj)
!
!   Solve l=m=0 including center
      if(sph_rj%inod_rj_center .eq. 0) return
!
!      write(50+my_rank,*) 'matrix for l=m=0'
!      call check_single_radial_3band_mat(my_rank, sph_rj%nidx_rj(1),   &
!     &    sph_rj%radius_1d_rj_r, band_s00_evo%mat)
!
      call lubksb_3band_ctr(band_s00_evo, sol_00)
      call copy_degree0_comps_from_sol                                  &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero, sol_00,     &
     &    is_field, n_point, ntot_phys_rj, d_rj)
!
      end subroutine solve_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_scalar_coefs(id_file, l, m, sph_rj,              &
     &          is_field, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: l, m, is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: j,k,inod
      integer :: l4, m4
!
!
      l4 = int(l)
      m4 = int(m)
      j = find_local_sph_address(sph_rj, l4, m4)
      if(j .eq. 0) return
!
      write(id_file,*) 'field ID, l, m: ', is_field, l, m
      do k = 1, sph_rj%nidx_rj(1)
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        write(id_file,*) k, d_rj(inod,is_field)
      end do
!
      end subroutine check_scalar_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_NaN_temperature                                  &
     &         (is_field, sph_rj, n_point, ntot_phys_rj, d_rj)
!
      use spherical_harmonics
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do inod = 1, n_point
        if(d_rj(inod,is_field) .ne. d_rj(inod,is_field)) then
          j = sph_rj%idx_global_rj(inod,2)
          k = sph_rj%idx_global_rj(inod,1)
          call get_degree_order_by_full_j(j, l, m)
          write(50+my_rank,*) 'Broken', inod, k, j, l, m,  &
     &              d_rj(inod,is_field)
        end if
      end do
!
      end subroutine check_NaN_temperature
!
! -----------------------------------------------------------------------
!
      end module solve_sph_fluid_crank
